library(httr)
library(xml2)
library(dplyr)
library(tibble)
library(purrr)
library(jsonlite)

# Function to fetch stock data (Price, Time, Short Name, Security Name) 
# from MOEX API (vectorized for multiple tickers, only for BOARDID="TQBR")

get_recent_stock_prices <- function(tickers) {
  
  # Ensure tickers are in vector format
  tickers <- as.character(tickers)
  
  # Get today's date
  today_date <- Sys.Date()
  
  # Iterate over tickers and fetch data
  results <- map_dfr(tickers, function(ticker) {
    url <- paste0("https://iss.moex.com/iss/engines/stock/markets/shares/securities/", ticker, ".xml")
    response <- tryCatch(GET(url), error = function(e) return(NULL))
    
    if (is.null(response) || http_error(response)) {
      message("Failed to retrieve data from the MOEX API for ticker: ", ticker)
      return(tibble(ticker = ticker, price = NA, time = NA, shortname = NA, secname = NA))
    }
    
    content <- tryCatch(content(response, as = "text", encoding = "UTF-8"), error = function(e) return(NULL))
    if (is.null(content)) {
      message("Failed to parse response content for ticker: ", ticker)
      return(tibble(ticker = ticker, price = NA, time = NA, shortname = NA, secname = NA))
    }
    
    doc <- tryCatch(read_xml(content), error = function(e) return(NULL))
    if (is.null(doc)) {
      message("Failed to parse XML document for ticker: ", ticker)
      return(tibble(ticker = ticker, price = NA, time = NA, shortname = NA, secname = NA))
    }
    
    # Extract required fields
    rows_marketdata <- xml_find_all(doc, "//data[@id='marketdata']/rows/row")
    rows_securities <- xml_find_all(doc, "//data[@id='securities']/rows/row")
    
    # Handle invalid ticker with empty rows
    if (length(rows_marketdata) == 0 && length(rows_securities) == 0) {
      message("Invalid ticker: ", ticker, ". No data available.")
      return(tibble(ticker = ticker, price = NA, time = NA, shortname = NA, secname = NA))
    }
    
    # Extract price, time, and system date for the "TQBR" board
    market_data <- rows_marketdata %>%
      map_df(~tibble(
        BOARDID = xml_attr(.x, "BOARDID"),
        price = as.numeric(xml_attr(.x, "LAST")),
        time = xml_attr(.x, "TIME"),
        date = xml_attr(.x, "SYSTIME") %>% as_datetime() %>% as_date()
      )) %>%
      filter(BOARDID == "TQBR") %>%
      slice(1)
    
    # Extract security names
    security_data <- rows_securities %>%
      map_df(~tibble(
        BOARDID = xml_attr(.x, "BOARDID"),
        shortname = xml_attr(.x, "SHORTNAME"),
        secname = xml_attr(.x, "SECNAME")
      )) %>%
      filter(BOARDID == "TQBR") %>%
      slice(1)
    
    # Check if data is from today, otherwise return NA
    if (nrow(market_data) == 0 || market_data$date != today_date) {
      return(tibble(ticker = ticker, price = NA, time = NA, shortname = shortname, secname = secname))
    }
    
    # Combine results into a tibble
    tibble(
      ticker = ticker,
      price = market_data$price %>% first() %>% coalesce(NA),
      time = market_data$time %>% first() %>% coalesce(NA),
      shortname = security_data$shortname %>% first() %>% coalesce(NA),
      secname = security_data$secname %>% first() %>% coalesce(NA)
    )
  })
  
  return(results)
}

# Example usage: 
# get_recent_stock_prices("SBER")
# get_recent_stock_prices(c("SBER", "SBERP", "T"))
# get_recent_stock_prices("SBERY")


# Function to fetch historic stock prices from MOEX and return a tibble with tickers as columns and dates as rows

get_historic_stock_prices <- function(tickers = c("SBER", "GAZP", "T"), start_date = NULL, end_date = Sys.Date()) {
  
  # Convert end_date to Date format
  end_date <- tryCatch(as.Date(end_date), error = function(e) NA)
  if (is.na(end_date)) {
    stop("Invalid end_date format. Please use 'YYYY-MM-DD'.")
  }
  
  # Set start_date default (4 days before end_date)
  if (is.null(start_date)) {
    start_date <- end_date - 4
  } else {
    start_date <- tryCatch(as.Date(start_date), error = function(e) NA)
  }
  
  if (is.na(start_date)) {
    stop("Invalid start_date format. Please use 'YYYY-MM-DD'.")
  }
  
  if (start_date > end_date) {
    stop("Start date must be earlier than or equal to end date.")
  }
  
  # Convert dates to required format
  start_date <- format(start_date, "%Y-%m-%d")
  end_date <- format(end_date, "%Y-%m-%d")
  
  tickers <- as.character(tickers)
  all_data <- list()
  
  for (ticker in tickers) {
    base_url <- paste0("https://iss.moex.com/iss/history/engines/stock/markets/shares/securities/", ticker, ".json")
    
    response <- tryCatch(GET(base_url, query = list(from = start_date, till = end_date)), error = function(e) return(NULL))
    if (is.null(response) || status_code(response) != 200) {
      message("Failed to fetch data for ticker: ", ticker)
      next
    }
    
    # Parse the response
    data_json <- tryCatch(content(response, as = "text", encoding = "UTF-8"), error = function(e) return(NULL))
    if (is.null(data_json)) {
      message("Failed to parse data for ticker: ", ticker)
      next
    }
    data_list <- fromJSON(data_json)
    
    # Check if data exists
    if (is.null(data_list$history$data) || length(data_list$history$data) == 0) {
      message("No historical data found for ticker: ", ticker)
      next
    }
    
    # Convert JSON to dataframe
    df <- as.data.frame(data_list$history$data)
    if (!is.null(data_list$history$columns)) {
      colnames(df) <- data_list$history$columns
    }
    
    if ("TRADEDATE" %in% colnames(df) & "CLOSE" %in% colnames(df) & "BOARDID" %in% colnames(df)) {
      df <- df %>%
        filter(BOARDID == "TQBR") %>%
        select(TRADEDATE, CLOSE) %>%
        mutate(date = as.Date(TRADEDATE), price = as.numeric(CLOSE)) %>%
        select(date, price) %>%
        mutate(ticker = ticker)  # Добавляем тикер как отдельную колонку
      all_data[[ticker]] <- df
    }
  }
  
  # Объединяем все данные в один tibble
  if (length(all_data) == 0) {
    message("No data collected for the given tickers.")
    return(tibble())
  }
  
  result <- bind_rows(all_data)  # Собираем все тикеры в единый tibble
  
  # Создаем полный список всех комбинаций дат и тикеров
  all_dates <- tibble(date = seq(as.Date(start_date), as.Date(end_date), by = "days"))
  all_tickers <- tibble(ticker = tickers)
  full_combinations <- crossing(all_dates, all_tickers)
  
  # Объединяем с имеющимися данными, заполняя пропущенные цены NA
  final_result <- full_combinations %>%
    left_join(result, by = c("date", "ticker")) %>%
    arrange(date, ticker)
  
  return(final_result)
}

# Example usage
# get_historic_stock_prices(c("SBER", "SBERP", "GAZP"), start_date = "2025-02-01", end_date = "2025-12-31")
# get_historic_stock_prices()
