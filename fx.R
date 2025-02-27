library(httr)
library(jsonlite)
library(xml2)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)

# Function to fetch currency code from CBR
# currency names stored here: https://www.cbr.ru/scripts/XML_val.asp?d=0

get_currency_code <- function(currency_name) {
  
  url <- "https://www.cbr.ru/scripts/XML_val.asp?d=0"
  response <- tryCatch(GET(url), error = function(e) return(NULL))
  
  if (is.null(response) || status_code(response) != 200) {
    stop("Failed to retrieve currency codes from CBR.")
  }
  
  xml_raw <- tryCatch(content(response, as = "raw"), error = function(e) return(NULL))
  if (is.null(xml_raw)) {
    stop("Failed to process currency data.")
  }
  
  xml_text_content <- tryCatch(iconv(rawToChar(xml_raw), from = "windows-1251", to = "UTF-8"), error = function(e) return(NULL))
  if (is.null(xml_text_content)) {
    stop("Encoding conversion failed.")
  }
  
  parsed_xml <- tryCatch(read_xml(xml_text_content), error = function(e) return(NULL))
  if (is.null(parsed_xml)) {
    stop("Failed to parse XML content.")
  }
  
  items <- xml_find_all(parsed_xml, ".//Item")
  
  codes <- tibble(
    name_ru = xml_text(xml_find_all(items, "Name")),
    name_en = xml_text(xml_find_all(items, "EngName")),
    code = xml_attr(items, "ID")
  )
  
  # Filter for requested currency names
  filtered_codes <- codes %>% filter(name_ru %in% currency_name | name_en %in% currency_name)
  
  if (nrow(filtered_codes) == 0) {
    stop("Currency name not found.")
  }
  
  return(filtered_codes$code)
}

# Function to fetch multiple currency official exchange rates from CBR

get_cbrfx_rates <- function(currency_names = c("US Dollar", "Euro", "Юань"), start_date = NULL, end_date = Sys.Date()) {
  
  currency_codes <- tryCatch(get_currency_code(currency_names), error = function(e) stop("Invalid currency name or issue retrieving currency code."))
  
  if (length(currency_codes) == 0) {
    stop("No valid currency names provided.")
  }
  
  # Convert end_date to Date format
  end_date <- tryCatch(as.Date(end_date), error = function(e) NA)
  if (is.na(end_date)) {
    stop("Invalid end_date format. Please use 'YYYY-MM-DD'.")
  }
  
  # Set start_date default (14 days before end_date)
  if (is.null(start_date)) {
    start_date <- end_date - 14
  } else {
    start_date <- tryCatch(as.Date(start_date), error = function(e) NA)
  }
  
  if (is.na(start_date)) {
    stop("Invalid start_date format. Please use 'YYYY-MM-DD'.")
  }
  
  if (start_date > end_date) {
    stop("Start date must be earlier than or equal to end date.")
  }
  
  # Generate full sequence of dates
  if (is.na(start_date) || is.na(end_date)) {
    stop("Both start_date and end_date must be valid dates.")
  }
  
  all_dates <- tibble(date = seq(start_date, end_date, by = "days"))
  
  # Convert dates to the required format (dd/mm/yyyy)
  start_date_str <- format(start_date, "%d/%m/%Y")
  end_date_str <- format(end_date, "%d/%m/%Y")
  
  rates_list <- map2(currency_codes, currency_names, function(code, name) {
    url <- paste0("https://www.cbr.ru/scripts/XML_dynamic.asp?date_req1=", 
                  start_date_str, "&date_req2=", end_date_str, "&VAL_NM_RQ=", code)
    
    response <- tryCatch(GET(url), error = function(e) return(NULL))
    if (is.null(response) || status_code(response) != 200) {
      return(NULL)
    }
    
    xml_content <- tryCatch(content(response, as = "text", encoding = "UTF-8"), error = function(e) return(NULL))
    if (is.null(xml_content)) {
      return(NULL)
    }
    
    parsed_xml <- tryCatch(read_xml(xml_content), error = function(e) return(NULL))
    if (is.null(parsed_xml)) {
      return(NULL)
    }
    
    records <- xml_find_all(parsed_xml, ".//Record")
    if (length(records) == 0) {
      return(NULL)
    }
    
    tibble(
      date = dmy(xml_attr(records, "Date")),
      currency = name,
      value = as.numeric(gsub(",", ".", xml_text(xml_find_all(records, "Value"))))
    )
  })
  
  rates_df <- bind_rows(rates_list)
  
  # Ensure all requested dates are present, filling missing values with NA
  full_dates <- expand_grid(date = seq(start_date, end_date, by = "days"), currency = currency_names)
  rates_df <- full_join(full_dates, rates_df, by = c("date", "currency")) %>% arrange(date, currency) %>% as_tibble()
  
  return(rates_df)
}


# Example usage
 # get_cbrfx_rates(c("US Dollar", "Euro", "Юань"), "2025-01-01", "2025-03-01")
 # get_cbrfx_rates()


# Function to fetch historic market exchange rates from MOEX API
## To-do: find a link to the currency code dictionary

get_hist_fx_rates <- function(currencies = c("USD", "EUR", "CNY"), start_date = NULL, end_date = Sys.Date()) {
  
  # Convert end_date to Date format
  end_date <- tryCatch(as.Date(end_date), error = function(e) NA)
  
  if (is.na(end_date)) {
    stop("Invalid end_date format. Please use 'YYYY-MM-DD'.")
  }
  
  # Set start_date default (14 days before end_date)
  if (is.null(start_date)) {
    start_date <- end_date - 14
  } else {
    start_date <- tryCatch(as.Date(start_date), error = function(e) NA)
  }
  
  if (is.na(start_date)) {
    stop("Invalid start_date format. Please use 'YYYY-MM-DD'.")
  }
  
  if (start_date > end_date) {
    stop("Start date must be earlier than or equal to end date.")
  }
  
  # Convert dates to required format (YYYY-MM-DD)
  start_date <- format(start_date, "%Y-%m-%d")
  end_date <- format(end_date, "%Y-%m-%d")
  
  # Create full sequence of dates for all requested currencies
  all_dates <- expand.grid(date = seq(as.Date(start_date), as.Date(end_date), by = "days"), currency = currencies)
  
  results <- list()
  
  for (currency in currencies) {
    
    # Construct API request URL
    api_url <- paste0(
      "http://iss.moex.com/iss/statistics/engines/futures/markets/indicativerates/securities/", 
      currency, "/RUB.json?from=", start_date, "&till=", end_date, "&iss.meta=off"
    )
    
    # Fetch data from API
    response <- tryCatch(GET(api_url), error = function(e) NULL)
    
    if (is.null(response) || status_code(response) != 200) {
      warning(paste("Failed to retrieve data for", currency))
      next
    }
    
    # Parse JSON content
    content <- content(response, as = "text", encoding = "UTF-8")
    data <- tryCatch(
      fromJSON(content, flatten = TRUE),
      error = function(e) {
        warning(paste("Failed to parse JSON response for", currency, ":", e$message))
        return(NULL)
      }
    )
    
    if (is.null(data) || length(data$securities$data) == 0) {
      warning(paste("No data available for", currency))
      next
    }
    
    # Extract relevant data
    df <- as.data.frame(data$securities$data)
    colnames(df) <- data$securities$columns
    
    # Select only 'pk' rates, or 'vk' if 'pk' is missing
    df_filtered <- df %>%
      group_by(tradedate) %>%
      summarise(value = as.numeric(if (any(clearing == "pk")) {
        rate[clearing == "pk"]
      } else if (any(clearing == "vk")) {
        rate[clearing == "vk"]
      } else {
        NA_real_
      })) %>%
      ungroup() %>% as_tibble()
    
    # Rename columns and ensure correct data types
    df_filtered <- df_filtered %>%
      rename(date = tradedate) %>%
      mutate(
        date = as.Date(date),
        currency = currency
      )
    
    results[[currency]] <- df_filtered
  }
  
  # Combine results into a single tibble in long format
  final_df <- bind_rows(results) %>%
    select(date, currency, value)
  
  # Ensure all dates and currencies are present, filling missing values with NA
  final_df <- full_join(all_dates, final_df, by = c("date", "currency"))
  
  return(final_df)
}

# Example usage
# get_hist_fx_rates(c("CNY", "KZT"))
# get_hist_fx_rates("KZT")


# Function to fetch current foreign exchange rates from MOEX API
get_current_fx_rates <- function(currencies = c("USD", "EUR", "CNY")) {
  
  results <- list()
  
  for (currency in currencies) {
    
    # Construct API request URL
    api_url <- paste0(
      "http://iss.moex.com/iss/statistics/engines/futures/markets/indicativerates/securities/", 
      currency, "/RUB.json?iss.meta=off"
    )
    
    # Fetch data from API
    response <- tryCatch(GET(api_url), error = function(e) NULL)
    
    if (is.null(response) || status_code(response) != 200) {
      warning(paste("Failed to retrieve data for", currency))
      next
    }
    
    # Parse JSON content
    content <- content(response, as = "text", encoding = "UTF-8")
    data <- tryCatch(
      fromJSON(content, flatten = TRUE),
      error = function(e) {
        warning(paste("Failed to parse JSON response for", currency, ":", e$message))
        return(NULL)
      }
    )
    
    if (is.null(data) || length(data$securities.current$data) == 0) {
      warning(paste("No current data available for", currency))
      next
    }
    
    # Extract relevant data
    df <- as.data.frame(data$securities.current$data)
    colnames(df) <- data$securities.current$columns
    
    # Rename columns and ensure correct data types
    df_filtered <- df %>%
      select(secid, tradedate, tradetime, rate) %>%
      rename(currency = secid, date = tradedate, time = tradetime, value = rate) %>%
      mutate(
        date = as.Date(date),
        time = as.character(time),
        value = as.numeric(value),
        currency = gsub("/RUB", "", currency)
      )
    
    results[[currency]] <- df_filtered
  }
  
  # Combine results into a single tibble in long format
  final_df <- bind_rows(results) %>%
    select(date, time, currency, value) %>% as_tibble()
  
  return(final_df)
}

# Example usage
# fx_rates <- get_current_fx_rates(c("CNY", "USD"))
# print(fx_rates)