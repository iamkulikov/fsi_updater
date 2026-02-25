# Load required libraries
library(httr)
library(dplyr)
library(tidyr)
library(tibble)
library(fredr)
library(purrr)

# Function to fetch the latest Urals and Brent crude oil bid prices from the SSE feed

get_current_oil_prices <- function() {
  
  # Define the URL of the SSE feed providing oil prices
  url <- "https://jq.profinance.ru/html/htmlquotes/qsse?msg=1;SID=gV00jevN;T=1741616757672"
  
  # Fetch the data from the SSE feed with headers
  response <- tryCatch(GET(url, add_headers(
    'User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36'
  ), timeout(6)), error = function(e) return(NULL))
  
  # Check if the request was successful
  if (is.null(response) || status_code(response) != 200) {
    return(tibble(oil = c("urals", "brent"), price = NA_real_, date = Sys.Date(), time = NA_character_))
  }
  
  # Extract raw text from the response
  content_text <- content(response, as = "text", encoding = "UTF-8")
  
  # Split the text by newline to process each line separately
  lines <- unlist(strsplit(content_text, "\n"))
  
  # Extract the latest relevant lines for Urals and Brent (based on latest timestamp)
  urals_lines <- lines[grepl("S=Urals Med", lines)]
  brent_lines <- lines[grepl("S=Brent oil", lines)]
  
  if (length(urals_lines) == 0 || length(brent_lines) == 0) {
    return(tibble(oil = c("urals", "brent"), price = NA_real_, date = Sys.Date(), time = NA_character_))
  }
  
  urals_line <- tail(urals_lines, 1)  # Get the most recent entry
  brent_line <- tail(brent_lines, 1)  # Get the most recent entry
  
  # Extract bid price (B) and time (T) using regex
  extract_value <- function(line, key) {
    match <- regmatches(line, regexpr(paste0(key, "=[^;]+"), line))
    if (length(match) > 0) {
      return(sub(paste0(key, "="), "", match))
    } else {
      return(NA)
    }
  }
  
  urals_price <- as.numeric(gsub("[^0-9.]+", "", extract_value(urals_line, "B")))
  brent_price <- as.numeric(gsub("[^0-9.]+", "", extract_value(brent_line, "B")))
  time_value <- extract_value(brent_line, "T")
  
  # Validate extracted values
  if (is.na(urals_price) || is.na(brent_price) || is.null(time_value) || time_value == "") {
    return(tibble(oil = c("urals", "brent"), price = NA_real_, date = Sys.Date(), time = NA_character_))
  }
  
  # Create a tibble with the extracted data in long format
  oil_prices <- tibble(
    oil = c("urals", "brent"),
    price = c(urals_price, brent_price),
    date = Sys.Date(),
    time = time_value
  )
  
  return(oil_prices)
}

# Example usage
# oil_prices <- get_current_oil_prices()
# print(oil_prices)


# Function to get the historic oil prices from the FRED API (2 days lag)

get_hist_oil_prices <- function(
    oil_names  = c("brent", "wti"),
    start_date = NULL,
    end_date   = Sys.Date(),
    key_file   = "fred_key.txt"
) {
  
  # read API key from the disk
  key <- tryCatch(
    {
      lines <- readLines(key_file, warn = FALSE)
      trimws(lines[1])
    },
    error = function(e) NA_character_
  )
  if (is.na(key) || nchar(key) == 0) {
    stop("Could not read FRED API key from '", key_file, "'.",
         " Please create that file and put your key on the first line.")
  }
  fredr_set_key(key)
  
  # map user‑friendly names → FRED series IDs
  lookup <- tibble(
    oil        = c("brent", "urals", "wti"),
    series_id  = c("DCOILBRENTEU", "DCOILRUSM", "DCOILWTICO")
  )
  
  # validate requested oils
  oil_names <- unique(oil_names)
  invalid   <- setdiff(oil_names, lookup$oil)
  if (length(invalid) > 0) {
    stop("Unknown oil name(s): ", paste(invalid, collapse = ", "))
  }
  sel <- lookup %>% filter(oil %in% oil_names)
  
  # parse end_date
  end_date <- tryCatch(as.Date(end_date), error = function(e) NA)
  if (is.na(end_date)) stop("Invalid end_date format. Use 'YYYY-MM-DD'.")
  
  # default start_date = end_date − 14 days
  if (is.null(start_date)) {
    start_date <- end_date - 14
  } else {
    start_date <- tryCatch(as.Date(start_date), error = function(e) NA)
  }
  if (is.na(start_date)) stop("Invalid start_date format. Use 'YYYY-MM-DD'.")
  if (start_date > end_date) stop("start_date must be ≤ end_date.")
  
  # fetch each series via fredr
  prices_list <- purrr::map2_dfr(
    sel$series_id, sel$oil,
    ~ {
      df <- tryCatch(
        fredr(
          series_id       = .x,
          observation_start = start_date,
          observation_end   = end_date
        ),
        error = function(e) NULL
      )
      if (is.null(df) || nrow(df) == 0) {
        return(tibble(date = seq(start_date, end_date, by = "days"),
                      oil   = .y,
                      price = NA_real_))
      }
      df %>%
        transmute(
          date  = as.Date(date),
          oil   = .y,
          price = value
        )
    }
  )
  
  # ensure full date × oil grid, filling missing with NA
  full_grid <- expand_grid(
    date = seq(start_date, end_date, by = "days"),
    oil  = oil_names
  )
  
  prices_list %>%
    right_join(full_grid, by = c("date", "oil")) %>%
    arrange(date, oil) %>%
    as_tibble()
}

# Example usage
# oil_prices <- get_hist_oil_prices()
# print(oil_prices)