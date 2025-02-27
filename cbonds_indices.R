library(httr)
library(jsonlite)
library(dplyr)
library(tibble)
library(lubridate)

# Function to fetch data from Cbonds for a given date range (for the recent history)

get_cbonds_index <- function(index_code, start_date = NULL, end_date = Sys.Date(), api_key = NULL) {
  
  # Validate and set date defaults
  end_date <- tryCatch(as.Date(end_date), error = function(e) NA)
  if (is.na(end_date)) {
    stop("Invalid end_date format. Please use 'YYYY-MM-DD'.")
  }
  
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
  
  # Generate sequence of dates in the required format
  date_seq <- seq(start_date, end_date, by = "day")
  formatted_dates <- format(date_seq, "%Y-%m-%d")
  
  # Function to fetch data for a single date
  fetch_index <- function(date) {
    url <- paste0("https://cbonds.ru/api/indexes/",index_code ,"/", date, "/getValue/")
    
    # Define headers
    headers <- add_headers(
      'User-Agent' = 'Mozilla/5.0 (Linux; Android 11; SAMSUNG SM-G973U) AppleWebKit/537.36 (KHTML, like Gecko) SamsungBrowser/14.2 Chrome/87.0.4280.141 Mobile Safari/537.36',
      'Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8'
    )
    
    # If API key is needed, add it
    if (!is.null(api_key)) {
      headers <- add_headers(
        `Authorization` = paste("Bearer", api_key)
      )
    }
    
    # Send request
    response <- tryCatch(GET(url, headers), error = function(e) return(NULL))
    
    # Handle response errors
    if (is.null(response) || status_code(response) == 403) {
      return(tibble(date = as.Date(date), value = NA))
    }
    
    if (status_code(response) != 200) {
      return(tibble(date = as.Date(date), value = NA))
    }
    
    # Parse JSON response
    content <- tryCatch(content(response, as = "text", encoding = "UTF-8"), error = function(e) return(NULL))
    if (is.null(content) || content == "[]") {  # Handle empty responses (e.g., holidays)
      return(tibble(date = as.Date(date), value = NA))
    }
    
    json_data <- tryCatch(fromJSON(content), error = function(e) return(NULL))
    if (is.null(json_data) || is.null(json_data$CbondsIndexValue.numeric)) {
      return(tibble(date = as.Date(date), value = NA))
    }
    
    return(tibble(date = as.Date(date), value = as.numeric(json_data$CbondsIndexValue.numeric)))
  }
  
  # Fetch data for all dates
  results <- bind_rows(lapply(formatted_dates, fetch_index))
  
  return(results)
}

# Example usage:
# get_cbonds_index(10, "2025-02-20", "2025-02-27")

# Function to fetch historic IFX-cbonds index values for a given date range
get_ifx_cbonds <- function(start_date = NULL, end_date = Sys.Date(), api_key = NULL) {
  get_cbonds_index(index_code = 10, start_date = start_date, end_date = end_date, api_key = api_key)
  }

# Function to fetch historic Urals oil price for a given date range
get_urals_cbonds <- function(start_date = NULL, end_date = Sys.Date(), api_key = NULL) {
  get_cbonds_index(index_code = 1594, start_date = start_date, end_date = end_date, api_key = api_key)
}

# Function to fetch historic Brent oil price for a given date range
get_brent_cbonds <- function(start_date = NULL, end_date = Sys.Date(), api_key = NULL) {
  get_cbonds_index(index_code = 624, start_date = start_date, end_date = end_date, api_key = api_key)
}

# Function to fetch historic WTI oil price for a given date range
get_wti_cbonds <- function(start_date = NULL, end_date = Sys.Date(), api_key = NULL) {
  get_cbonds_index(index_code = 26955, start_date = start_date, end_date = end_date, api_key = api_key)
}

# Function to fetch historic TTF gas price for a given date range
get_ttfgas_cbonds <- function(start_date = NULL, end_date = Sys.Date(), api_key = NULL) {
  get_cbonds_index(index_code = 71489, start_date = start_date, end_date = end_date, api_key = api_key)
}

# Function to fetch historic XXCY currency basis for a given date range
get_xxcy_cbonds <- function(start_date = NULL, end_date = Sys.Date(), api_key = NULL) {
  get_cbonds_index(index_code = 8055, start_date = start_date, end_date = end_date, api_key = api_key)
}

# Function to fetch historic RVI index for a given date range
get_rvi_cbonds <- function(start_date = NULL, end_date = Sys.Date(), api_key = NULL) {
  get_cbonds_index(index_code = 9699, start_date = start_date, end_date = end_date, api_key = api_key)
}
