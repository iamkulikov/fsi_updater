# Load required libraries
library(httr)
library(dplyr)
library(tibble)

# Function to fetch the latest Urals and Brent crude oil bid prices from the SSE feed

get_current_oil_prices <- function() {
  
  # Define the URL of the SSE feed providing oil prices
  url <- "https://jq.profinance.ru/html/htmlquotes/qsse?msg=1;SID=jOkqGS60;T=1740581573793"
  
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
