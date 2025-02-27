library(httr)
library(xml2)
library(dplyr)
library(lubridate)
library(tibble)
library(purrr)
library(tidyr)

# Function to get historical and current RUONIA rate from the CBR web service

get_ruonia <- function(start_date = NULL, end_date = Sys.Date()) {
  
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
  all_dates <- tibble(date = seq(start_date, end_date, by = "days"))
  
  # Convert dates to the required SOAP format (YYYY-MM-DD)
  start_date <- format(start_date, "%Y-%m-%d")
  end_date <- format(end_date, "%Y-%m-%d")
  
  # Construct SOAP request body for RUONIA
  soap_body <- paste0(
    '<?xml version="1.0" encoding="utf-8"?>',
    '<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
    ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"',
    ' xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">',
    '<soap:Body>',
    '<Ruonia xmlns="http://web.cbr.ru/">',
    '<fromDate>', start_date, '</fromDate>',
    '<ToDate>', end_date, '</ToDate>',
    '</Ruonia>',
    '</soap:Body>',
    '</soap:Envelope>'
  )
  
  # Send SOAP request with error handling
  response <- tryCatch(
    POST(
      url = "https://www.cbr.ru/DailyInfoWebServ/DailyInfo.asmx",
      body = soap_body,
      content_type("text/xml; charset=utf-8"),
      add_headers(SOAPAction = "http://web.cbr.ru/Ruonia")
    ),
    error = function(e) return(NULL)
  )
  
  # Check if response is valid
  if (is.null(response) || status_code(response) != 200) {
    return(all_dates %>% mutate(rate = NA_real_))
  }
  
  # Parse XML response with error handling
  xml_content <- tryCatch(
    content(response, as = "text", encoding = "UTF-8"),
    error = function(e) return(NULL)
  )
  
  if (is.null(xml_content)) {
    return(all_dates %>% mutate(rate = NA_real_))
  }
  
  xml_parsed <- tryCatch(
    read_xml(xml_content),
    error = function(e) return(NULL)
  )
  
  if (is.null(xml_parsed)) {
    return(all_dates %>% mutate(rate = NA_real_))
  }
  
  # Extract <ro> nodes inside <Ruonia>
  ruonia_nodes <- xml_find_all(xml_parsed, ".//diffgr:diffgram/Ruonia/ro")
  
  # Check if data exists
  if (length(ruonia_nodes) == 0) {
    return(all_dates %>% mutate(value = NA_real_))
  }
  
  # Extract date and rate only
  ruonia_df <- tibble(
    date = map_chr(ruonia_nodes, ~ xml_text(xml_find_first(.x, "D0"))),
    value = map_dbl(ruonia_nodes, ~ as.numeric(xml_text(xml_find_first(.x, "ruo"))))
  ) %>%
    mutate(date = as.Date(date))
  
  # Merge with all requested dates and fill missing values with NA
  full_ruonia_df <- all_dates %>%
    left_join(ruonia_df, by = "date")
  
  return(full_ruonia_df)
}

# Example usage
# get_ruonia("2025-02-19", "2025-02-25")
# ruonia_data <- get_ruonia()
# print(ruonia_data)


# Function to get historical and current key rate from the CBR web service

get_key_rate <- function(start_date = NULL, end_date = Sys.Date()) {
  
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
  all_dates <- tibble(date = seq(start_date, end_date, by = "days"))
  
  # Convert dates to the required SOAP format (YYYY-MM-DD)
  start_date <- format(start_date, "%Y-%m-%d")
  end_date <- format(end_date, "%Y-%m-%d")
  
  # Construct SOAP request body for Key Rate
  soap_body <- paste0(
    '<?xml version="1.0" encoding="utf-8"?>',
    '<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
    ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"',
    ' xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">',
    '<soap:Body>',
    '<KeyRateXML xmlns="http://web.cbr.ru/">',
    '<fromDate>', start_date, '</fromDate>',
    '<ToDate>', end_date, '</ToDate>',
    '</KeyRateXML>',
    '</soap:Body>',
    '</soap:Envelope>'
  )
  
  # Send SOAP request with error handling
  response <- tryCatch(
    POST(
      url = "https://www.cbr.ru/DailyInfoWebServ/DailyInfo.asmx",
      body = soap_body,
      content_type("text/xml; charset=utf-8"),
      add_headers(SOAPAction = "http://web.cbr.ru/KeyRateXML")
    ),
    error = function(e) return(NULL)
  )
  
  # Check if response is valid
  if (is.null(response) || status_code(response) != 200) {
    return(all_dates %>% mutate(rate = NA_real_))
  }
  
  # Parse XML response with error handling
  xml_content <- tryCatch(
    content(response, as = "text", encoding = "UTF-8"),
    error = function(e) return(NULL)
  )
  
  if (is.null(xml_content)) {
    return(all_dates %>% mutate(rate = NA_real_))
  }
  
  xml_parsed <- tryCatch(
    read_xml(xml_content),
    error = function(e) return(NULL)
  )
  
  if (is.null(xml_parsed)) {
    return(all_dates %>% mutate(rate = NA_real_))
  }
  
  # Extract <KR> nodes inside <KeyRate>
  keyrate_nodes <- xml_find_all(xml_parsed, ".//KeyRate/KR")
  
  # Check if data exists
  if (length(keyrate_nodes) == 0) {
    return(all_dates %>% mutate(rate = NA_real_))
  }
  
  # Extract date and rate only
  keyrate_df <- tibble(
    date = map_chr(keyrate_nodes, ~ xml_text(xml_find_first(.x, "DT"))),
    value = map_dbl(keyrate_nodes, ~ as.numeric(xml_text(xml_find_first(.x, "Rate"))))
  ) %>%
    mutate(date = as.Date(date))
  
  # Merge with all requested dates and fill missing values with NA
  full_keyrate_df <- all_dates %>%
    left_join(keyrate_df, by = "date")
  
  return(full_keyrate_df)
}

# Example usage
# get_key_rate("2025-02-19", "2025-02-25")
# get_key_rate()
# key_rate_data_custom <- get_key_rate("2024-01-01", "2024-02-10")  # Custom range
# print(key_rate_data_custom)


# Function to get historical and current MIACR rate from the CBR web service

get_miacr <- function(start_date = NULL, end_date = Sys.Date(), duration = 1) {
  
  # Ensure duration is treated as a vector
  if (!is.vector(duration)) {
    duration <- c(duration)
  }
  
  # Validate duration input
  valid_durations <- c(1, 7, 30, 90, 180, 360)
  if (!all(duration %in% valid_durations)) {
    stop("Invalid duration. Choose from 1, 7, 30, 90, 180, or 360.")
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
  all_dates <- tibble(date = seq(start_date, end_date, by = "days"))
  
  # Convert dates to the required SOAP format (YYYY-MM-DD)
  start_date <- format(start_date, "%Y-%m-%d")
  end_date <- format(end_date, "%Y-%m-%d")
  
  # Map durations to correct column names
  duration_map <- c("1" = "d1", "7" = "d7", "30" = "d30", "90" = "d90", "180" = "d180", "360" = "d360")
  selected_cols <- duration_map[as.character(duration)]
  
  # Construct SOAP request body for MIACR
  soap_body <- paste0(
    '<?xml version="1.0" encoding="utf-8"?>',
    '<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
    ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"',
    ' xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">',
    '<soap:Body>',
    '<MKR xmlns="http://web.cbr.ru/">',
    '<fromDate>', start_date, '</fromDate>',
    '<ToDate>', end_date, '</ToDate>',
    '</MKR>',
    '</soap:Body>',
    '</soap:Envelope>'
  )
  
  # Send SOAP request with error handling
  response <- tryCatch(
    POST(
      url = "https://www.cbr.ru/DailyInfoWebServ/DailyInfo.asmx",
      body = soap_body,
      content_type("text/xml; charset=utf-8"),
      add_headers(SOAPAction = "http://web.cbr.ru/MKR")
    ),
    error = function(e) return(NULL)
  )
  
  # Check if response is valid
  if (is.null(response) || status_code(response) != 200) {
    return(all_dates %>%
             expand_grid(duration = duration) %>%
             mutate(rate = NA_real_))
  }
  
  # Parse XML response with error handling
  xml_content <- tryCatch(
    content(response, as = "text", encoding = "UTF-8"),
    error = function(e) return(NULL)
  )
  
  if (is.null(xml_content)) {
    return(all_dates %>%
             expand_grid(duration = duration) %>%
             mutate(rate = NA_real_))
  }
  
  xml_parsed <- tryCatch(
    read_xml(xml_content),
    error = function(e) return(NULL)
  )
  
  if (is.null(xml_parsed)) {
    return(all_dates %>%
             expand_grid(duration = duration) %>%
             mutate(rate = NA_real_))
  }
  
  # Extract <MKR> nodes
  miacr_nodes <- xml_find_all(xml_parsed, ".//MKR")
  
  # If no MIACR data is found, return a tibble with NA values
  if (length(miacr_nodes) == 0) {
    return(all_dates %>%
             expand_grid(duration = duration) %>%
             mutate(rate = NA_real_))
  }
  
  # Filter only MIACR (p1 == 3) nodes before extracting data
  miacr_filtered_nodes <- miacr_nodes[
    map_chr(miacr_nodes, ~ xml_text(xml_find_first(.x, "p1"))) == "3"
  ]
  
  # If no MIACR nodes after filtering, return a tibble with NAs
  if (length(miacr_filtered_nodes) == 0) {
    return(all_dates %>%
             expand_grid(duration = duration) %>%
             mutate(rate = NA_real_))
  }
  
  # Extract base data (only for MIACR RUB)
  miacr_df <- tibble(
    date = map_chr(miacr_filtered_nodes, ~ xml_text(xml_find_first(.x, "CDate")))
  ) %>%
    mutate(date = as.Date(date))
  
  # Extract rates for each requested duration
  for (dur in duration) {
    col_name <- duration_map[as.character(dur)]
    
    # Extract values **only from MIACR-filtered nodes**
    extracted_values <- map_chr(miacr_filtered_nodes, ~ xml_text(xml_find_first(.x, col_name)))
    
    # Convert to numeric and ensure the length matches the number of unique dates
    miacr_df[[paste0("miacr_", dur)]] <- as.numeric(extracted_values)
  }
  
  # Convert from wide to long format
  long_miacr_df <- miacr_df %>%
    pivot_longer(cols = starts_with("miacr_"),
                 names_to = "duration",
                 names_prefix = "miacr_",
                 values_to = "value") %>%
    mutate(duration = as.numeric(duration)) # Convert duration to numeric
  
  # Merge with all requested dates and fill missing values with NA
  full_miacr_df <- all_dates %>%
    expand_grid(duration = duration) %>%
    left_join(long_miacr_df, by = c("date", "duration"))
  
  return(full_miacr_df)
}

# Example usage
# get_miacr("2025-02-19", "2025-02-25", duration = c(1, 7))
# miacr_data <- get_miacr()  # Fetch last 14 days (default: overnight MIACR)
# print(miacr_data)
# 
# miacr_data_custom <- get_miacr("2024-01-01", "2024-02-10", duration = 7)  # Fetch MIACR for 7-day duration
# print(miacr_data_custom)

