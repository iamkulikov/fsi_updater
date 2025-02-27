library(httr)      
library(jsonlite)  
library(dplyr)     
library(tibble) 
library(tidyr)   

# Function to fetch MOEX historic index data with pagination and date filtering
# Efficiently fetches only the necessary pages by starting from the latest available data
# Index codes are stored here: https://iss.moex.com/iss/statistics/engines/stock/markets/index/analytics


get_hist_index_data <- function(index_codes = "IMOEX", start_date = NULL, end_date = Sys.Date()) {
  
  # Validate end_date format
  end_date <- tryCatch(as.Date(end_date), error = function(e) NA)
  if (is.na(end_date)) {
    stop("Invalid end_date format. Please use 'YYYY-MM-DD'.")
  }
  
  # Set default start_date to 14 days before end_date if not provided
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
  
  # Create a tibble with all requested dates and indices
  date_seq <- expand_grid(date = seq(start_date, end_date, by = "day"), indicator = index_codes)
  
  # Initialize a list to store data for each index
  index_data_list <- list()
  
  for (index_code in index_codes) {
    
    # Construct the data link
    base_url <- paste0("https://iss.moex.com/iss/history/engines/stock/markets/index/securities/", index_code, ".json")
    
    # First request to determine total rows and page size
    response <- GET(base_url)
    if (status_code(response) != 200) {
      warning(paste("Failed to fetch data for index:", index_code))
      next
    }
    
    # Convert JSON response to list
    data_json <- content(response, as = "text", encoding = "UTF-8")
    data_list <- fromJSON(data_json)
    
    # Check if the response contains actual data
    if (is.null(data_list$history$data) || length(data_list$history$data) == 0) {
      warning(paste("No data available for the provided index code:", index_code))
      next
    }
    
    # Extract total row count and dynamic page size from API response
    total_rows <- data_list$history.cursor$data[1,2]  # Second value is TOTAL
    page_size <- data_list$history.cursor$data[1,3]  # Third value is PAGESIZE
    pages <- ceiling(total_rows / page_size)  # Calculate total pages
    
    all_data <- list()  # Initialize list to store all pages
    earliest_date <- Sys.Date()  # Track the earliest date found in the data
    
    # Loop through pages in reverse to minimize downloads
    for (i in seq((pages - 1) * page_size, 0, by = -page_size)) {
      url <- paste0(base_url, "?start=", i)
      response <- GET(url)
      
      if (status_code(response) != 200) next  # Skip if request fails
      
      # Parse JSON response
      data_json <- content(response, as = "text", encoding = "UTF-8")
      data_list <- fromJSON(data_json)
      
      # Convert JSON data to dataframe
      df <- as.data.frame(data_list$history$data)
      
      # Assign column names properly
      if (!is.null(data_list$history$columns)) {
        colnames(df) <- data_list$history$columns
      }
      
      # Select and rename relevant columns if they exist
      if ("TRADEDATE" %in% colnames(df) & "CLOSE" %in% colnames(df)) {
        df <- df %>%
          select(TRADEDATE, CLOSE) %>%
          rename(date = TRADEDATE, value = CLOSE) %>%
          mutate(
            date = as.Date(date),   # Convert date to Date type
            value = as.numeric(value),  # Ensure Close prices are numeric
            indicator = index_code,  # Add indicator column
            time = ifelse(!is.na(value), "18:00:00", NA_character_)  # Set time for non-NA values
          ) %>%
          arrange(date)  # Ensure data is sorted by date (ascending)
        
        # Update the earliest date found
        earliest_date <- min(df$date, na.rm = TRUE)
        
        # Append data to the list
        all_data[[length(all_data) + 1]] <- df
      }
      
      # Stop if the earliest date is still later than the start_date
      if (earliest_date <= start_date) {
        break
      }
    }
    
    # Combine all collected pages into one dataframe for the current index
    if (length(all_data) > 0) {
      index_data <- bind_rows(all_data) %>%
        filter(date >= start_date & date <= end_date) %>%
        arrange(date)
    } else {
      index_data <- tibble(date = seq(start_date, end_date, by = "day"),
                           indicator = index_code, value = NA_real_, time = NA_character_)
    }
    
    index_data_list[[index_code]] <- index_data
  }
  
  # Ensure base tibble if no data is found
  if (length(index_data_list) == 0) {
    full_df <- date_seq %>%
      mutate(value = NA_real_, time = NA_character_)
  } else {
    full_df <- bind_rows(index_data_list) %>%
      select(indicator, date, time, value)
  }
  
  # Merge with complete date/indicator sequence to fill missing values with NA
  full_df <- full_join(date_seq, full_df, by = c("indicator", "date")) %>%
    arrange(date, indicator)
  
  # Convert to tibble for better readability
  return(as_tibble(full_df))
}

# Example usage:
# moex_data <- get_hist_index_data("RVI")
# print(moex_data)
# 
# moex_data <- get_hist_index_data(c("IMOEX", "MOEXFN"), start_date = "2025-01-01", end_date = "2025-12-31")
# print(moex_data)
# 
# moex_data <- get_hist_index_data("MOEXFN", end_date = "2024-12-31")
# print(moex_data)

# debug
# base_url <- "https://iss.moex.com/iss/history/engines/stock/markets/index/securities/rgbi.json"
# url <- "https://iss.moex.com/iss/history/engines/stock/markets/index/securities/rgbi.json?start=100"



# Function to fetch current indices values from MOEX

get_current_index_data <- function(index_codes) {
  
  # API URL for current index values
  url <- "https://iss.moex.com/iss/engines/stock/markets/index/securities.json"
  
  # Fetch data from the API
  response <- GET(url)
  
  if (status_code(response) != 200) {
    stop("Failed to fetch current data from MOEX. Check API status.")
  }
  
  # Parse JSON response
  data_json <- content(response, as = "text", encoding = "UTF-8")
  data_list <- fromJSON(data_json)
  
  # Extract relevant data from "securities" and "marketdata" sections
  securities_data <- as.data.frame(data_list$securities$data)
  colnames(securities_data) <- data_list$securities$columns
  
  marketdata_data <- as.data.frame(data_list$marketdata$data)
  colnames(marketdata_data) <- data_list$marketdata$columns
  
  # Merge both datasets by SECID
  full_data <- merge(securities_data, marketdata_data, by = c("SECID", "BOARDID"), all = TRUE)
  
  # Process and filter data for today's indices
  today_data <- full_data %>%
    filter(SECID %in% index_codes) %>%
    select(SECID, CURRENTVALUE, TRADEDATE, UPDATETIME) %>%
    mutate(
      value = as.numeric(CURRENTVALUE),  # Ensure numeric values
      date = as.Date(TRADEDATE),         # Convert to Date format
      time = ifelse(!is.na(UPDATETIME), UPDATETIME, "18:00:00"),  # Default to 18:00:00 if missing
      indicator = SECID  # Rename SECID to indicator
    ) %>%
    filter(date == Sys.Date()) %>%  # Keep only today's data
    select(indicator, date, time, value) %>%
    arrange(indicator)  # Ensure sorting by index code
  
  # Identify missing indices (those not in today's data)
  missing_indices <- setdiff(index_codes, today_data$indicator)
  
  # Create NA-filled rows for missing indices
  missing_data <- tibble(
    indicator = missing_indices,
    date = Sys.Date(),
    time = NA_character_,
    value = NA_real_
  )
  
  # Combine available and missing data
  result <- bind_rows(today_data, missing_data) %>%
    arrange(indicator)  # Ensure sorting
  
  # Convert to tibble for better readability
  return(as_tibble(result))
}

# Example usage:
 # current_data <- get_current_index_data(c("IMOEX", "MOEXFN", "RGBI", "RVI"))
 # print(current_data)

