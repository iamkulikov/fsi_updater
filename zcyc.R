library(httr)
library(jsonlite)
library(dplyr)
library(tibble)
library(readr)
library(tidyr)
library(hms)
library(here)
library(purrr)

here::i_am("zcyc.R")

# Define compute_yield as a standalone function

compute_yield <- function(beta_0, beta_1, beta_2, lambda, g, duration) {
  
  # Validate that duration is provided and is numeric
  if (missing(duration) || !is.numeric(duration)) {
    stop("Error: `duration` must be a numeric value.")
  }
  
  # Check if any required parameter (except duration) is NA
  if (any(is.na(c(beta_0, beta_1, beta_2, lambda, g)))) {
    return(NA_real_)
  }
  
  # Compute Nelson-Siegel formula
  term <- duration / lambda
  G_t <- beta_0 +
    (beta_1 + beta_2) * (1 - exp(-term)) / term -
    beta_2 * exp(-term)
  
  # Compute correction terms using Gaussian basis functions
  k <- 1.6
  a <- numeric(9)
  b <- numeric(9)
  
  # Compute a_i and b_i recursively
  a[1] <- 0
  a[2] <- 0.6
  b[1] <- a[2]
  
  for (i in 2:8) {
    a[i+1] <- a[i] + a[2] * k^(i-1)
    b[i+1] <- b[i] * k
  }
  
  # Apply Gaussian correction
  correction_term <- sum(g * exp(-((duration - a)^2) / (b^2)))
  G_t <- G_t + correction_term
  
  # Convert to spot yield Y(t)
  Y_t <- 100 * (exp(G_t / 10000) - 1)
  
  return(Y_t)
}


# Function to compute the latest yield using the Nelson-Siegel model with correction terms for multiple durations

get_current_gyield <- function(duration_years = c(1, 5, 10)) {
  
  # API URL
  api_url <- "https://iss.moex.com/iss/history/engines/stock/zcyc.json"
  
  # Fetch the data
  response <- GET(api_url)
  
  # Check for successful response
  if (status_code(response) != 200) {
    stop("Failed to retrieve data from the Moscow Exchange API.")
  }
  
  # Parse the JSON content
  content <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(content, flatten = TRUE)
  
  # Extract yield curve data
  zcyc_data <- data$params$data
  zcyc_columns <- data$params$columns
  zcyc_df <- as.data.frame(zcyc_data)
  colnames(zcyc_df) <- zcyc_columns
  
  # Get the last row (latest available data)
  latest_data <- tail(zcyc_df, 1)
  
  # Extract timestamp
  date <- as.Date(latest_data$tradedate)
  
  # If the date is not today, set time and yields to NA
  if (date != Sys.Date()) {
    trade_time <- NA_character_
    yield_values <- rep(NA_real_, length(duration_years))
  } else {
    trade_time <- as.character(latest_data$tradetime)
    
    # Extract Nelson-Siegel parameters
    beta_0 <- as.numeric(latest_data$b1)  # Long-term yield component
    beta_1 <- as.numeric(latest_data$b2)  # Short-term yield component
    beta_2 <- as.numeric(latest_data$b3)  # Medium-term yield component
    lambda <- as.numeric(latest_data$t1)  # Decay factor
    
    # Extract correction coefficients (g1, g2, ..., g9)
    g <- as.numeric(latest_data %>% select(starts_with("g")) %>% unlist())
    
    # Compute yields for all durations
    yield_values <- sapply(duration_years, function(d) 
      compute_yield(beta_0, beta_1, beta_2, lambda, g, d)
    )
  }
  
  # Convert output to long format
  results <- tibble(
    date = date,
    time = trade_time,
    duration = duration_years,
    yield = yield_values
  )
  
  return(results)
}

# Example usage: Get the yield for multiple durations

# yields <- get_current_gyield(c(1, 3, 5, 7, 10, 30))
# print(yields)


# Function to fetch historical yield curve parameters and compute yields for multiple durations

get_hist_gyields <- function(start_date = NULL, 
                             end_date = Sys.Date(), 
                             duration_years = c(1, 5, 10), 
                             online = TRUE, 
                             folder_path = here()) {
  
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
    stop("Error: start_date must be earlier than or equal to end_date.")
  }
  
  # Validate duration range
  if (any(duration_years < 1 / 365) | any(duration_years > 50)) {
    stop("Error: All duration_years must be between 1 day (1/365 years) and 50 years.")
  }
  
  # Define file paths
  zip_file_path <- file.path(folder_path, "dynamic.csv.zip")
  csv_file_path <- file.path(folder_path, "dynamic.csv")
  
  # Download and extract data if online mode is on
  if (online) {
    message("Loading data...")
    download_url <- "http://moex.com/iss/downloads/engines/stock/zcyc/dynamic.csv.zip"
    message("Downloading historical yield curve parameters...")
    download.file(download_url, destfile = zip_file_path, mode = "wb")
    
    # Unzip the file
    message("Extracting data...")
    unzip(zip_file_path, exdir = folder_path)
  }
  
  # Check if the file exists
  if (!file.exists(csv_file_path)) {
    stop("Error: The dynamic.csv file is missing! Run in online mode first or ensure the file is in the correct directory.")
  }
  
  # Read CSV file with the correct delimiter
  df <- read_delim(csv_file_path, delim = ";", skip = 2, col_names = TRUE, col_types = cols(.default = "c"))
  
  # Convert date and time columns
  df <- df %>%
    mutate(
      date = as.Date(tradedate, format = "%d.%m.%Y"),
      trade_time = as_hms(tradetime)  # Convert time to proper format
    )
  
  # Generate all possible date-duration combinations
  all_dates <- tibble(date = seq(start_date, end_date, by = "days"))
  all_combinations <- expand_grid(date = all_dates$date, duration = duration_years)
  
  # Filter data within the requested date range
  df_filtered <- df %>%
    filter(date >= start_date & date <= end_date)
  
  # Convert numeric columns
  df_filtered <- df_filtered %>%
    mutate(across(B1:G9, ~ as.numeric(gsub(",", ".", .x, fixed = TRUE))))  # Convert from European decimal format
  
  # Compute historical yields using external `compute_yield`
  results <- df_filtered %>%
    rowwise() %>%
    mutate(
      yields_list = list(
        purrr::map_dbl(duration_years, ~ compute_yield(
          beta_0 = B1,
          beta_1 = B2,
          beta_2 = B3,
          lambda = T1,
          g = c(G1, G2, G3, G4, G5, G6, G7, G8, G9),
          duration = .x
        ))
      )
    ) %>%
    ungroup() %>%
    tidyr::unnest_longer(yields_list) %>%
    mutate(duration = rep(duration_years, times = n() / length(duration_years))) %>%
    rename(yield = yields_list) %>%
    mutate(time = trade_time) %>%
    select(date, time, duration, yield)
  
  # Merge with all combinations of date and duration to ensure every pair exists
  results <- full_join(all_combinations, results, by = c("date", "duration")) %>%
    arrange(date, duration)  # Ensure chronological order
  
  return(results)
}


# Example usage: Get historical yields for multiple durations

# historical_yields <- get_hist_gyields("2025-02-23", "2025-02-26", c(0.25, 1, 3, 5), online = F)
# print(historical_yields)

# historical_yields <- get_hist_gyields()
# print(historical_yields)
