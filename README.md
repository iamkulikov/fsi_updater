---
editor_options: 
  markdown: 
    wrap: 72
---

# Financial Data Fetcher for Russian Markets

## Overview

This project provides a set of R scripts for retrieving financial data
related to the Russian economy, including exchange rates, bond indices,
stock prices, oil prices, and money market rates. The scripts fetch data
from various sources such as the Moscow Exchange (MOEX), the Central
Bank of Russia (CBR), and Cbonds.

## Project Structure

```         
.
├── oil.R                # Fetches Urals and Brent crude oil prices
├── cbonds_indices.R     # Retrieves bond indices and commodity prices from Cbonds
├── moex_share_prices.R  # Fetches MOEX share prices (recent and historical)
├── moex_indices.R       # Retrieves MOEX stock indices (historical and current)
├── fx.R                 # Fetches foreign exchange rates from MOEX and CBR
├── zcyc.R               # Computes and fetches zero-coupon yield curve (ZCYC) data from MOEX
├── cbr_mmarket.R        # Retrieves money market rates (RUONIA, key rate, MIACR) from CBR
└── README.md            # Documentation
```

## Installation

To run these scripts, install the necessary R packages:

``` r
install.packages(c("httr", "jsonlite", "xml2", "dplyr", "tibble", "tidyr", "lubridate", "purrr", "readr", "hms", "here"))
```

## Functionality

### 1. Oil Prices (`oil.R`)

Fetches the latest Urals and Brent crude oil prices from a financial
market data feed.

\- **Functions:** `get_current_oil_prices()`

\- **Data Source:** Profinance SSE feed

### 2. Cbonds Indices (`cbonds_indices.R`)

Retrieves historical and current bond indices and commodity prices from
Cbonds.

\- **Functions:**
`get_cbonds_index(index_code, start_date, end_date, api_key)` ,
`get_ifx_cbonds()`, `get_urals_cbonds()` , `get_brent_cbonds()`, etc.

\- **Data Source:** Cbonds API (requires API key)

### 3. MOEX Share Prices (`moex_share_prices.R`)

Fetches recent and historical stock prices from MOEX.

\- **Functions:** `get_recent_stock_prices(tickers)`,
`get_historic_stock_prices(tickers, start_date, end_date)`

\- **Data Source:** MOEX ISS API

### 4. MOEX Indices (`moex_indices.R`)

Retrieves historical and current MOEX stock indices.

\- **Functions:**
`get_hist_index_data(index_codes, start_date, end_date)` ,
`get_current_index_data(index_codes)`

\- **Data Source:** MOEX ISS API

### 5. Foreign Exchange Rates (`fx.R`)

Fetches exchange rates from the Central Bank of Russia and MOEX.

\- **Functions:**
`get_cbrfx_rates(currency_names, start_date, end_date)`,
`get_hist_fx_rates(currencies, start_date, end_date)` ,
`get_current_fx_rates(currencies)`

\- **Data Sources:** CBR API, MOEX API

### 6. Zero-Coupon Yield Curve (`zcyc.R`)

Computes the zero-coupon yield curve (ZCYC) using the Nelson-Siegel
model.

\- **Functions:** `get_current_gyield(duration_years)` ,
`get_hist_gyields(start_date, end_date, duration_years, online)`

\- **Data Source:** MOEX ZCYC API

### 7. Money Market Rates (`cbr_mmarket.R`)

Retrieves RUONIA, the key rate, and MIACR rates from the Central Bank of
Russia.

\- **Functions:** `get_ruonia(start_date, end_date)` ,
`get_key_rate(start_date, end_date)`,
`get_miacr(start_date, end_date, duration)`

\- **Data Source:** CBR SOAP API

## Usage

Each script can be sourced in R, and functions can be called as needed.
Example usage:

``` r
source("moex_share_prices.R")
stocks <- get_recent_stock_prices(c("SBER", "GAZP"))
print(stocks)
```

## License

This project is open-source and provided under the MIT License.
