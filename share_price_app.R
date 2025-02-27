library(shiny)
library(dplyr)
library(tidyr)

# Source the external function
source("moex_share_prices.R")  # Loads `get_recent_stock_prices`

# Default tickers list
default_tickers <- c("SBER", "SBERP", "T", "MOEX", "VTBR", "CBOM", "QIWI",
                     "BSPB", "SFIN", "SPBE", "RENI", "FTRE", "PSBR", "AFKS",
                     "VZRZ", "MMBM", "ROSB", "LEAS", "MBNK", "ZAYM", "SVCB")

# Define UI
ui <- fluidPage(
  
  # Add custom CSS for layout styling
  tags$head(
    tags$style(HTML("
      .shiny-output-error { visibility: hidden; } /* Hide error messages */
      .shiny-output-error:before { visibility: hidden; }

      .container {
        display: flex;
        align-items: center;
        justify-content: start;
        gap: 20px; /* Space between button and text output */
      }

      .table-container {
        width: auto;
        text-align: left;
      }

      table {
        width: 100%;
        max-width: 900px; /* Adjusted for additional columns */
        border-collapse: collapse;
        margin-top: 10px;
      }
      
      th, td {
        border: 1px solid #ddd;
        padding: 8px;
        text-align: left;
        font-size: 14px;
      }

      th {
        background-color: #f2f2f2;
      }

      input[type='text'] {
        font-size: 14px;
        text-align: center;
      }
      
      #update {
        padding: 8px 15px;
        font-size: 16px;
      }

      #priceTextOutput {
        width: 400px; /* Ensures the text box is readable */
        font-size: 14px;
        text-align: left;
      }
    "))
  ),
  
  titlePanel("MOEX Stock Price Checker"),
  
  div(class = "container",
      actionButton("update", "Get Prices"),  # Button aligned to the left
      textInput("priceTextOutput", label = NULL, value = "", width = "400px")  # Standard text box for tab-separated prices
  ),
  
  div(class = "table-container", tableOutput("priceTable")) # Table aligned to the left
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive storage for tickers, prices, and times
  stock_data <- reactiveVal(
    tibble(
      Ticker = default_tickers,
      Price = NA_real_,
      Time = NA_character_,
      ShortName = NA_character_,
      SecName = NA_character_,
    )
  )
  
  observeEvent(input$update, {
    isolate({
      # Read tickers dynamically from UI
      tickers <- sapply(default_tickers, function(t) input[[paste0("ticker_", t)]], USE.NAMES = TRUE)
      
      # Fetch stock prices using the external function
      stock_info <- get_recent_stock_prices(tickers)
      
      # Ensure `ticker` column exists in `stock_info`
      if (!"ticker" %in% colnames(stock_info)) {
        stock_info <- tibble(ticker = tickers, price = NA_real_, time = NA_character_,
                             shortname = NA_character_, secname = NA_character_)
      }
      
      # Rename `ticker` to `Ticker` for consistency
      stock_info <- stock_info %>%
        rename(Ticker = ticker, Price = price, Time = time, ShortName = shortname, SecName = secname)
      
      # Merge with default tickers to ensure all tickers remain
      stock_info <- full_join(tibble(Ticker = tickers), stock_info, by = "Ticker") %>%
        mutate(across(everything(), ~ replace_na(.x, NA)))  # Replace missing values with NA
      
      # Update the table with tickers, prices, times, shortnames, and secnames
      stock_data(stock_info)
      
      # Replace NA values with "#Н/Д" in the tab-separated price output
      formatted_prices <- ifelse(is.na(stock_info$Price), "#Н/Д", stock_info$Price)
      
      # Update text input with tab-separated prices
      updateTextInput(session, "priceTextOutput", value = paste(formatted_prices, collapse = "\t"))
      
    })
  })
  
  output$priceTable <- renderTable({
    df <- stock_data()
    
    # Replace the first column's values with text inputs for ticker modification
    df$Ticker <- sapply(df$Ticker, function(t) as.character(textInput(paste0("ticker_", t), label = NULL, value = t)))
    
    df
  }, sanitize.text.function = function(x) x, rownames = FALSE)
}

# Run the app
shinyApp(ui = ui, server = server)
