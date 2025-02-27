library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)

# Источник данных
source("moex_share_prices.R")
source("oil.R")
source("zcyc.R")
source("moex_indices.R")
source("fx.R")
source("cbonds_indices.R")
source("cbr_mmarket.R")

# Default tickers list
default_tickers <- c("SBER", "SBERP", "T", "MOEX", "VTBR", "CBOM", "QIWI",
                     "BSPB", "SFIN", "SPBE", "RENI", "FTRE", "PSBR", "AFKS",
                     "VZRZ", "MMBM", "ROSB", "LEAS", "MBNK", "ZAYM", "SVCB")

# Default indicator list
indicators <- c(
  "Urals oil price", "Brent oil price", "Brent futures 1Y", "Natural gas (TTF, Netherlands)", 
  "MOEX", "MOEXFN", "USDRUB TOD", "USDRUB CBR", "EURRUB TOD", "EURRUB CBR", 
  "CNYRUB TOD", "CNYRUB CBR", "IFX Cbonds", "OFZZC 3m", "OFZZC 5y", 
  "RGBI", "Key rate", "MIACR 1d", "RUONIA ON", "MOSPRIME ON", 
  "MIACR 1w", "MOSPRIME 1w", "USDRUB XCCY", "RVI"
)

# UI
ui <- fluidPage(
  titlePanel("FSI components"),
  
  actionButton("get_data", "Get data"),
  
  fluidRow(
    column(6, textInput("ifx_ofzzc", label = "Previuos date bond yields:", value = "", width = "95%")),
    column(6, textInput("miacr_ruonia", label = "Previuos date money market rates:", value = "", width = "95%"))
  ),
  
  fluidRow(
    column(12, textInput("dataTextOutput", label = "Current date:", value = "", width = "98%"))
  ),
  
  tableOutput("data_table")
)

# Server
server <- function(input, output, session) {
  
  dates <- as.character(seq(Sys.Date() - 3, Sys.Date(), by = "day"))
  
  # Изначально создаем пустую таблицу
  initial_data <- tibble(Indicator = indicators)
  initial_data[dates] <- NA
  initial_data["Time"] <- NA
  
  data_values <- reactiveVal(initial_data)
  
  observeEvent(input$get_data, {
    
    update_data <- data_values()
    
    # Заполняем существующие ячейки значениями "loading"
    update_data <- update_data %>% mutate(across(all_of(dates), ~ "loading"))
    data_values(update_data)
    
    # Оптимизированные запросы к API
    oil_prices <- get_current_oil_prices() %>% mutate(date_chr = as.character(date))
    fx_hist_rates <- get_hist_fx_rates(c("USD", "EUR", "CNY"), min(dates), max(dates)) %>% mutate(date_chr = as.character(date))
    fx_current_rates <- get_current_fx_rates(c("USD", "EUR", "CNY")) %>% mutate(date_chr = as.character(date))
    cbr_fx_rates <- get_cbrfx_rates(c("US Dollar", "Euro", "Юань"), min(dates), max(dates)) %>% mutate(date_chr = as.character(date))
    moex_hist_index_data <- get_hist_index_data(c("IMOEX", "MOEXFN", "RGBI", "RVI"), min(dates), max(dates)) %>% mutate(date_chr = as.character(date))
    moex_current_index_data <- get_current_index_data(c("IMOEX", "MOEXFN", "RGBI", "RVI")) %>% mutate(date_chr = as.character(date))
    gyields <- get_hist_gyields(min(dates), max(dates), duration_years = c(0.25, 5), online = T) %>% mutate(date_chr = as.character(date))
    gyields_latest <- get_current_gyield(duration_years = c(0.25, 5))
    key_rate <- get_key_rate(min(dates), max(dates))
    ruonia_rates <- get_ruonia(min(dates), max(dates))
    miacr_rates <- get_miacr(min(dates), max(dates), duration = c(1, 7)) %>% mutate(date_chr = as.character(date))
    ifx_cbonds <- get_ifx_cbonds(min(dates), max(dates))
    xccy_cbonds <- get_xxcy_cbonds(min(dates), max(dates))
    rvi_cbonds <- get_rvi_cbonds(min(dates), max(dates))
    
    for (dat in dates) {
      
      update_data[[dat]] <- case_when(
        update_data$Indicator == "Urals oil price" & dat == as.character(Sys.Date()) ~ {oil_prices %>% filter(oil == "urals") %>% summarise(price = if_else(n() > 0, first(price), NA_real_)) %>% pull(price)},
        update_data$Indicator == "Urals oil price" ~ get_urals_cbonds(dat, dat)$value,
        update_data$Indicator == "Brent oil price" & dat == as.character(Sys.Date()) ~ {oil_prices %>% filter(oil == "brent") %>% summarise(price = if_else(n() > 0, first(price), NA_real_)) %>% pull(price)},
        update_data$Indicator == "Brent oil price" ~ get_brent_cbonds(dat, dat)$value,
        update_data$Indicator == "Natural gas (TTF, Netherlands)" ~ get_ttfgas_cbonds(dat, dat)$value,
        update_data$Indicator == "MOEX" & dat == as.character(Sys.Date()) ~ {moex_current_index_data %>% filter(indicator == "IMOEX", date_chr == dat) %>% summarise(value = if_else(n() > 0, first(value), NA_real_)) %>% pull(value)},
        update_data$Indicator == "MOEX" ~ {moex_hist_index_data  %>% filter(indicator == "IMOEX", date_chr == dat) %>% pull(value)},
        update_data$Indicator == "MOEXFN" & dat == as.character(Sys.Date()) ~ {moex_current_index_data %>% filter(indicator == "MOEXFN", date_chr == dat) %>% summarise(value = if_else(n() > 0, first(value), NA_real_)) %>% pull(value)},
        update_data$Indicator == "MOEXFN" ~ {moex_hist_index_data  %>% filter(indicator == "MOEXFN", date_chr == dat) %>% pull(value)},
        update_data$Indicator == "RGBI" & dat == as.character(Sys.Date()) ~ {moex_current_index_data %>% filter(indicator == "RGBI", date_chr == dat) %>% summarise(value = if_else(n() > 0, first(value), NA_real_)) %>% pull(value)},
        update_data$Indicator == "RGBI" ~ {moex_hist_index_data %>% filter(indicator == "RGBI", date_chr == dat) %>% pull(value)},
        update_data$Indicator == "OFZZC 3m"& dat == as.character(Sys.Date()) ~ {gyields_latest %>% filter(duration == 0.25) %>% summarise(yield = if_else(n() > 0, first(yield), NA_real_)) %>% pull(yield)},
        update_data$Indicator == "OFZZC 3m" ~ {gyields %>% filter(duration == 0.25) %>% summarise(yield = if_else(n() > 0, first(yield), NA_real_)) %>% pull(yield)},
        update_data$Indicator == "OFZZC 5y" & dat == as.character(Sys.Date()) ~ {gyields_latest %>% filter(duration == 5) %>% summarise(yield = if_else(n() > 0, first(yield), NA_real_)) %>% pull(yield)},
        update_data$Indicator == "OFZZC 5y" ~ {gyields %>% filter(duration == 5) %>% summarise(yield = if_else(n() > 0, first(yield), NA_real_)) %>% pull(yield)},
        update_data$Indicator == "Key rate" ~ key_rate$value[match(dat, key_rate$date)],
        update_data$Indicator == "RUONIA ON" ~ ruonia_rates$value[match(dat, ruonia_rates$date)],
        update_data$Indicator == "MIACR 1d" ~ {miacr_rates %>% filter(duration == 1, date_chr == dat) %>% pull(value)},
        update_data$Indicator == "MIACR 1w" ~ {miacr_rates %>% filter(duration == 7, date_chr == dat) %>% pull(value)},
        update_data$Indicator == "USDRUB TOD" & dat == as.character(Sys.Date()) ~ {fx_current_rates %>% filter(currency == "USD", date_chr == dat) %>% summarise(value = if_else(n() > 0, first(value), NA_real_)) %>% pull(value)},
        update_data$Indicator == "USDRUB TOD" ~ {fx_hist_rates %>% filter(currency == "USD", date_chr == dat) %>% pull(value)},
        update_data$Indicator == "EURRUB TOD" & dat == as.character(Sys.Date()) ~ {fx_current_rates %>% filter(currency == "EUR", date_chr == dat) %>% summarise(value = if_else(n() > 0, first(value), NA_real_)) %>% pull(value)},
        update_data$Indicator == "EURRUB TOD" ~ {fx_hist_rates %>% filter(currency == "EUR", date_chr == dat) %>% pull(value)},
        update_data$Indicator == "CNYRUB TOD" & dat == as.character(Sys.Date()) ~ {fx_current_rates %>% filter(currency == "CNY", date_chr == dat) %>% summarise(value = if_else(n() > 0, first(value), NA_real_)) %>% pull(value)},
        update_data$Indicator == "CNYRUB TOD" ~ {fx_hist_rates %>% filter(currency == "CNY", date_chr == dat) %>% pull(value)},
        update_data$Indicator == "USDRUB CBR" ~ {cbr_fx_rates %>% filter(currency == "US Dollar", date_chr == dat) %>% pull(value)},
        update_data$Indicator == "EURRUB CBR" ~ {cbr_fx_rates %>% filter(currency == "Euro", date_chr == dat) %>% pull(value)},
        update_data$Indicator == "CNYRUB CBR" ~ {cbr_fx_rates %>% filter(currency == "Юань", date_chr == dat) %>% pull(value)},
        update_data$Indicator == "IFX Cbonds" ~ ifx_cbonds$value[match(dat, ifx_cbonds$date)],
        update_data$Indicator == "USDRUB XCCY" ~ xccy_cbonds$value[match(dat, xccy_cbonds$date)],
        update_data$Indicator == "RVI" & dat == as.character(Sys.Date()) ~ {moex_current_index_data %>% filter(indicator == "RVI", date_chr == dat) %>% summarise(value = if_else(n() > 0, first(value), NA_real_)) %>% pull(value)},
        update_data$Indicator == "RVI" ~ {moex_hist_index_data  %>% filter(indicator == "RVI", date_chr == dat) %>% pull(value)},
        TRUE ~ NA_real_
      )
    }
    
    update_data[["Time"]] <- case_when(
      update_data$Indicator == "Urals oil price" ~ {oil_prices %>% filter(oil == "urals") %>% summarise(time = if_else(n() > 0, first(time), NA)) %>% pull(time)},
      update_data$Indicator == "Brent oil price" ~ {oil_prices %>% filter(oil == "brent") %>% summarise(time = if_else(n() > 0, first(time), NA)) %>% pull(time)},
      update_data$Indicator == "USDRUB TOD" ~ {fx_current_rates %>% filter(currency == "USD", date_chr == dat) %>% summarise(time = if_else(n() > 0, first(time), NA)) %>% pull(time)},
      update_data$Indicator == "EURRUB TOD" ~ {fx_current_rates %>% filter(currency == "EUR", date_chr == dat) %>% summarise(time = if_else(n() > 0, first(time), NA)) %>% pull(time)},
      update_data$Indicator == "CNYRUB TOD" ~ {fx_current_rates %>% filter(currency == "CNY", date_chr == dat) %>% summarise(time = if_else(n() > 0, first(time), NA)) %>% pull(time)},
      update_data$Indicator == "MOEX" ~ {moex_current_index_data %>% filter(indicator == "IMOEX", date_chr == dat) %>% summarise(time = if_else(n() > 0, first(time), NA)) %>% pull(time)},
      update_data$Indicator == "MOEXFN" ~ {moex_current_index_data %>% filter(indicator == "MOEXFN", date_chr == dat) %>% summarise(time = if_else(n() > 0, first(time), NA)) %>% pull(time)},
      update_data$Indicator == "OFZZC 3m" ~ {gyields_latest %>% filter(duration == 0.25) %>% summarise(time = if_else(n() > 0, first(time), NA)) %>% pull(time)},
      update_data$Indicator == "OFZZC 5y" ~ {gyields_latest %>% filter(duration == 5) %>% summarise(time = if_else(n() > 0, first(time), NA)) %>% pull(time)},
      update_data$Indicator == "RGBI" ~ {moex_current_index_data %>% filter(indicator == "RGBI", date_chr == dat) %>% summarise(time = if_else(n() > 0, first(time), NA)) %>% pull(time)},
      update_data$Indicator == "RVI" ~ {moex_current_index_data %>% filter(indicator == "RVI", date_chr == dat) %>% summarise(time = if_else(n() > 0, first(time), NA)) %>% pull(time)}
    )
    
    # Обновляем таблицу
    data_values(update_data)
    
    # Определяем текущую дату и обновляем label для текущей даты
    current_date <- as.character(Sys.Date())
    updateTextInput(session, "dataTextOutput", label = paste("Current date (", current_date, "):", sep = ""))
    
    # Получаем колонку с последними значениями, заменяем NA на #Н/Д
    last_col_values <- data_values()[[as.character(Sys.Date())]]
    formatted_prices <- ifelse(is.na(last_col_values), "#Н/Д", last_col_values)
    
    # Обновляем текстовое поле с последними значениями
    updateTextInput(session, "dataTextOutput", value = paste(formatted_prices, collapse = "\t"))
    
    # Определяем ближайший к текущей дате столбец, где MOEX ненулевой
    moex_values <- data_values() %>% filter(Indicator == "MOEX") %>% select(all_of(dates))
    valid_dates <- rev(dates[-length(dates)])[which.max(sapply(rev(dates[-length(dates)]), function(d) !is.na(moex_values[[d]]) & moex_values[[d]] > 0))]
    
    if (length(valid_dates) == 1) {
      non_zero_moex_date <- valid_dates
      
      # Обновляем label для предыдущей даты
      updateTextInput(session, "ifx_ofzzc", label = paste("Previous date (", non_zero_moex_date, ") bond yields:", sep = ""))
      updateTextInput(session, "miacr_ruonia", label = paste("Previous date (", non_zero_moex_date, ") money market rates:", sep = ""))
      
      # Данные для IFX Cbonds, OFZZC 3m, OFZZC 5y
      ifx_ofzzc_values <- data_values() %>%
        filter(Indicator %in% c("IFX Cbonds", "OFZZC 3m", "OFZZC 5y")) %>%
        pull(!!sym(non_zero_moex_date))
      ifx_ofzzc_values <- ifelse(is.na(ifx_ofzzc_values), "#Н/Д", ifx_ofzzc_values)
      updateTextInput(session, "ifx_ofzzc", value = paste(ifx_ofzzc_values, collapse = "\t"))
      
      # Данные для MIACR 1d, RUONIA ON, MOSPRIME ON, MIACR 1w
      miacr_ruonia_values <- data_values() %>%
        filter(Indicator %in% c("MIACR 1d", "RUONIA ON", "MOSPRIME ON", "MIACR 1w")) %>%
        pull(!!sym(non_zero_moex_date))
      miacr_ruonia_values <- ifelse(is.na(miacr_ruonia_values), "#Н/Д", miacr_ruonia_values)
      updateTextInput(session, "miacr_ruonia", value = paste(miacr_ruonia_values, collapse = "\t"))
    }
    
  })
  
  output$data_table <- renderTable({
    data_values()
  }, rownames = FALSE)
  
}

shinyApp(ui = ui, server = server)