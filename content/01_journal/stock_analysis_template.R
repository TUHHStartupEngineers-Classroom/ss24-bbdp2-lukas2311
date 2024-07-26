# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - DATA ANALYSIS -----

# APPLICATION DESCRIPTION ----
# - The user will select 1 stock from the SP 500 stock index
# - The functionality is designed to pull the past 180 days of stock data by default
# - We will implement 2 moving averages - short (fast) and long (slow)
# - We will produce a timeseries visualization
# - We will produce automated commentary based on the moving averages

# LIBRARIES ----
library(tidyverse)
library(fs)
library(glue)

library(rvest)
library(quantmod)

library(plotly)

# 1.0 GET STOCK LIST ----

get_stock_list <- function(stock_index = "DAX") {
  
  # Control for upper and lower case
  index_lower <- str_to_lower(stock_index)
  # Control if user input is valid
  index_valid <- c("dax", "sp500", "dow", "nasdaq")
  if (!index_lower %in% index_valid) {stop(paste0("x must be a character string in the form of a valid exchange.",
                                                  " The following are valid options:\n",
                                                  stringr::str_c(str_to_upper(index_valid), collapse = ", ")))
  }
  
  # Control for different currencies and different column namings in wiki
  vars <- switch(index_lower,
                 dax    = list(wiki     = "DAX", 
                               columns  = c("Ticker symbol", "Company")),
                 sp500  = list(wiki     = "List_of_S%26P_500_companies", 
                               columns  = c("Symbol", "Security")),
                 dow    = list(wiki     = "Dow_Jones_Industrial_Average",
                               columns  = c("Symbol", "Company")),
                 nasdaq = list(wiki     = "NASDAQ-100",
                               columns  = c("Ticker", "Company"))
  )
  
  # Extract stock list depending on user input
  read_html(glue("https://en.wikipedia.org/wiki/{vars$wiki}")) %>% 
    
    # Extract table from wiki
    html_nodes(css = "#constituents") %>% 
    html_table() %>% 
    dplyr::first() %>% 
    as_tibble(.name_repair = "minimal") %>% 
    # Select desired columns (different for each article)
    dplyr::select(vars$columns) %>% 
    # Make naming identical
    set_names(c("symbol", "company")) %>% 
    
    # Clean (just relevant for DOW)
    mutate(symbol = str_remove(symbol, "NYSE\\:[[:space:]]")) %>% 
    
    # Sort
    arrange(symbol) %>%
    # Create the label for the dropdown list (Symbol + company name)
    mutate(label = str_c(symbol, company, sep = ", ")) %>%
    dplyr::select(label)
  
}

stock_list_tbl <- get_stock_list("SP500")
# get_stock_list("DOW")
# get_stock_list("SP500")

stock_list_tbl

# 2.0 EXTRACT SYMBOL BASED ON USER INPUT ----

get_symbol_from_user_input <- function(user_input) {
  user_input %>% ... %>% ... 
}

# 3.0 GET STOCK DATA ----

get_stock_data <- function(stock_symbol, 
                           from = today() - days(180), 
                           to   = today(), 
                           mavg_short = 20, mavg_long = 50) {
  
  stock_symbol %>% quantmod::getSymbols( 
    src         = "yahoo", 
    from        = ..., 
    to          = ..., 
    auto.assign = FALSE) %>% 
    
    # Convert to tibble
    timetk::tk_tbl(preserve_index = T, 
                   silent         = T) %>% 
    
    # Add currency column (based on symbol)
    mutate(currency = case_when(
      str_detect(names(.) %>% last(), "...") ~ "...",
      TRUE                                   ~ "...")) %>% 
    
    # Modify tibble 
    set_names(c("date", "open", "high", "low", "close", "volume", "adjusted", "currency")) %>% 
    drop_na() %>%
    
    # Convert the date column to a date object (I suggest a lubridate function)
    dplyr::... %>% 
    
    # Add the moving averages
    # name the columns mavg_short and mavg_long
    dplyr::...(... = ...(..., ...,  fill = NA, align = "right")) %>% 
    dplyr::...(... = ...(..., ...,  fill = NA, align = "right")) %>% 
    
    # Select the date and the adjusted column
    dplyr::select(date, adjusted, mavg_short, mavg_long, currency)
    
}

stock_data_tbl <- get_stock_data("AAPL", from = "2020-06-01", to = "2021-01-12", mavg_short = 5, mavg_long = 8)

# 4.0 PLOT STOCK DATA ----



# 5.0 GENERATE COMMENTARY ----
warning_signal <- stock_data_tbl %>%
  tail(1) %>% # Get last value
  mutate(mavg_warning_flag = ..) %>% # insert the logical expression
  pull(mavg_warning_flag)
n_short <- stock_data_tbl %>% pull(mavg_short) %>% is.na() %>% sum() + 1
n_long  <- .. # Do the same for the long average

if (warning_signal) {
  str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is below the {n_long}-day moving average, indicating negative trends")
} else {
  str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is above the {n_long}-day moving average, indicating positive trends")
}


generate_commentary <- function(data, user_input) {
  warning_signal <- data %>% ..
  
  n_short <- data %>% ..
  n_long  <- data %>% ..
  
  if (..) {
    str_glue(".. {user_input} .. {n_long} .. negative..")
  } else {
    str_glue(".. {user_input} .. {n_short} .. positive")
    
  }
}

generate_commentary(stock_data_tbl, user_input = user_input)
# 6.0 TEST WORKFLOW ----

# get_stock_list("DAX")
"ADS.DE, Adidas" %>% 
  get_symbol_from_user_input() %>%
  get_stock_data(from = from, to = to ) %>%
  # plot_stock_data() %>%
  generate_commentary(user_input = "ADS.DE, Adidas")
## In reviewing the stock prices of ADS.DE, Adidas, the 20-day moving average is above the 50-day moving average, indicating positive trends

# 7.0 SAVE SCRIPTS ----

fs::dir_create("00_scripts") #create folder

# write functions to an R file
dump(
  list = c("get_stock_list", "get_symbol_from_user_input", "get_stock_data", "plot_stock_data", "currency_format", "generate_commentary"),
  file = "00_scripts/stock_analysis_functions.R", 
  append = FALSE) # Override existing 

