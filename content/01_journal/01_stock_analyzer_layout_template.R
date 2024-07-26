# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary


# LIBRARIES ----
library(shiny)
library(shinyWidgets)

library(plotly)
library(tidyverse)
library(quantmod)
library(rvest)
library(glue)
library(stringr)
library(zoo)
source(file = "00_scripts/stock_analysis_functions.R")
          
stock_list_tbl <- get_stock_list("SP500")


# Überprüfe das Ergebnis




ui <- fluidPage(title = "Stock Analyzer",
    # 1.0 HEADER ----

    div(
      h1("Stock Analzer"),
      p("This is my second shiny project")
    ),  
    # 2.0 APPLICATION UI -----
    div(
      column(
        width = 4,
        wellPanel(
          
          # Add content here 
          pickerInput(
            inputId = "index_selection",
            choices = c("SP500", "NASDAQ", "DOW", "DAX")
          ), 
          
          
          uiOutput("indices"),
          actionButton(
            inputId = "analyze",
            icon=icon("download"),
            label="Analyze"
          ),
          dateRangeInput(
            inputId = "date_range",
            label = "Select Date Range:",
            start = as.Date("2021-01-01"),   # Startdatum
            end = as.Date("2024-12-12"),     # Enddatum
            min = as.Date("2020-01-01"),     # Minimaler Startwert
            max = as.Date("2024-12-31"),     # Maximaler Endwert
            format = "yyyy-mm-dd"            # Datumsformat
          ),
          hr(),
          
          sliderInput(
            inputId = "shortSlider",
            label = "Short Moving Average",
            min = 5,       # Minimaler Wert
            max = 40,     # Maximaler Wert
            value = 50,    # Standardwert
            step = 1       # Schrittweite des Sliders
          ),
          sliderInput(
            inputId = "longSlider",
            label = "Long Moving Average",
            min = 50,       # Minimaler Wert
            max = 120,     # Maximaler Wert
            value = 50,    # Standardwert
            step = 1       # Schrittweite des Sliders
          ),
        )
      ), 
      column(
        width = 8,
        div(uiOutput("plot_header")),  
        verbatimTextOutput("stock_data"),  
        plotlyOutput("plotly_plot")
        )
    ),          

      # 3.0 ANALYST COMMENTARY ----
    fluidRow(
      column(
        width = 12,
        uiOutput("commentary")
      )
    ) 
)

# SERVER ----
server <- function(input, output, session) {
  
  # Reactive expression to store the selected stock symbol
  stock_symbol <- eventReactive(input$analyze, {
    stock_sel = x <- unlist(strsplit(input$stock_selection, ","))
    stock_sel = stock_sel[1]
    req(stock_sel)  # Ensure input is available
    stock_sel
    
  })
  
  output$plot_header <- renderUI({
    req(stock_symbol())  # Warten, bis das Aktiensymbol verfügbar ist
    h4(paste("Stock Price Plot for", stock_symbol()))
  })
  
  output$commentary <- renderText({
    req(stock_symbol())  # Wait until stock symbol is available
    stock_data <- stock_data_tbl()
    generate_commentary(stock_data, user_input = stock_symbol())
  })
  
  stock_data_tbl <- reactive({
    req(stock_symbol())  # Warten, bis das Aktiensymbol verfügbar ist
    stock_symbol() %>% 
      get_stock_data(from = today() - days(180), 
                     to   = today(),
                     mavg_short = 20,
                     mavg_long  = 50)
  })
  output$plotly_plot <- renderPlotly({
    # Zugriff auf die reaktiven Daten
    data <- stock_data_tbl() 
    date_min <- as.Date(input$date_range[1])
    date_max <- as.Date(input$date_range[2])
    
    filtered_data <- data %>% filter(date >= date_min & date <= date_max)
    
    
    
   
    filtered_data$mavg_short <- rollmean(filtered_data$adjusted, k = input$shortSlider, fill = NA, align = "right")
    filtered_data$mavg_long <- rollmean(filtered_data$adjusted, k = input$longSlider, fill = NA, align = "right")
    
    
    
    
    
    # Erstellen des Plotly-Diagramms
    plot_ly() %>%
      add_trace(data = filtered_data, x = ~date, y = ~adjusted, type = 'scatter', mode = 'lines+markers', name = 'Adjusted') %>%
      add_trace(data = filtered_data, x = ~date, y = ~mavg_short, type = 'scatter', mode = 'lines', name = 'Moving Average Short', line = list(color = 'red')) %>%
      add_trace(data = filtered_data, x = ~date, y = ~mavg_long, type = 'scatter', mode = 'lines', name = 'Moving Average Long', line = list(color = 'green')) %>%
      layout(title = 'Zeitreihe der Aktienkurse mit gleitenden Durchschnitten',
             xaxis = list(title = 'Datum'),
             yaxis = list(title = 'Aktienkurs'))
  })
  
  
 
  
  # Stock-Daten ausgeben
  output$stock_data <- renderPrint({
    req(stock_data_tbl())  # Warten, bis die Stock-Daten verfügbar sind
    stock_data_tbl()
  })
  
  
  selected_index <- reactive({
    req(input$index_selection)  # Ensure the index selection input is available
    input$index_selection
  })
  
   stock_options <- reactive({
    index <- selected_index()
    # Ensure the index is not NULL or empty
    req(index)
    # Update stock list based on selected index
    stock_list_tbl <- tibble(
      label = get_stock_list(index)  # Function to get stock list for the selected index
    )
    stock_list_tbl
  })
  output$indices <- renderUI({
    # Use the stock_options reactive to get updated choices
    stock_list <- stock_options()
    pickerInput(
      inputId = "stock_selection", 
      choices = stock_list$label,
      multiple = FALSE, 
      options = list(actionsBox = FALSE, liveSearch = TRUE, size = 10)
    )
  })
  #------------------------------------------------------
  
  
  
}


# RUN APP ----
shinyApp(ui = ui, server = server)
