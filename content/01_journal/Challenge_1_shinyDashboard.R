

library(flexdashboard)
library(rsconnect)
# Core
library(tidyverse)

# Interactive Visualizations
library(plotly)

# Spatial Data
library(raster)
library(sf)
library(shiny)
# Currency formatting
source("00_scripts/plot_sales.R")

library(geodata)
library(sp)
library(shiny)
library(knitr)
library(shinyWidgets)


bikes_tbl      <- readRDS("01_data/bikes_tbl.rds")
bikeshops_tbl  <- readRDS("01_data/bikeshops_tbl.rds")
orderlines_tbl <- readRDS("01_data/orderlines_tbl.rds")

bike_orderlines_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl,     by = c("product_id" = "bike_id")) %>%
  left_join(bikeshops_tbl, by = c("customer_id" = "bikeshop_id")) %>%
  mutate(total_price = price_euro * quantity)




ui <- fluidPage(
  id="fluidpage",
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {
        background-color: white;
        color: black;
      }
      #sidebar {
      background-color: #dde6f0;
      height: 500px;
      margin: 0;
      }
      #fluidpage {
      margin: 0;
      }
      .button {
      background-color: #090909
      color: white
      }
      #first {
      border: 4px solid #e7e7e7;
      }
      .shiny-input-container {
        color: #474747;
      }"))
    
  ),
  
  titlePanel("Sales Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(id="sidebar",
      width=2,
      dateRangeInput(
        inputId = "date_range",
        label = "Select Date Range:",
        start = as.Date("2015-01-01"),   # Startdatum
        end = as.Date("2024-12-31"),     # Enddatum
        min = as.Date("2015-01-01"),     # Minimaler Startwert
        max = as.Date("2024-12-31"),     # Maximaler Endwert
        format = "yyyy-mm-dd"            # Datumsformat
      ),
      checkboxGroupButtons(
        inputId = "dataset", 
        label = "Bike Type:", 
        choices = c("Mountain", "Road", "Hybrid/City", "E-Bikes", "Gravel"), 
        selected = "cars",
        checkIcon = list(
          yes = icon("ok", 
                     lib = "glyphicon"),
          no = icon("remove",
                    lib = "glyphicon")
        )
      ),
      pickerInput(
        inputId = "bikefamily", 
        label = "Bike Family:", 
        choices = c("Race", "Triathlon", "Endurance", "Cyclocross", "Trail", "CrossCountry", "Enduro", "Downhill", "FatBikes", "DirtJump"),
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE
        ),
        multiple = FALSE
      ),
      
      actionButton("apply", "Apply"),
      actionButton("reset", "Reset")
    ),
    
    mainPanel(
      id = "main",
      
      uiOutput(
      "stats"),
      uiOutput(
        "sales"),
      uiOutput(
        "ratio"),
      column(id = "first",
        width=5,
        h5("By State"),
        plotlyOutput("stock_plot"),
      ),
      column(
        width=5,
        h5("Over Time"),
        tabsetPanel(
          tabPanel("D", 
                   plotlyOutput("plot1")
          ),
          tabPanel("W", 
                   plotlyOutput("plot2")
          ),
          tabPanel("M", 
                   plotlyOutput("plot3")
          ),
          tabPanel("Q", 
                   plotlyOutput("plot4")
          ),
          tabPanel("Y", 
                   plotlyOutput("plot5")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$stock_plot <- renderPlotly({
    geo_plot_tbl <- bike_orderlines_tbl %>% 
      group_by(state) %>%
      summarise(total_revenue = sum(total_price)) %>%
      ungroup() %>%
      right_join(germany_sf, by = c("state" = "VARNAME_1")) %>% 
      mutate(total_revenue = ifelse(is.na(total_revenue), 0, total_revenue)) %>% 
      mutate(label_text = str_glue("State: {state}
                                         Revenue: {format_to_euro(total_revenue)}")) %>% 
      # Convert back to an sf object, that can be plotted
      st_as_sf()
    
    plot_ly(geo_plot_tbl, 
            split      = ~NAME_1, 
            color      = ~total_revenue,
            colors     = "Blues",
            stroke     = I("black"),
            hoverinfo  = 'text', 
            text       = ~label_text, 
            hoveron    = "fills", 
            showlegend = FALSE) 
    
  })
  
  output$stats <- renderText({
    total_orders <- bike_orderlines_tbl %>%
      summarise(total_orders = n_distinct(order_id))
    single_element <- as.character(total_orders[[1]])
    
    result2 = paste(single_element, "Orders")
    result2
  })
  output$ratio <- renderText({
  
  ratio <- bike_orderlines_tbl %>%
    filter(category_1 %in% c(category_1, category_2)) %>%
    group_by(category_1) %>%
    summarise(total_sales = sum(total_price)) %>%
    pivot_wider(names_from = category_1, values_from = total_sales, names_prefix = "total_") %>%
    summarise(ratio = total_Mountain / total_Road) %>%
    pull(ratio)
    result3 = paste(ratio, "Ratio Mountain to Road")
    result3
    
  })
  
  
  output$sales <- renderText({
    total_sales <- bike_orderlines_tbl %>%
      summarise(total_sales = sum(total_price))
    single_elementsales <- as.character(total_sales[[1]]/1000000)
    result = paste(single_elementsales, "M  $ Sales")
    result
  })
  
  #------------------------------------
  
  output$plot1 <- renderPlotly({
    
    
    selected_categories_1 <- "All"
    selected_categories_2 <- "All"
    
    
    
    total_sales_m_tbl <- bike_orderlines_tbl %>%
      dplyr::select(order_date, category_1, category_2, total_price) %>%
      
      mutate(date_rounded = floor_date(order_date, unit = "day")) %>%
      
      group_by(date_rounded, category_1, category_2) %>%
      summarise(total_sales = sum(total_price)) %>%
      ungroup() %>%
      
      mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}"))
    
   
    date_min <- as.Date(input$date_range[1])
    date_max <- as.Date(input$date_range[2])
    
    filtered_data <- total_sales_m_tbl %>% filter(date_rounded >= date_min & date_rounded <= date_max)
    
    plot_ly() %>%
      add_trace(data = filtered_data, x = ~date_rounded, y = ~total_sales, type = 'scatter', mode = 'lines+markers', name = 'Adjusted') %>%
       layout(title = 'Zeitreihe der Aktienkurse mit gleitenden Durchschnitten',
             xaxis = list(title = 'Datum'),
             yaxis = list(title = 'Aktienkurs'))
  })
  
 
  
  output$plot2 <- renderPlotly({
    
    
    selected_categories_1 <- "All"
    selected_categories_2 <- "All"
    
    
    total_sales_m_tbl <- bike_orderlines_tbl %>%
      dplyr::select(order_date, category_1, category_2, total_price) %>%
      
      mutate(date_rounded = floor_date(order_date, unit = "week")) %>%
      
      group_by(date_rounded, category_1, category_2) %>%
      summarise(total_sales = sum(total_price)) %>%
      ungroup() %>%
      
      mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}"))
    
    
    date_min <- as.Date(input$date_range[1])
    date_max <- as.Date(input$date_range[2])
    
    filtered_data <- total_sales_m_tbl %>% filter(date_rounded >= date_min & date_rounded <= date_max)
    
    plot_ly() %>%
      add_trace(data = filtered_data, x = ~date_rounded, y = ~total_sales, type = 'scatter', mode = 'lines+markers', name = 'Adjusted') %>%
      layout(title = 'Zeitreihe der Aktienkurse mit gleitenden Durchschnitten',
             xaxis = list(title = 'Datum'),
             yaxis = list(title = 'Aktienkurs'))
  })
  output$plot3 <- renderPlotly({
    
    
    selected_categories_1 <- "All"
    selected_categories_2 <- "All"
    
    
    total_sales_m_tbl <- bike_orderlines_tbl %>%
      dplyr::select(order_date, category_1, category_2, total_price) %>%
      
      mutate(date_rounded = floor_date(order_date, unit = "month")) %>%
      
      group_by(date_rounded, category_1, category_2) %>%
      summarise(total_sales = sum(total_price)) %>%
      ungroup() %>%
      
      mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}"))
    
    
    date_min <- as.Date(input$date_range[1])
    date_max <- as.Date(input$date_range[2])
    
    filtered_data <- total_sales_m_tbl %>% filter(date_rounded >= date_min & date_rounded <= date_max)
    
    plot_ly() %>%
      add_trace(data = filtered_data, x = ~date_rounded, y = ~total_sales, type = 'scatter', mode = 'lines+markers', name = 'Adjusted') %>%
      layout(title = 'Zeitreihe der Aktienkurse mit gleitenden Durchschnitten',
             xaxis = list(title = 'Datum'),
             yaxis = list(title = 'Aktienkurs'))
  })

  output$plot4 <- renderPlotly({
    
    
    selected_categories_1 <- "All"
    selected_categories_2 <- "All"
    
    
    total_sales_m_tbl <- bike_orderlines_tbl %>%
      dplyr::select(order_date, category_1, category_2, total_price) %>%
      
      mutate(date_rounded = floor_date(order_date, unit = "quarter")) %>%
      
      group_by(date_rounded, category_1, category_2) %>%
      summarise(total_sales = sum(total_price)) %>%
      ungroup() %>%
      
      mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}"))
    
    
    date_min <- as.Date(input$date_range[1])
    date_max <- as.Date(input$date_range[2])
    
    filtered_data <- total_sales_m_tbl %>% filter(date_rounded >= date_min & date_rounded <= date_max)
    
    plot_ly() %>%
      add_trace(data = filtered_data, x = ~date_rounded, y = ~total_sales, type = 'scatter', mode = 'lines+markers', name = 'Adjusted') %>%
      layout(title = 'Zeitreihe der Aktienkurse mit gleitenden Durchschnitten',
             xaxis = list(title = 'Datum'),
             yaxis = list(title = 'Aktienkurs'))
  })  
  
  output$plot5 <- renderPlotly({
    
    
    selected_categories_1 <- "All"
    selected_categories_2 <- "All"
    
    
    total_sales_m_tbl <- bike_orderlines_tbl %>%
      dplyr::select(order_date, category_1, category_2, total_price) %>%
      
      mutate(date_rounded = floor_date(order_date, unit = "year")) %>%
      
      group_by(date_rounded, category_1, category_2) %>%
      summarise(total_sales = sum(total_price)) %>%
      ungroup() %>%
      
      mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}"))
    
    
    date_min <- as.Date(input$date_range[1])
    date_max <- as.Date(input$date_range[2])
    
    filtered_data <- total_sales_m_tbl %>% filter(date_rounded >= date_min & date_rounded <= date_max)
    
    plot_ly() %>%
      add_trace(data = filtered_data, x = ~date_rounded, y = ~total_sales, type = 'scatter', mode='lines+markers', mode = 'lines+markers', name = 'Adjusted') %>%
      layout(title = 'Zeitreihe der Aktienkurse mit gleitenden Durchschnitten',
             xaxis = list(title = 'Datum'),
             yaxis = list(title = 'Aktienkurs'))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
