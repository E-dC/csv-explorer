# Load all dependencies
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(leaflet)
library(readr)
library(tibble)
library(lubridate)
library(DT)

source('R/objects.R')
source('R/utils.R')

server <- shinyServer(function(input, output, session) {
 
  # ----- File input ----- 
  base_data <- eventReactive(input$csv_input, {
    inFile <- input$csv_input
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header) %>% as_tibble()
  })
  
  # ----- Populate dataset types in the UI ----- 
  observeEvent(base_data(), {
    update_type_inputs(base_data(), session)
  })
  
  # ----- Apply required type transformations on data ----- 
  # Recalculate every time user presses the submit button
  input_data <- eventReactive(input$submit, {

    date_parser <- ifelse(input$date_parsing == 'auto',
                          find_parser(base_data() %>% select(all_of(input$date_cols))),
                          date_parsing_options[[input$date_parsing]])

    o <- base_data() %>%
          mutate(across(input$int_cols, as.integer)) %>%
          mutate(across(input$dbl_cols, as.double)) %>%
          mutate(across(input$chr_cols, as.character)) %>%
          mutate(across(input$lgl_cols, as.logical)) %>%
          mutate(across(input$fct_cols, as.factor)) %>%
          mutate(across(input$date_cols, date_parser))
    o
  })  
  
  # ----- Data Table View -----
  output$contents <- renderDataTable({input_data()},
                                     options = list(pageLength = 20))

  # ----- Map View -----
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      setView(-5, 10, 2)})
  
  # Recalculate when input_data() changes
  latitude <- reactive(tryCatch(
    {input_data()[[input$lat_cols[1]]]},
    error = function(x) NULL))
  longitude <- reactive(tryCatch(
    {input_data()[[input$lon_cols[1]]]},
    error = function(x) NULL))

  observeEvent(input$submit, {
    leafletProxy('map') %>%
      clearMarkers() %>%
      clearMarkerClusters()
    
    if (!is.null(latitude()) & !is.null(longitude())){
      leafletProxy('map') %>%
        addMarkers(lng = longitude(),
                   lat = latitude(),
                   clusterOptions = markerClusterOptions())
    } 
  })
  
})

