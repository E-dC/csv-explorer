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
source('R/ui_components.R')

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
  
  coltype_structure <- eventReactive(input_data(), {
    list_df_coltypes(input_data())
  })
  
  # ----- Populate charts option in the UI ----- 
  observeEvent(input$submit, {
    update_mappings_inputs(input_data(), session)
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
  
  # ----- Update "Show more" button -----
  observeEvent(input$toggle_optional_mappings, {
    if (input$toggle_optional_mappings %% 2 != 0){
      updateActionButton(session, 'toggle_optional_mappings', label = 'Show less')
    } else {
      updateActionButton(session, 'toggle_optional_mappings', label = 'Show more')
    }
  })

  # Get the mappings from the user when interacting with UI
  aes_mappings <- reactive({
    aes_mappings <- possible_mappings %>%
      sapply(., function(val) input[[val]]) %>%
      purrr::discard(is.null) %>%
      purrr::discard(. == '') %>%
      purrr::discard(. == ' ') %>%
      purrr::map(., as.name) %>%
      purrr::set_names(., stringr::str_remove(names(.),
                                              pattern = 'aes_'))
    print(aes_mappings)
    aes_mappings
  })
  
  # Don't show a plot if there's an error
  output$main_plot <- renderPlot({
    
    params <- list(na.rm = TRUE)
    if (input$stat == 'bin'){
      params <- c(params, bins = input$stat_param)
    }
    
    p <- tryCatch({
      ggplot(data = input_data()) +
        layer(mapping = do.call(aes, aes_mappings()),
              geom = input$geom_type,
              stat = input$stat,
              position = input$position,
              params = params)},
      error =  function(e) {print(e) ; ggplot() + geom_blank()})
    
    p})
})

