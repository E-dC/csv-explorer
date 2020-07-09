#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(tibble)

source('R/objects.R')
source('R/utils.R')

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  base_data <- eventReactive(input$csv_input, {
    inFile <- input$csv_input
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header) %>% as_tibble()
  })
  
  
  observeEvent(base_data(), {
    update_type_inputs(base_data(), session)
  })
  
  type_triggers <- reactive(c(input$int_cols, input$dbl_cols, input$lgl_cols,
                              input$chr_cols, input$fct_cols, 
                              input$date_cols, input$date_parsing,
                              input$lat_cols, input$lon_cols))

  input_data <- eventReactive(type_triggers(), {
    o <- base_data() %>%
          mutate(across(input$int_cols, as.integer)) %>%
          mutate(across(input$dbl_cols, as.double)) %>%
          mutate(across(input$chr_cols, as.character)) %>%
          mutate(across(input$lgl_cols, as.logical)) %>%
          mutate(across(input$fct_cols, as.factor)) %>%
          mutate(across(input$date_cols, date_parsing_options[[input$date_parsing]]))
        
    o
  })  
  
  
  output$contents <- renderTable({
    input_data() %>%
      mutate(across(input$date_cols, as.character))
  })
})

