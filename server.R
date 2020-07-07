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

source('R/utils.R')

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  input_data <- eventReactive(input$csv_input, {
    inFile <- input$csv_input
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header) %>% as_tibble()
  })
  
  
  observeEvent(input_data(), {
    update_type_inputs(input_data(), session)
  })
  
  output$contents <- renderTable({
    input_data()
  })
})

