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

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  # col_name_types <- list(
  #   columns = c(),
  #   numeric = c(),
  #   character = c(),
  #   logical = c(),
  #   factor = c()
  # )
  
  input_data <- eventReactive(input$csv_input, {
    inFile <- input$csv_input
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header) %>% as_tibble()
  })
  
  type2colnames <- function(df, x, col_names = names(df)){
    valid_types <- switch(x,
                          'geo' = c('double', 'numeric'),
                          'lat' = c('double', 'numeric'),
                          'lon' = c('double', 'numeric'),
                          'numeric' = c('numeric', 'integer', 'double'),
                          'char' = c('character'),
                          'fct' = c('factor'),
                          'lgl' = c('logical'),
                          'date' = c('date', 'Date'),
                          NULL = c()
                          )
    
    if (x %in% c('lat', 'lon')){
      p <- ifelse(x == 'lat', 'lat(itude)?', '(long?(itude)?|lng)')
      o <- sapply(col_names,
                  function(x) ifelse(
                    grepl(p, x, ignore.case = TRUE) &
                    is(df[[x]])[1] %in% valid_types,
                    x,
                    NA),
                  USE.NAMES = FALSE)
    } else {
      o <- sapply(col_names,
                  function(x) ifelse(
                    is(df[[x]])[1] %in% valid_types,
                    x,
                    NA),
                  USE.NAMES = FALSE)
    }
    o[!is.na(o)]
  }

  update_type_inputs <- function(df, session){
    choices <- names(df)
    
    lat <- type2colnames(df, 'lat')
    
    updateSelectInput(session,
                      'lat_cols',
                      choices = choices,
                      selected = lat)
    
    lon <- type2colnames(df, 'lon')
    # print(lon)
    updateSelectInput(session,
                      'lon_cols',
                      choices = choices,
                      selected = lon)
    
    col_names <- choices[! choices %in% c(lat, lon)]
    
    updateSelectInput(session,
                      'real_cols',
                      choices = choices,
                      selected = type2colnames(df, 'numeric', col_names))
    
    updateSelectInput(session,
                      'char_cols',
                      choices = choices,
                      selected = type2colnames(df, 'char', col_names))
    
    updateSelectInput(session,
                      'factor_cols',
                      choices = choices,
                      selected = type2colnames(df, 'fct', col_names))
    
    updateSelectInput(session,
                      'lgl_cols',
                      choices = choices,
                      selected = type2colnames(df, 'lgl', col_names))
  }
  
  observeEvent(input_data(), {
    update_type_inputs(input_data(), session)
  })
  
  # col_name_types <- eventReactive(input_data(),{
  #   list(columns = names(input_data()),
  #        numeric = sapply(
  #          names(input_data()),
  #          function(x) ifelse(
  #            typeof(input_data()[[x]]) %in% c('integer', 'double', 'numeric'),
  #            x,
  #            NA),
  #          USE.NAMES = FALSE),
  #        character = sapply(
  #          names(input_data()),
  #          function(x) ifelse(
  #            typeof(input_data()[[x]]) %in% c('character'),
  #            x,
  #            NA),
  #          USE.NAMES = FALSE),
  #        logical = sapply(
  #          names(input_data()),
  #          function(x) ifelse(
  #            typeof(input_data()[[x]]) %in% c('logical'),
  #            x,
  #            NA),
  #          USE.NAMES = FALSE), 
  #        factor = sapply(
  #          names(input_data()),
  #          function(x) ifelse(
  #            typeof(input_data()[[x]]) %in% c('factor'),
  #            x,
  #            NA),
  #          USE.NAMES = FALSE)
  #        
  # )
  # })
  
  output$contents <- renderTable({
    input_data()
  })
})

