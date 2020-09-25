# ----- Data Load module structures -----
choices <- list()
date_parsing_functions <- list("dmy" = lubridate::dmy,
                               "dmy_h" = lubridate::dmy_h,
                               "dmy_hm" = lubridate::dmy_hm,
                               "dmy_hms" = lubridate::dmy_hms,
                               "dym" = lubridate::dym,
                               "mdy" = lubridate::mdy,
                               "mdy_h" = lubridate::mdy_h,
                               "mdy_hm" = lubridate::mdy_hm,
                               "mdy_hms" = lubridate::mdy_hms,
                               "myd" = lubridate::myd,
                               "ydm" = lubridate::ydm,
                               "ydm_h" = lubridate::ydm_h,
                               "ydm_hm" = lubridate::ydm_hm,
                               "ydm_hms" = lubridate::ydm_hms,
                               "ymd" = lubridate::ymd,
                               "ymd_h" = lubridate::ymd_h,
                               "ymd_hm" = lubridate::ymd_hm,
                               "ymd_hms" = lubridate::ymd_hms)
date_parsing_options <- c("auto" = function(x) return(x), date_parsing_functions)

lat_pat <- 'lat(itude)?'
lon_pat <- '(long?(itude)?|lng)'
date_pat <- '(\\bdate\\b|date$|\\bdatetime\\b|datetime$)'


# ----- Data Load module UI function -----
#' @import shiny
#' @export
dataLoadUI <- function(id, tab_label = "Data Load") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  choices <- list()

  tabPanel(
    tab_label,
    sidebarLayout(
      
      # ----- File Upload -----
      sidebarPanel(
        fileInput(ns('csv_input'),
                  label = 'Select a CSV file to explore',
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$hr(),
        checkboxInput(ns("header"), "Header", TRUE)
      ),
      
      # ----- Datatypes Handling -----
      mainPanel(
        
        fluidRow(
          titlePanel('Select column types for each column.')
        ),
        # ----- Numeric types -----
        fluidRow(
          column(4, selectInput(ns('int_cols'),
                                label = 'Integer',
                                choices = choices,
                                multiple = TRUE)),
          column(4, selectInput(ns('dbl_cols'),
                                label = 'Float',
                                choices = choices,
                                multiple = TRUE)),
          column(4, selectInput(ns('lgl_cols'),
                                label = 'Logical',
                                choices = choices,
                                multiple = TRUE))),
        hr(),
        # ----- Text types -----
        fluidRow(
          column(4, selectInput(ns('chr_cols'),
                                label = 'Character',
                                choices = choices,
                                multiple = TRUE)),
          column(4, selectInput(ns('fct_cols'),
                                label = 'Factor',
                                choices = choices,
                                multiple = TRUE))),
        hr(),
        # ----- Date parsing -----
        fluidRow(
          column(4, selectInput(ns('date_cols'),
                                label = 'Date',
                                choices = choices,
                                multiple = TRUE)),
          column(4, selectInput(ns('date_parsing'),
                                label = 'Date parsing instructions',
                                choices = names(date_parsing_options),
                                selected = 'auto',
                                multiple = FALSE))),
        hr(),
        # ----- Lat/lon types -----
        fluidRow(
          column(4, selectInput(ns('lat_cols'),
                                label = 'Geo: Latitude',
                                choices = choices,
                                multiple = TRUE)),
          column(4, selectInput(ns('lon_cols'),
                                label = 'Geo: Longitude',
                                choices = choices,
                                multiple = TRUE))),
        # ----- Submit button -----
        fluidRow(column(5),
                 column(2, actionButton(ns('submit'),
                                        'Submit',
                                        type = "button", 
                                        class = "btn btn-danger")),
                 column(5))
      )
    )
  )
}

# ----- Data Load module server function -----
#' @import shiny, dplyr, tidyr, readr
#' @export
dataLoadServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
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
      df <- eventReactive(input$submit, {
        
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
      
      # ----- Return values to be used by other modules -----
      return(list(
        loaded_df = df,
        lon_cols = reactive(input$lon_cols),
        lat_cols = reactive(input$lat_cols),
        submit_data = reactive(input$submit)
      ))
    }
  )    
}
