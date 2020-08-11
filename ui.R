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

ui <- bootstrapPage(
    navbarPage(
        "CSV Explorer",
        id = 'nav',
        inverse = TRUE,       
        
        # Map panel
        tabPanel(
            'Data Load',
            
            # Include CSS
            # div(class="outer",
            #     tags$head(
            #         includeCSS("www/styles.css"),
            #     ),
                
                sidebarLayout(
                    sidebarPanel(
                        fileInput('csv_input',
                                  label = 'Select a CSV file to explore',
                                  accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")
                        ),
                        tags$hr(),
                        checkboxInput("header", "Header", TRUE)
                    ),
                    mainPanel(
                        fluidRow(
                            titlePanel('Select column types for each column.')
                        ),
                        fluidRow(
                            column(4,
                                selectInput('int_cols',
                                            label = 'Integer',
                                            choices = choices,
                                            multiple = TRUE)
                            ),
                            column(4,
                                selectInput('dbl_cols',
                                            label = 'Float',
                                            choices = choices,
                                            multiple = TRUE)
                            ),
                            column(4,
                                   selectInput('lgl_cols',
                                               label = 'Logical',
                                               choices = choices,
                                               multiple = TRUE)
                            )
                        ),
                        hr(),
                        fluidRow(
                            column(4,
                                   selectInput('chr_cols',
                                               label = 'Character',
                                               choices = choices,
                                               multiple = TRUE)
                            ),
                            column(4,
                                   selectInput('fct_cols',
                                               label = 'Factor',
                                               choices = choices,
                                               multiple = TRUE)
                            )
                        ),
                        hr(),
                        fluidRow(
                            column(4,
                                   selectInput('date_cols',
                                               label = 'Date',
                                               choices = choices,
                                               multiple = TRUE)
                            ),
                            column(4,
                                   selectInput('date_parsing',
                                               label = 'Date parsing instructions',
                                               choices = names(date_parsing_options),
                                               selected = 'auto',
                                               multiple = FALSE
                                    )
                            )
                        ),
                        hr(),
                        fluidRow(
                            column(4,
                                   selectInput('lat_cols',
                                               label = 'Geo: Latitude',
                                               choices = choices,
                                               multiple = TRUE)
                            ),
                            column(4,
                                   selectInput('lon_cols',
                                               label = 'Geo: Longitude',
                                               choices = choices,
                                               multiple = TRUE)
                            )
                        ),
                        fluidRow(column(5),
                                 column(2,
                                        actionButton('submit',
                                                     'Submit',
                                                     type = "button", 
                                                     class = "btn btn-danger")),
                                 column(5))
                    )
                )
            ),
            tabPanel(
                'Data Table',
                dataTableOutput("contents")
            ),
            tabPanel("Map",
                     leafletOutput('map', width = '100%', height = '700px')
            ),
            tabPanel("Charts",
                     fluidPage(
                        sidebarLayout(
                            sidebarPanel(width = 2,
                                selectInput('aes_x',
                                            'X',
                                            choices = choices),
                                selectInput('aes_y',
                                            'Y',
                                            choices = choices),
                                selectInput('aes_fill',
                                            'Fill',
                                            choices = choices),
                                selectInput('aes_colour',
                                            'Colour',
                                            choices = choices),
                                selectInput('aes_alpha',
                                            'Transparency',
                                            choices = choices),
                                selectInput('aes_group',
                                            'Group',
                                            choices = choices),
                                selectInput('aes_linetype',
                                            'Line type',
                                            choices = choices),
                                selectInput('aes_size',
                                            'Size',
                                            choices = choices)
                     ),
                     mainPanel(width = 10,
                         fluidRow(
                             column(3,
                             selectInput('geom_type',
                                         'Plot type',
                                         choices = c('None' = ' ',
                                                     'Bar' = 'bar',
                                                     'Point' = 'point',
                                                     'Line' = 'line'),
                                         selected = 'None',
                                         multiple = FALSE)),
                             column(3,
                             selectInput('position',
                                         'Position',
                                         choices = c('None' = ' ',
                                                     'Identity' = 'identity',
                                                     'Stack' = 'stack',
                                                     'Dodge' = 'dodge',
                                                     'Jitter' = 'jitter'),
                                         selected = 'Identity',
                                         multiple = FALSE)),
                             column(3,
                             selectInput('stat',
                                         'Stat',
                                         choices = c('None' = ' ',
                                                     'Identity' = 'identity',
                                                     'Count' = 'count',
                                                     'Bin' = 'bin',
                                                     'Unique' = 'unique'),
                                         selected = 'None',
                                         multiple = FALSE)),
                             column(3,
                             sliderInput('stat_param',
                                         'Bins',
                                         value = 30,
                                         min = 1,
                                         max = 100,
                                         step = 1))
                         ),
                         fluidRow(
                               plotOutput('main_plot', height = "600px")
                         )
                     )
                )
            )
        )
    )
)





