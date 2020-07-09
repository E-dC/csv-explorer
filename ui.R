#! /usr/bin/Rscript
# ---------- Read in arguments, setup options ----------
# require('docopt', quietly = TRUE)
# "Run Shiny app for SwingPlanIt events. 
# Use a dbname ending with .rda.
# 
# Usage:
#   app.R  [<dbname>]
# 
# Options:
# -h --help         Show this" -> doc
# 
# args <- docopt::docopt(doc) 

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(leaflet)
library(lubridate)

source('R/objects.R')

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
                                               selected = 'none',
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
                        )
                    )
                )
            ),
            tabPanel(
                'Data Table',
                tableOutput("contents")
            )
        )
    )



