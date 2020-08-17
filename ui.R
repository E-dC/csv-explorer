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

ui <- bootstrapPage(
    navbarPage(
        "CSV Explorer",
        id = 'nav',
        inverse = TRUE,       
        
        # ----- Data Load and datatypes UI -----
        tabPanel(
            "Data Load",
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
                mainPanel(ui_datatypes)
            )
        ),
            
        # ----- Data Table -----
        tabPanel(
            "Data Table",
            dataTableOutput("contents")
        ),
            
        # ----- Map -----
        tabPanel(
            "Map",
            leafletOutput('map', width = '100%', height = '700px')
        ),
            
        # ----- Chart output + UI -----
        tabPanel(
            "Charts",
            fluidRow(
                sidebarLayout(
                    # Plot layers elements: mappings, geom, position, stat, stat parameters
                    sidebarPanel(width = 3,
                                 ui_current_layer,
                    
                                 tags$hr(),
                            
                                 # Eventually, way to work with multiple layers
                                 tabsetPanel(type = 'pills',
                                             tabPanel('Layer 1', value = 'layer_1'),
                                             tabPanel('Add layer', value = 'add_layer'),
                                             tabPanel('Remove current layer', value = 'remove_layer'))),
                    mainPanel(width = 9,
                              plotOutput('main_plot', height = "650px"))
                )
            )
        )
    )
)






