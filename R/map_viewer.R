# ----- Map Viewer module UI function -----
#' @import shiny
#' @export
mapViewerUI <- function(id, tab_label = "Map") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  tabPanel(
    tab_label,
    leafletOutput(ns('map'), width = '100%', height = '700px')
  )
}

# ----- Map Viewer module server function -----
mapViewerServer <- function(id, loaded_df, lon_cols, lat_cols) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

      output$map <- renderLeaflet({
        leaflet() %>%
          addProviderTiles(providers$CartoDB.Voyager) %>%
          setView(-5, 10, 2)})
      
      observeEvent(loaded_df(), {
        leafletProxy('map') %>%
          clearMarkers() %>%
          clearMarkerClusters()
        
        # Recalculate when input_data() changes
        latitude <- reactive(tryCatch(
          {loaded_df()[[lat_cols()[1]]]},
          error = function(x) NULL))
        longitude <- reactive(tryCatch(
          {loaded_df()[[lon_cols()[1]]]},
          error = function(x) NULL))
        
        if (!is.null(latitude()) & !is.null(longitude())){
          leafletProxy('map') %>%
            addMarkers(lng = longitude(),
                       lat = latitude(),
                       clusterOptions = markerClusterOptions())
        } 
      })
    }
  )
}