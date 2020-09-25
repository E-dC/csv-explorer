# ----- Table Viewer module UI function -----

tableViewerUI <- function(id, tab_label = "Data Table") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tabPanel(
    tab_label,
    dataTableOutput(ns("contents"))
  )
}
  
# ----- Table Viewer module server function -----

tableViewerServer <- function(id, loaded_df) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      output$contents <- renderDataTable({loaded_df()},
                                         options = list(pageLength = 20))

    }
  )
}