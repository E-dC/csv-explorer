library('CSVexplorer')
library('shiny')
library('zeallot')

server <- shinyServer(function(input, output, session) {

  # ----- Data Load Module server call ----- 
  c(loaded_df, lon_cols, lat_cols, submit_data) %<-% dataLoadServer('data_load')
  
  # ----- Data Table View -----
  tableViewerServer('table_viewer', loaded_df)
  
  # ----- Map View -----
  mapViewerServer('map_viewer', loaded_df, lon_cols, lat_cols)

  # ----- Plot View -----
  plotViewerServer('plot_viewer', loaded_df)
  
})

