library('CSVexplorer')
library('shiny')

ui <- bootstrapPage(
    navbarPage(
        "CSV Explorer",
        id = 'nav',
        inverse = TRUE,       
        
        # ----- Data Load and datatypes UI -----
        dataLoadUI('data_load',
                   tab_label = "Data Load"),
        
        # ----- Data Table -----
        tableViewerUI('table_viewer',
                      tab_label = "Data Table"), 
            
        # ----- Map -----
        mapViewerUI('map_viewer',
                    tab_label = "Map"),    
        
        # ----- Chart output + UI -----
        plotViewerUI('plot_viewer',
                     tab_label = "Charts")
    )
)
