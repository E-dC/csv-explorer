# ----- Plot Viewer module structures -----
possible_mappings <- c('aes_x', 'aes_y', 'aes_alpha', 'aes_fill', 'aes_colour', 'aes_size', 'aes_linetype', 'aes_group')

# ----- Plot Viewer module UI function -----
plotViewerUI <- function(id, tab_label = "Charts") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tabPanel(
    tab_label,
    fluidRow(
      sidebarLayout(
        # Plot layers elements: mappings, geom, position, stat, stat parameters
        sidebarPanel(width = 3,
                     tabsetPanel(
                       # ----- Mappings UI -----
                       tabPanel('Mappings',
                                selectInput(ns('aes_x'),
                                            'X',
                                            choices = choices),
                                selectInput(ns('aes_y'),
                                            'Y',
                                            choices = choices),
                                conditionalPanel('input.toggle_optional_mappings % 2 != 0',
                                                 ns = ns,
                                                 selectInput(ns('aes_fill'),
                                                             'Fill',
                                                             choices = choices),
                                                 selectInput(ns('aes_colour'),
                                                             'Colour',
                                                             choices = choices),
                                                 selectInput(ns('aes_alpha'),
                                                             'Transparency',
                                                             choices = choices),
                                                 selectInput(ns('aes_group'),
                                                             'Group',
                                                             choices = choices),
                                                 selectInput(ns('aes_linetype'),
                                                             'Line type',
                                                             choices = choices),
                                                 selectInput(ns('aes_size'),
                                                             'Size',
                                                             choices = choices)),
                                actionLink(ns('toggle_optional_mappings'), label = 'Show more')
                       ),
                       
                       # ----- Geom, position, stat UI -----
                       tabPanel('Plot',
                                selectInput(ns('geom_type'),
                                            'Plot type',
                                            choices = c('None' = ' ',
                                                        'Bar' = 'bar',
                                                        'Point' = 'point',
                                                        'Line' = 'line'),
                                            selected = 'None',
                                            multiple = FALSE),
                                selectInput(ns('position'),
                                            'Position',
                                            choices = c('None' = ' ',
                                                        'Identity' = 'identity',
                                                        'Stack' = 'stack',
                                                        'Dodge' = 'dodge',
                                                        'Jitter' = 'jitter'),
                                            selected = 'identity',
                                            multiple = FALSE),
                                selectInput(ns('stat'),
                                            'Stat',
                                            choices = c('None' = ' ',
                                                        'Identity' = 'identity',
                                                        'Count' = 'count',
                                                        'Bin' = 'bin',
                                                        'Unique' = 'unique'),
                                            selected = 'identity',
                                            multiple = FALSE)
                       ),
                       
                       # ----- Stat parameters UI -----
                       tabPanel('Stat',
                                sliderInput(ns('stat_param'),
                                            'Bins',
                                            value = 30,
                                            min = 1,
                                            max = 100,
                                            step = 1)
                       )
                     ),

                     
                     tags$hr(),
                     
                     # Eventually, way to work with multiple layers
                     tabsetPanel(id = ns('plot_layers'),
                                 type = 'pills',
                                 tabPanel('Layer 1', value = 'layer_1'),
                                 tabPanel('Layer 2', value = 'layer_2'),
                                 tabPanel('Layer 3', value = 'layer_3'),
                                 tabPanel('Layer 4', value = 'layer_4'),
                                 tabPanel('Layer 5', value = 'layer_5')
                     )
                     # tags$hr(),
                     # actionButton('add_layer',
                     #              label = 'Add layer',
                     #              type = "button", 
                     #              class = "btn btn-success"),
                     # 
                     # actionButton(ns('remove_layer'),
                     #              label = 'Clear layer',
                     #              type = "button", 
                     #              class = "btn btn-danger")
        ),
        mainPanel(width = 9,
                  plotOutput(ns('main_plot'), height = "650px"))
      )# End SidebarLayout
    )# End fluidRow
  )# End TabPanel
}

# ----- Plot Viewer module server function -----

plotViewerServer <- function(id, loaded_df) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      
      # ----- Populate charts option in the UI ----- 
      observeEvent(loaded_df(), {
        update_mappings_inputs(loaded_df(), session)
      })
      
      # ----- Update "Show more" button -----
      observeEvent(input$toggle_optional_mappings, {
        if (input$toggle_optional_mappings %% 2 != 0){
          updateActionButton(session, 'toggle_optional_mappings', label = 'Show less')
        } else {
          updateActionButton(session, 'toggle_optional_mappings', label = 'Show more')
        }
      })
      
      # Get the mappings from the user when interacting with UI
      aes_mappings <- reactive({
        aes_mappings <- possible_mappings %>%
          sapply(., function(val) input[[val]]) %>%
          purrr::discard(is.null) %>%
          purrr::discard(. == '') %>%
          purrr::discard(. == ' ') %>%
          purrr::map(., as.name) %>%
          purrr::set_names(., stringr::str_remove(names(.),
                                                  pattern = 'aes_'))
        print(aes_mappings)
        aes_mappings
      })
      
      # Don't show a plot if there's an error
      output$main_plot <- renderPlot({
        
        params <- list(na.rm = TRUE)
        if (input$stat == 'bin'){
          params <- c(params, bins = input$stat_param)
        }
        
        p <- tryCatch({
          ggplot(data = loaded_df()) +
            layer(mapping = do.call(aes, aes_mappings()),
                  geom = input$geom_type,
                  stat = input$stat,
                  position = input$position,
                  params = params)},
          error =  function(e) {print(e) ; ggplot() + geom_blank()})
        
        p})
      
    }
  )
}

