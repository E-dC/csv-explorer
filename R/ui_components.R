ui_datatypes <- tagList(
  fluidRow(
    titlePanel('Select column types for each column.')
  ),
  # ----- Numeric types -----
  fluidRow(
    column(4, selectInput('int_cols',
                          label = 'Integer',
                          choices = choices,
                          multiple = TRUE)),
    column(4, selectInput('dbl_cols',
                          label = 'Float',
                          choices = choices,
                          multiple = TRUE)),
    column(4, selectInput('lgl_cols',
                          label = 'Logical',
                          choices = choices,
                          multiple = TRUE))),
  hr(),
  # ----- Text types -----
  fluidRow(
    column(4, selectInput('chr_cols',
                          label = 'Character',
                          choices = choices,
                          multiple = TRUE)),
    column(4, selectInput('fct_cols',
                          label = 'Factor',
                          choices = choices,
                          multiple = TRUE))),
  hr(),
  # ----- Date parsing -----
  fluidRow(
    column(4, selectInput('date_cols',
                          label = 'Date',
                          choices = choices,
                          multiple = TRUE)),
    column(4, selectInput('date_parsing',
                          label = 'Date parsing instructions',
                          choices = names(date_parsing_options),
                          selected = 'auto',
                          multiple = FALSE))),
  hr(),
  # ----- Lat/lon types -----
  fluidRow(
    column(4, selectInput('lat_cols',
                          label = 'Geo: Latitude',
                          choices = choices,
                          multiple = TRUE)),
    column(4, selectInput('lon_cols',
                          label = 'Geo: Longitude',
                          choices = choices,
                          multiple = TRUE))),
  # ----- Submit button -----
  fluidRow(column(5),
           column(2, actionButton('submit',
                                  'Submit',
                                  type = "button", 
                                  class = "btn btn-danger")),
           column(5))
)



ui_xy_mappings <- tagList(
  selectInput('aes_x',
              'X',
              choices = choices),
  selectInput('aes_y',
              'Y',
              choices = choices)
)

# ----- Optional mappings UI -----

ui_optional_mappings <- tagList(
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
)


ui_plot_options <- tagList(
  selectInput('geom_type',
              'Plot type',
              choices = c('None' = ' ',
                          'Bar' = 'bar',
                          'Point' = 'point',
                          'Line' = 'line'),
              selected = 'None',
              multiple = FALSE),
  selectInput('position',
              'Position',
              choices = c('None' = ' ',
                          'Identity' = 'identity',
                          'Stack' = 'stack',
                          'Dodge' = 'dodge',
                          'Jitter' = 'jitter'),
              selected = 'identity',
              multiple = FALSE),
  selectInput('stat',
              'Stat',
              choices = c('None' = ' ',
                          'Identity' = 'identity',
                          'Count' = 'count',
                          'Bin' = 'bin',
                          'Unique' = 'unique'),
              selected = 'identity',
              multiple = FALSE)
)

ui_stats_options <- tagList(
  sliderInput('stat_param',
              'Bins',
              value = 30,
              min = 1,
              max = 100,
              step = 1)
)

ui_current_layer <- tagList(
  tabsetPanel(
    # ----- Mappings UI -----
    tabPanel('Mappings',
             ui_xy_mappings,
             conditionalPanel('input.toggle_optional_mappings % 2 != 0',
                              ui_optional_mappings),
             actionLink('toggle_optional_mappings', label = 'Show more')),
    
    # ----- Geom, position, stat UI -----
    tabPanel('Plot',
             ui_plot_options),
    
    # ----- Stat parameters UI -----
    tabPanel('Stat',
             ui_stats_options)
  )
)

