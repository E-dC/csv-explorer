#' Attempt to find the correct date parser for a vector of character dates
#' 
#' @description Check 5% of all rows in a date column so as to find the correct
#'   order for the parser (ymd, dmy, mdy, etc.). Then we can just use that instead
#'   of a more permissive but potentially slower parser.
#' 
#' @param x A list or character vector of dates to analyse
#' @return The function to use for parsing 
find_parser <- function(x){
  wrapper <- function(f){
    v <- f(frac)
    return (length(v[!is.na(v)]))
  }
  
  if (! is.null(x)){
    x <- x[!is.na(x)]
    total <- length(x)
    n <- ifelse(total < 50, total, round(0.05 * length(x)))
    frac <- sample(x, n)

    r <- suppressWarnings(sapply(date_parsing_functions, wrapper))
    top <- sort(r, decreasing = TRUE)[1]

    if (top > (0.5 * n)){
      return (do.call(switch, c(names(top), date_parsing_functions)))
    }
  }
  return (function (x) return (x))

}

#' @title Get \code{tibble}-guessed data types for each dataframe column
#' 
#' @description Get all of \code{df} columns' data types and put them in
#'   a list usable to update the UI 
#' @param df Dataframe created from the loaded CSV file.
#' @return A list where each name is the \code{inputId} of an input in the
#'   UI, and each value is a vector of column names with the correct
#'   characteristics.
#' @import magrittr
#' @import dplyr
#' @import tidyr
get_df_coltypes <- function(df){
  lat_cols <- df %>%
    select(matches(lat_pat, ignore.case = TRUE) & where(is.double)) %>%
    names()
  lon_cols <- df %>%
    select(matches(lon_pat, ignore.case = TRUE) & where(is.double)) %>%
    names()
  date_cols <- df %>%
    select(matches(date_pat, ignore.case = TRUE) | where(lubridate::is.Date)) %>%
    names()
  
  list('lat_cols' = lat_cols,
       'lon_cols' = lon_cols,
       'date_cols' = date_cols,
       'lgl_cols' = df %>%
         select(where(is.logical)) %>%
         names(),
       'int_cols' = df %>%
         select(where(is.integer)) %>%
         names(),
       'dbl_cols' = df %>%
         select(-lat_cols & -lon_cols & where(is.double)) %>%
         names(),
       'chr_cols' = df %>%
         select(-date_cols & where(is.character)) %>%
         names(),
       'fct_cols' = df %>%
         select(-date_cols & where(is.factor)) %>%
         names()
         )
}

#' @title Update type controls in the UI
#' 
#' @description  When a new CSV file is loaded, update the select list input controls
#'   on the starting UI tab
#'   
#' @param df Dataframe created from the loaded CSV file.
#' @param session The session object passed to function given to shinyServer.
#' @return Nothing, we only want a side effect here 
#' @import shiny
update_type_inputs <- function(df, session){
  choices <- names(df)
  col_types <- get_df_coltypes(df)
  
  mapply(function(input_ref, selected) {
    updateSelectInput(session = session,
                      inputId = input_ref,
                      choices = choices,
                      selected = selected)},
    input_ref = names(col_types),
    selected = col_types)
}

#' @title Update variables drop-downs in the charts UI 
#' 
#' @description  When a dataframe has been type-checked from the loaded CSV
#'   file, update the select list aesthetics mappings controls on the charts UI tab.
#'   
#' @param df Dataframe created from the loaded CSV file.
#' @param session The session object passed to function given to shinyServer.
#' @return Nothing, we only want a side effect here 
#' @import shiny
#' @export
update_mappings_inputs <- function(df, session){
  choices <- c("No mapping" = ' ', names(df))
  selected = "No mapping"
  
  input_refs <- c('aes_x', 'aes_y',
                  'aes_fill', 'aes_colour',
                  'aes_linetype', 'aes_size',
                  'aes_group', 'aes_alpha')
  
  mapply(function(input_ref, selected) {
    updateSelectInput(session = session,
                      inputId = input_ref,
                      choices = choices,
                      selected = selected)},
    input_ref = input_refs,
    selected = selected)
}


#' @param text Character
#' @param search Character. Will be passed to stringr::regex, with ignore_case = TRUE
#' @return Highlighted HTML
highlight_search <- function(text, search){
  replacer <- function(x){
    as.character(span(class = 'highlight-search', x))
  }
  if (search != ''){
    tryCatch({
      HTML(stringr::str_replace_all(text,
                                    stringr::regex(search, ignore_case = TRUE),
                                    replacer))
    }, error = function(e) text)
  } else {
    return (HTML(text))
  }
}

#' @import dplyr, tidyr, maggrittr
list_df_coltypes <- function(df){
  list('date_cols' = df %>%
         select(where(is.Date)) %>%
         names(),
       'lgl_cols' = df %>%
         select(where(is.logical)) %>%
         names(),
       'int_cols' = df %>%
         select(where(is.integer)) %>%
         names(),
       'dbl_cols' = df %>%
         select(where(is.double)) %>%
         names(),
       'chr_cols' = df %>%
         select(where(is.character)) %>%
         names(),
       'fct_cols' = df %>%
         select(where(is.factor)) %>%
         names()
  )
}

#' @import shiny
barplot_ui <- function(coltypes){
  all_coltypes <- unlist(coltypes, use.names = FALSE)
  categorical <- unlist(coltypes[grepl('(char|chr|fct|lgl|date)',
                                       names(coltypes))],
                        use.names = FALSE)
  numerical <- unlist(coltypes[grepl('(int|dbl)',
                                     names(coltypes))],
                      use.names = FALSE)
  
  list(
      selectInput('aes_x',
                  label = 'Variable: x',
                  choices = categorical),
      selectInput('aes_y',
                  label = 'Variable: y',
                  choices = c(NULL, all_coltypes)),
      selectInput('aes_alpha',
                  label = 'Variable: transparency',
                  choices = c('', numerical)),
      selectInput('aes_fill',
                  label = 'Variable: fill colour',
                  choices = c('', all_coltypes)),
      selectInput('aes_colour',
                  label = 'Variable: outline colour',
                  choices = c('', all_coltypes))
  )
}

