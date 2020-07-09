type2colnames <- function(df, x, col_names = names(df)){
  valid_types <- switch(x,
                        'geo' = c('double', 'numeric'),
                        'lat' = c('double', 'numeric'),
                        'lon' = c('double', 'numeric'),
                        'int' = c('integer'),
                        'dbl' = c('double'),
                        'numeric' = c('numeric', 'integer', 'double'),
                        'char' = c('character'),
                        'fct' = c('factor'),
                        'lgl' = c('logical'),
                        'date' = c('date', 'Date'),
                        NULL = c()
  )
  
  if (x %in% c('lat', 'lon')){
    p <- ifelse(x == 'lat', 'lat(itude)?', '(long?(itude)?|lng)')
    o <- sapply(col_names,
                function(x) ifelse(
                  grepl(p, x, ignore.case = TRUE) &
                    is(df[[x]])[1] %in% valid_types,
                  x,
                  NA),
                USE.NAMES = FALSE)
  } else {
    o <- sapply(col_names,
                function(x) ifelse(
                  is(df[[x]])[1] %in% valid_types,
                  x,
                  NA),
                USE.NAMES = FALSE)
  }
  o[!is.na(o)]
}


#' @title Get \code{tibble}-guessed data types for each dataframe column
#' 
#' @description Get all of \code{df} columns' data types and put them in
#'   a list usable to update the UI 
#' @param df Dataframe created from the loaded CSV file.
#' @return A list where each name is the \code{inputId} of an input in the
#'   UI, and each value is a vector of column names with the correct
#'   characteristics.
#'   @import magrittr
#'   @import dplyr
get_df_coltypes <- function(df){
  lat_cols <- df %>%
    select(matches(lat_pat, ignore.case = TRUE) & where(is.double)) %>%
    names()
  lon_cols <- df %>%
    select(matches(lon_pat, ignore.case = TRUE) & where(is.double)) %>%
    names()

  list('lat_cols' = lat_cols,
       'lon_cols' = lon_cols,
       'lgl_cols' = df %>%
         select(where(is.logical)) %>%
         names(),
       'int_cols' = df %>%
         select(where(is.integer)) %>%
         names(),
       'dbl_cols' = df %>%
         select(-lat_cols & -lon_cols & where(is.double)) %>%
         names(),
       'char_cols' = df %>%
         select(where(is.character)) %>%
         names(),
       'fct_cols' = df %>%
         select(where(is.factor)) %>%
         names(),
       'date_cols' = df %>%
         select(where(lubridate::is.Date)) %>%
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
