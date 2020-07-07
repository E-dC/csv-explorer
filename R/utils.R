type2colnames <- function(df, x, col_names = names(df)){
  valid_types <- switch(x,
                        'geo' = c('double', 'numeric'),
                        'lat' = c('double', 'numeric'),
                        'lon' = c('double', 'numeric'),
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

update_type_inputs <- function(df, session){
  choices <- names(df)
  
  lat <- type2colnames(df, 'lat')
  
  updateSelectInput(session,
                    'lat_cols',
                    choices = choices,
                    selected = lat)
  
  lon <- type2colnames(df, 'lon')
  # print(lon)
  updateSelectInput(session,
                    'lon_cols',
                    choices = choices,
                    selected = lon)
  
  col_names <- choices[! choices %in% c(lat, lon)]
  
  updateSelectInput(session,
                    'real_cols',
                    choices = choices,
                    selected = type2colnames(df, 'numeric', col_names))
  
  updateSelectInput(session,
                    'char_cols',
                    choices = choices,
                    selected = type2colnames(df, 'char', col_names))
  
  updateSelectInput(session,
                    'factor_cols',
                    choices = choices,
                    selected = type2colnames(df, 'fct', col_names))
  
  updateSelectInput(session,
                    'lgl_cols',
                    choices = choices,
                    selected = type2colnames(df, 'lgl', col_names))
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
