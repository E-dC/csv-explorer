choices <- list()
date_parsing_functions <- list("dmy" = dmy,
                               "dmy_h" = dmy_h,
                               "dmy_hm" = dmy_hm,
                               "dmy_hms" = dmy_hms,
                               "dym" = dym,
                               "mdy" = mdy,
                               "mdy_h" = mdy_h,
                               "mdy_hm" = mdy_hm,
                               "mdy_hms" = mdy_hms,
                               "myd" = myd,
                               "ydm" = ydm,
                               "ydm_h" = ydm_h,
                               "ydm_hm" = ydm_hm,
                               "ydm_hms" = ydm_hms,
                               "ymd" = ymd,
                               "ymd_h" = ymd_h,
                               "ymd_hm" = ymd_hm,
                               "ymd_hms" = ymd_hms)
date_parsing_options <- c("auto" = function(x) return(x), date_parsing_functions)

possible_mappings <- c('aes_x', 'aes_y', 'aes_alpha', 'aes_fill', 'aes_colour', 'aes_size', 'aes_linetype', 'aes_group')


lat_pat <- 'lat(itude)?'
lon_pat <- '(long?(itude)?|lng)'
date_pat <- '(\\bdate\\b|date$|\\bdatetime\\b|datetime$)'