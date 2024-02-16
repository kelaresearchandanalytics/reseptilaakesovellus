#' helpers 
#'





#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 
#' @export
add_line_break <- function(x = "very many many characters and words and sentences",
                           n = 20){
  y <- gsub(paste0("(.{1,",n,"})(\\s|$)"), "\\1<br/>", x)
  y <- sub("<br/>$", "", y)
  return(y)
}

#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 
#' @export
add_line_break2 <- function(x = "very many many characters and words and sentences",
                            n = 20){
  y <- gsub(paste0("(.{1,",n,"})(\\s|$)"), "\\1\n", x)
  y <- sub("\n$", "", y)
  return(y)
}

#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 
#' @export
is.even <- function(x) x %% 2 == 0


#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 
#' @export
create_color_palette <- function(years){
  atc_color_palette <- c('#66cdaa','#0000ff','#ff8c00','#00ff00','#1e90ff','#ff1493')
  atc_year_range <- 2019:2024
  
  tmp <- tibble(col = atc_color_palette,
                year = atc_year_range)
  tmp[tmp$year %in% years,]$col
}

