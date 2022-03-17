#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  
  mod_03_app_res <- mod_03_app_server("03_app_1")
  mod_04_source_server("04_source_1", mod_in = mod_03_app_res)
  mod_05_accessibility_server("05_accessibility_1", mod_in = mod_03_app_res)
  mod_02_navi_server("02_navi_1", mod_in = mod_03_app_res)

}
