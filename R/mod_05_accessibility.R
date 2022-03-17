#' 05_accessibility UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_05_accessibility_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    uiOutput(ns("ui_accessibility_statement"))
    ))
  )
}
    
#' 05_accessibility Server Functions
#'
#' @noRd 
mod_05_accessibility_server <- function(id, mod_in){
  moduleServer( id, function(input, output, session, mod_in_01 = mod_in){
    ns <- session$ns
 
    output$ui_accessibility_statement <- renderUI({
      
      req(mod_in_01$selected_language)
      
      input_selected_language <- mod_in_01$selected_language
      
      taglst <- create_accessibility_statement(lang = input_selected_language)
      tagList(
        taglst
      )
    })
    
    
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
