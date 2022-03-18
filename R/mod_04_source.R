#' 04_source UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_04_source_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    uiOutput(ns("ui_about_app")),
                    tags$hr()
    ))
  )
}
    
#' 04_source Server Functions
#'
#' @noRd 
mod_04_source_server <- function(id, mod_in){
  moduleServer( id, function(input, output, session, mod_in_01 = mod_in){
    ns <- session$ns
    
    output$ui_about_app <- renderUI({
      
      req(mod_in_01$selected_language)
      
      input_selected_language <- mod_in_01$selected_language
      
      
      taglst01 <- create_about_app(lang = input_selected_language)
      # viittaaminen sovellukseen
      taglst02 <- HTML(glue('
      <!--div class="alert alert-success" role="alert"-->
                       <strong>Näin viittaat sovellukseen</strong>
                       <div style = "margin-left:30px;">
                       <em>{attr(unclass(citation("reseptilaakesovellus"))[[1]],"textVersion")}
                            <br/>Accessed: {Sys.Date()}
                       </em>
                       <div>
                       <!--/div-->
                       '))
      
      
      tagList(
        taglst01,
        taglst02
      )
    })
    
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
