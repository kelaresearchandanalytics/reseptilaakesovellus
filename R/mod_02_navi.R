#' 02_navi UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_02_navi_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      a {
      color: #0f73a9
      }
          
      .container_1280 {
        max-width: 1280px; 
        margin: auto;
        }")),
    HTML('<a class="sr-only sr-only-focusable" href="#maincontent">Skip to main</a>
    <header class="blog-header sticky-top container_1280">
                <nav class="navbar navbar-kela bg-kela navbar-light sticky-top">
                     <div class="navbar-brand" role="brand">
      <img src = "https://www.kela.fi/image/layout_set_logo?img_id=2174196&t=1585229282595" style = "height: 35px; padding-right: 0px;" alt = "Kelan logo">
      </div>
      <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarResponsive" aria-controls="navbarResponsive" aria-expanded="false" aria-label="Avaa valikko">
        <span class="navbar-toggler-icon"></span>
      </button>
      <div role = "navigation" class="collapse navbar-collapse justify-content-between" id="navbarResponsive">
               <ul></ul>'),
    uiOutput(ns("ui_navigation_links")),
    HTML('</div>
    </nav>
  </header>')
    
  )
}
    
#' 02_navi Server Functions
#'
#' @noRd 
mod_02_navi_server <- function(id, mod_in){
  moduleServer( id, function(input, output, session, mod_in_01 = mod_in){
    ns <- session$ns
 
    create_navigation <- function(lang){
      
      
      if (lang == "fi"){
        
        taglst <-  tagList(
          HTML('
         <ul class="navbar-nav ml-auto">
           <li class="nav-item">
             <a class="nav-link" href="#ohje">Ohjeet</a>
           </li>
           <li class="nav-item">
             <a class="nav-link" href="#info">Lähdekoodi</a>
           </li>
           <li class="nav-item">
             <a class="nav-link" href="#saavutettavuus">Saavutettavuusseloste</a>
           </li>
        </ul>')
        )
      } else if (lang == "en"){
        
        taglst <-  tagList(
          HTML('
        <ul class="navbar-nav ml-auto">
          <li class="nav-item">
            <a class="nav-link" href="#ohjeet">Instructions</a>
          </li>
          <li class="nav-item">
            <a class="nav-link" href="#info">Source code</a>
          </li>
          <li class="nav-item">
            <a class="nav-link" href="#saavutettavuus">Accessibility </a>
          </li>
        </ul>')
        )
      } else if (lang == "sv"){
        
        taglst <-  tagList(
          HTML('<ul class="navbar-nav ml-auto">
          <li class="nav-item">
            <a class="nav-link" href="#ohje">Instruktioner</a>
          </li>
          <li class="nav-item">
            <a class="nav-link" href="#info">Källkod</a>
          </li>
          <li class="nav-item">
            <a class="nav-link" href="#saavutettavuus">Tillgänglighet</a>
          </li>
        </ul>')      )
      }
      
      return(taglst)
    }
    
    output$ui_navigation_links <- renderUI({
      
      req(mod_in_01$selected_language)
      
      input_selected_language <- mod_in_01$selected_language

      taglst <- create_navigation(lang = input_selected_language)
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
