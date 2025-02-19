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
    <header class="blog-header container_1280">
                <nav class="navbar navbar-kela bg-kela navbar-light sticky-top">
                     <div class="navbar-brand" role="brand">
      <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 272.6 132" xml:space="preserve" height="48" width="83"><circle cx="242.9" cy="29.8" r="29.1" fill="#fdb913"></circle><path d="M226.8 7.8h5.8v2.6h7.2V7.8h6v2.7h7.2V7.8h5.7l3.8 41.3h-12.6s-.9-10-.9-9.6c.1.3-.9-5.6-6.5-5.2-5.9 0-6.2 5.8-6.2 5.8l-.8 9H223l3.8-41.3z" fill="#fff"></path><circle cx="242.7" cy="22.4" r="6.2" fill="#fdb913"></circle><path d="m43.8 130.1-26-44.6v44.6H.2V44.6h17.6v39l24.4-39h21.2L36 84.2l30.4 45.9H43.8zm40.4-28.3v.5c0 9.7 4.8 15.3 13.3 15.3 5.7 0 10.9-2.1 16-6.3l6.4 9.8c-7.3 5.9-14.9 8.7-23.7 8.7-18.1 0-29.8-12.8-29.8-32.6 0-11.3 2.3-18.8 7.9-25 5.2-5.8 11.4-8.5 19.8-8.5 7.3 0 14.1 2.5 18.2 6.6 5.8 5.9 8.4 14.4 8.4 27.6v3.8H84.2zM103.6 89c0-4.7-.5-7.1-2-9.5-1.6-2.5-3.9-3.7-7.3-3.7-6.3 0-9.8 4.9-9.8 13.7v.2h19.1V89zm44.9 40.6c-7 0-12.7-3.3-14.6-8.6-1.2-3.2-1.5-5.2-1.5-14.1v-47c0-8.2-.2-13.3-.9-18.9l16.9-3.8c.6 3.4.9 7.5.9 16.4v49.1c0 10.8.1 12.3 1.1 14 .6 1.1 2 1.7 3.3 1.7.6 0 1 0 1.8-.2l2.8 9.8c-2.8.9-6.3 1.6-9.8 1.6zm59.6 2.3c-3.8-1.6-6.9-5-8.5-8.2-1.2 1.2-2.6 2.5-3.8 3.3-3.1 2.2-7.5 3.4-12.7 3.4-14 0-21.6-7.1-21.6-19.7 0-14.8 10.2-21.6 30.3-21.6 1.2 0 2.3 0 3.7.1v-2.6c0-7-1.4-9.3-7.4-9.3-5.3 0-11.4 2.6-18.2 7.1l-7-11.8c3.3-2.1 5.8-3.3 10.2-5.2 6.1-2.6 11.4-3.7 17.2-3.7 10.6 0 17.8 3.9 20.3 10.9.9 2.6 1.2 4.6 1.1 11.3l-.4 21.2c-.1 6.9.4 9.8 5.9 14l-9.1 10.8zm-13.6-31.4c-11.4 0-15.4 2.1-15.4 9.6 0 4.9 3.1 8.2 7.3 8.2 3.1 0 6.2-1.6 8.6-4.3l.2-13.5h-.7z" fill="#004895"></path></svg>
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
