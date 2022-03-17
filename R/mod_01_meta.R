#' 01_meta UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import metathis
#' @importFrom magrittr %>%
mod_01_meta_ui <- function(id){
  ns <- NS(id)
  tagList(
    meta() %>%
      meta_description(description = "Vertaa sairausvakuutuksesta korvattavien reseptilääkkeiden kustannuksia, ostomääriä ja ostajien määriä") %>%
      meta_social(
        title = "Reseptilääkkeiden ostot ATC-luokittain",
        description = "Vertaa sairausvakuutuksesta korvattavien reseptilääkkeiden kustannuksia, ostomääriä ja ostajien määriä",
        url = "https://kelaresearchandanalytics.shinyapps.io/korona_atc_app/",
        image = "https://kelaresearchandanalytics.shinyapps.io/korona_atc_app/og_img.png",
        image_alt = "An image for social media cards",
        twitter_creator = "@Kelantutkimus",
        twitter_card_type = "summary_large_image",
        twitter_site = "@Kelantutkimus"
      )
    ,
    includeHTML(path = system.file("inst", "app/www/piwik.txt", package = "reseptilaakesovellus"))
    
  )
}
    
#' 01_meta Server Functions
#'
#' @noRd 
mod_01_meta_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI

## To be copied in the server
# mod_01_meta_server("01_meta_1")
