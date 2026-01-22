#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  
  # i18n <- golem::get_golem_options(which = "translator")
  # i18n <- shiny.i18n::Translator$new(translation_json_path = system.file("app/www/translations/", "translation.json", package="reseptilaakesovellus"))
  # i18n$set_translation_language("fi")
  # shiny.i18n::usei18n(i18n)
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("fi")
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = bslib::bs_theme(primary = "#0f73a9",
                              base_font = "Lato",
                              heading_font = "Noto Sans"),
      mod_01_meta_ui("01_meta_1"),
      mod_02_navi_ui("02_navi_1"),
      mod_03_app_ui("03_app_1"),
      mod_04_source_ui("04_source_1"),
      mod_05_accessibility_ui("05_accessibility_1"),
      tags$html(HTML('</main></div>'))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
      tags$style(HTML("
        
body {font-family: 'Lato', sans-serif;
  border-top: 5px solid #fdb916;
  border-bottom: 5px solid #fdb916;
  margin-bottom: 1rem;

}

.bg-kela {
  background-color:#f3f5f9!important}

.btn-outline-kela{color:#003580;border-color:#003580}

.shiny-plot-output {
  /*border: 1px solid rgba(0, 0, 0, 0.125);*/
margin-top: 20px;
margin-bottom: 20px;
height: auto;
border:3px solid #fff;
/*box-shadow: 0 4px 8px rgba(0,0,0,0.15);
-moz-box-shadow: 0 4px 8px rgba(0,0,0,0.15);
-webkit-box-shadow: 0 4px 8px rgba(0,0,0,0.15);
-khtml-box-shadow: 0 4px 8px rgba(0,0,0,0.15);*/
 
}



.btn-outline-default, .btn-default:not(.btn-primary, .btn-secondary, .btn-info, .btn-success, .btn-danger, .btn-warning, .btn-light, .btn-dark, .btn-link, [class*='btn-outline-']) {
  --bs-btn-hover-bg: #003580;
  --bs-btn-hover-color: #FFF;
}




a:focus  {
  border: 3px solid #393939;
}



/*

.selectize-input {
  border: 0px solid #ced4da !important;
  background-color: #003580 !important;
  color: #FFF;
  padding-top: 15px;
}

.form-control.selectize-control:focus-within  {
  
  border: 2px solid #000;
}

.accordion-button:focus {
  border: 3px solid #393939;
}

.form-control:focus {
  border: 3px solid #393939;
}

.form:focus {
  border: 3px solid #393939;
}
  
.selectize-control.single .selectize-input:not(.no-arrow)::after {
  content: '';
  display: block;
  position: absolute;
  top: 50%;
  right: calc(0.75rem + 5px);
  margin-top: 0px;
  width: 0;
  height: 0;
  border-style: solid;
  border-width: 7px 7px 0px 7px;
  border-color: #FFF transparent transparent transparent;
}

.selectize-dropdown .selected {
  background-color: #f0f5ff;
  color: #000;
}


*/


")),
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "reseptilaakesovellus"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    tags$link(rel="shortcut icon", href="https://www.kela.fi/kelafi-theme/images/favicon.ico"),
    tags$link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.7.1/font/bootstrap-icons.css"),
    tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.1/css/all.min.css"),
    tags$link(rel="stylesheet", href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"),
    tags$script(src="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js"), 
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/fancybox/3.5.7/jquery.fancybox.min.js"),
    tags$link(rel="stylesheet", href="www/custom.css")
  )
}
