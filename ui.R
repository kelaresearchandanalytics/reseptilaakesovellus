source("./global.R")

##  KÄYTTÖLIITTYMÄ ----


i18n <- Translator$new(translation_json_path='translations/translation.json')
i18n$set_translation_language('fi')


shinyUI(fluidPage(lang = "fi",
          title = "Reseptilääkkeiden ostot ATC-luokittain",
          tags$head(tags$link(rel="shortcut icon", href="https://www.kela.fi/kelafi-theme/images/favicon.ico"),
                    tags$link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.7.1/font/bootstrap-icons.css"),
                    tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.1/css/all.min.css"),
                    tags$link(rel="stylesheet", href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"),
                    tags$link(rel="stylesheet", href="https://laaketieto.kela.fi/css/style.css"),
                    tags$script(src="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js"), 
                    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/fancybox/3.5.7/jquery.fancybox.min.js")#, 
                    # tags$script(src="https://code.jquery.com/jquery-3.5.1.min.js")
          ),
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
            ),
          tags$style(HTML("
      .container_1280 {
        max-width: 1280px; 
        margin: auto;
        }")),
          shiny.i18n::usei18n(i18n),
          theme = bslib::bs_theme(primary = "#0f73a9"),
          if (file.exists("www/piwik_script.txt")){
            includeHTML(path = "www/piwik_script.txt")
          },

          tags$html(HTML('<a class="sr-only sr-only-focusable" href="#maincontent">Skip to main</a>')),

          uiOutput("ui_navigation"),
          tags$html(HTML('<main id="maincontent">')),
          tags$html(HTML('<div class="container_1280">')),

          # Application title
          fluidRow(column(width = 6,
                          uiOutput("ui_info_text")),
                   column(width = 6,
                          uiOutput("ui_language_selection"),
                          uiOutput("ui_atc_box"))),
          uiOutput("data_updated"),
          tags$hr(),
          fluidRow(
            column(3, uiOutput("inputs_search_type")),
            column(5, uiOutput("inputs_search_box"),
                   uiOutput("inputs_atc_search"),
                   uiOutput("inputs_atc1"),
                   uiOutput("inputs_atc2"),
                   uiOutput("inputs_atc3"),
                   uiOutput("inputs_atc4"),
                   uiOutput("inputs_atc5")),
            column(2,
                   uiOutput("inputs_region_level")),
            column(2,
                   uiOutput("ui_bookmark")),

          ),
          fluidRow(
            column(width = 4,
                   uiOutput("inputs_varname")),
            column(width = 2,
                   uiOutput("ui_download_csv")),
            column(width = 2,
                   uiOutput("ui_download_pdf")),
            column(width = 2,
                   uiOutput("ui_download_png")),
            column(width = 2,
                   uiOutput("ui_download_svg"))
          ),
          tags$hr(),
          shinycssloaders::withSpinner(uiOutput("plot_main_ui")),
          tags$hr(),
          fluidRow(column(8,
                          uiOutput("ui_about_app"),
                          tags$hr(),
                          uiOutput("ui_accessibility_statement")
                          )),
          tags$html(HTML('</main></div>'))
))
