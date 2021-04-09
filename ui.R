source("./global.R")

##  KÄYTTÖLIITTYMÄ ----


i18n <- Translator$new(translation_json_path='translations/translation.json')
i18n$set_translation_language('fi')


shinyUI(fluidPage(lang = "fi",
          title = "Reseptilääkkeiden ostot ATC-luokittain",
          shiny.i18n::usei18n(i18n),
          theme = bslib::bs_theme(),
          includeCSS("www/styles.css"),
          
          uiOutput("ui_navigation"),
          
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
            column(4, 
                   uiOutput("inputs_region_level"))#,
            # column(3,
            # uiOutput("ui_language_selection"))
          ),
          # fluidRow(
          # column(9,   uiOutput("inputs_varname")),
          # column(3,   uiOutput("ui_bookmark"))
          # ),
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
                          ))
))