#' 03_app UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList 
#' @import dplyr
#' @import tidyr
#' @import shinyWidgets
#' @import glue
#' @import hrbrthemes
#' @import geofacet
#' @import ggplot2
#' @import patchwork
#' @import shiny.i18n
#' 
mod_03_app_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    HTML('<main id="maincontent">'),
    HTML('<div class="container_1280">'),
    
    # VUODENVAIHDENOOTTI
    
          HTML('
<div class="row alert alert-info" role="alert">
<div class = "col-1">
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1000 1000" xml:space="preserve" width="3em" height="3em" name="IconBellCircle" class="mt-1 mb-2 icon-size-m"><path d="M503.4.6C227.6-1.3 2.4 220.8.6 496.6s220.2 501 496 502.9 501-220.2 502.9-496S779.3 2.4 503.4.6zm455.9 502.5C957.6 756.8 750.5 961 496.9 959.3S39 750.5 40.7 496.9C42.4 243.2 249.5 39 503.1 40.7S961 249.5 959.3 503.1z" fill="currentColor"></path><path d="m722.1 637.9 1.5-197.9c.7-100.2-65.3-185.4-156.7-213.4l.3-34.8c0-36.5-29.3-66.2-65.6-66.2s-65.9 29.3-66 65.5l-.3 36.5c-88.9 29-154.2 114-154.9 213.3l-1.4 197c-31.2 4.7-55 31.5-55 64 0 35.9 28.9 64.8 64.8 64.8h118.9c9 42.5 46.6 74.3 91.8 74.7 45.8.4 84.1-31.7 93.3-74.7h118.9c35.6 0 64.6-29 65.2-64.8-.1-32.4-23.7-59.2-54.8-64zM475.8 191.4c0-14.1 11.6-25.6 25.8-25.6 13.9 0 25.4 11.6 25.4 25.8l-.2 26.8c-6.3-.7-12.6-1.2-19-1.3-10.9-.3-21.7.3-32.3 1.6l.3-27.3zM320.6 441.2c.7-102.6 85.3-186.6 186.2-184 98.6 2.6 177.4 83.7 176.6 182.5l-1.5 197H319.1l1.5-195.5zm179.2 360.1c-23-.2-42.5-14.5-50.3-34.6H551c-8.1 20.4-28 34.8-51.2 34.6zm211.9-74.8h-282c-1.2-.2-2.5-.4-3.8-.4-1.4 0-2.8.1-4.2.4h-133c-13.7 0-24.6-10.9-24.6-24.6 0-13.7 10.9-24.6 24.6-24.6H712c13.6 0 24.5 10.8 24.6 24.4-.2 13.7-11.5 24.8-24.9 24.8z" fill="currentColor"></path></svg>
</div>
<div class = "col-9">
  <strong>Aluejako muuttunut!</strong> 15.5.2023 alkaen sairaanhoitopiirit on korvattu hyvinvointialueilla!<br/>
  <strong>Regionindelning ändrad!</strong> Från 15.5.2023 ersätts sjukvårdsdistrikt med välfärdsområder!<br/>
  <strong>Regional subdivision changed!</strong> From 15.5.2023 hospital districts are replaced with well-being srvice counties!
  </div>
               </div>'),
    
    
    
    # Application title
    fluidRow(column(width = 6,
                    uiOutput(ns("ui_info_text"))),
             column(width = 6,
                    uiOutput(ns("ui_language_selection")),
                    uiOutput(ns("ui_atc_box")))),
    uiOutput(ns("data_updated")),
    tags$hr(),
    fluidRow(
      column(3, uiOutput(ns("inputs_search_type"))),
      column(5, uiOutput(ns("inputs_search_box")),
             uiOutput(ns("inputs_atc_search")),
             uiOutput(ns("inputs_atc1")),
             uiOutput(ns("inputs_atc2")),
             uiOutput(ns("inputs_atc3")),
             uiOutput(ns("inputs_atc4")),
             uiOutput(ns("inputs_atc5"))),
      column(2,
             uiOutput(ns("inputs_region_level"))),
      column(2,
             uiOutput(ns("ui_bookmark"))),

    ),
    fluidRow(
      column(width = 4,
             uiOutput(ns("inputs_varname")),
             uiOutput(ns("inputs_action_button"))
             ),
      column(width = 2,
             uiOutput(ns("ui_download_csv"))
             ),
      column(width = 2,
             uiOutput(ns("ui_download_pdf"))
             ),
      column(width = 2,
             uiOutput(ns("ui_download_png"))
             ),
      column(width = 2,
             uiOutput(ns("ui_download_svg"))
             )
    ),
    tags$hr(),
    shinycssloaders::withSpinner(uiOutput(ns("plot_main_ui"))),
    tags$hr()
  )
}
    
#' 03_app Server Functions
#'
#' @noRd 
mod_03_app_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # 
    

    i18n <- golem::get_golem_options(which = "translator")
    i18n$set_translation_language("fi")

    # keep track of language object as a reactive
    i18n_r <- reactive({
      i18n
    })

    # change language
    observeEvent(input[["selected_language"]], {
      shiny.i18n::update_lang(session = session, language = input[["selected_language"]])
      i18n_r()$set_translation_language(input[["selected_language"]])
    })
    
    
    ### inputit ----
    output$ui_language_selection <- renderUI({
      tagList(
        radioButtons(inputId = ns('selected_language'),
                     label = "Valitse kieli / Select language / Välj språk",
                     inline = TRUE,
                     choices = i18n$get_languages(),
                     selected = input$selected_language)
      )
    })
    
    
    output$ui_atc_box <- renderUI({
      
      req(input$selected_language)
      
      taglst <- create_atc_box(lang = input$selected_language)
      tagList(
        taglst
      )
    })
    
    output$ui_info_text <- renderUI({
      
      req(input$selected_language)
      
      taglst <- create_info_text(lang = input$selected_language)
      
      tagList(
        taglst
      )
    })
    
    output$ui_navigation_links <- renderUI({
      
      req(input$selected_language)
      
      taglst <- create_navigation(lang = input$selected_language)
      tagList(
        taglst
      )
    })
    
    
    output$ui_text_title <- renderText({
      req(input$selected_language)
      i18n$t("Reseptilääkkeiden ostot ATC-luokittain")
    })
    
    output$ui_text_sidebartitle <- renderUI({
      tagList(
        tags$p(i18n$t("Valitse sivu"))
      )
    })
    
    ### datafunktiot ----
    create_metadata <- reactive({
      
      if (get_golem_config("offline_data")){
        load(system.file("data", "metadata_viikko.rda", package="reseptilaakesovellus"))
        names(metadata_viikko) <- tolower(names(metadata_viikko))
        df2 <- metadata_viikko
      } else {
      # data_viikko <- arrow::read_parquet("./inst/parquet/data_viikko.parquet")  
      # df2 <- readr::read_csv2("https://raw.githubusercontent.com/kelaresearchandanalytics/korona_atc_data/master/metadata_viikko_hva.csv")
        df2 <- arrow::read_parquet("https://raw.githubusercontent.com/kelaresearchandanalytics/korona_atc_data/master/metadata_viikko_hva.parquet")
      # df2 <- readr::read_csv2("~/tutkimus/laaketutkimus/korona_atc_data/metadata_viikko_hva.csv")
      }
      names(df2) <- tolower(names(df2))
      return(df2)
    })
    
    
    output$data_date <- renderText({
      meta <- create_metadata()
      as.character(unique(meta$updated))
    })
    
    create_data <- reactive({

      # load(system.file("data", "data_viikko.rda", package="reseptilaakesovellus"))
      data_viikko <- arrow::read_parquet("./inst/parquet/data_viikko.parquet")
      
      if (get_golem_config("offline_data")){
      # if (golem::get_golem_options(which = "offline_data")){
        names(data_viikko) <- tolower(names(data_viikko))
        df2 <- data_viikko
        
      } else {
      df6 <-   data_viikko[data_viikko$VUOSI != 2024,]
      # df5 <- readr::read_csv2("https://raw.githubusercontent.com/kelaresearchandanalytics/korona_atc_data/master/data_viikko_hva_2024.csv")
      df5 <- arrow::read_parquet("https://raw.githubusercontent.com/kelaresearchandanalytics/korona_atc_data/master/data_viikko_hva_2024.parquet")
      # df5 <- readr::read_csv2("~/tutkimus/laaketutkimus/korona_atc_data/data_viikko_hva_2023.csv")
      # df4 <- readr::read_csv2("https://raw.githubusercontent.com/kelaresearchandanalytics/korona_atc_data/master/data_viikko_2021.csv")
      # df3 <- readr::read_csv2("https://raw.githubusercontent.com/kelaresearchandanalytics/korona_atc_data/master/data_viikko_2020.csv")
      # df1 <- readr::read_csv2("https://raw.githubusercontent.com/kelaresearchandanalytics/korona_atc_data/master/data_viikko_2019.csv")
      # df2 <- bind_rows(df1,df3,df4,df5)
      df2 <- bind_rows(df6,df5 %>% select(-UPDATED))
      names(df2) <- tolower(names(df2))
      }
      
      if (input$selected_language == "fi") df2$atc_selite <- df2$atc_selite_fi
      if (input$selected_language == "sv") df2$atc_selite <- df2$atc_selite_sv
      if (input$selected_language == "en") df2$atc_selite <- df2$atc_selite_en
      
      if (input$selected_language == "fi") df2$aluenimi <- df2$aluenimi_fi
      if (input$selected_language == "sv") df2$aluenimi <- df2$aluenimi_sv
      if (input$selected_language == "en") df2$aluenimi <- df2$aluenimi_en
      
      datalist <- list()
      # datalist[["data"]] <- df2
      datalist[["atc"]] <- df2 %>%
        # ei erotellan atc-selitteen mukaan, koska siinä tulee mukana useita arvoja yhdelle atc-koodille
        distinct(atc_koodi, atc_taso, .keep_all = TRUE) %>%
        select(atc_koodi, atc_selite, atc_taso) %>%
        na.omit()
      datalist[["vuosi"]] <- unique(df2$vuosi)
      
      
      # df2$arvo <- round(df2[[input$value_var]]/1000)
      df3 <- df2 %>%
        pivot_longer(names_to = "variable", values_to = "arvo", cols = tidyselect::starts_with("var"))
      if (input$value_region == "Koko Suomi"){
        datalist[["data"]] <- df3 %>% dplyr::filter(aluekoodi %in% 99)
      } else {
        datalist[["data"]] <- df3 %>% dplyr::filter(!aluekoodi %in% 99)
      }
      return(datalist)
    })
    
    ### hakuinputit ----
    
    output$inputs_search_type <- renderUI({
      
      req(input$selected_language)
      
      
      search_choices <- c("valikkohaku","tekstihaku")
      names(search_choices) <- c(i18n$t("valikkohaku"),
                                 i18n$t("tekstihaku"))
      
      tagList(
        radioButtons(inputId = ns("value_search_type"),
                     label = i18n$t("Valitse hakutyyppi"),
                     choices = search_choices,
                     selected = search_choices[1])
      )
    })
    
    
    output$inputs_search_box <- renderUI({
      
      req(input$selected_language)
      req(input$value_search_type)
      
      if (input$value_search_type == "tekstihaku"){
        
        tagList(
          textInput(inputId = ns("value_atc_search_string"),
                    width = "100%",
                    label = i18n$t("Rajaa ATC-luokka nimen/koodin perusteella (erota hakuehdot |-merkillä)"),
                    value ="R03|A04")
        )
        
      } else {
        tagList()
      }
    })
    
    
    
    # observeEvent(input$selected_language, {
    #   updateActionButton("nappula", 
    #                      session = session,
    #                      label = i18n$t("Päivitä kuva")
    #   )
    # })
    
    output$inputs_action_button <- renderUI({
      
      req(input$selected_language)
      tagList(
        actionButton(inputId = ns("nappula"), 
                     label = i18n$t("Päivitä kuva"), 
                     icon("fas fa-sync")
        )
      )
    })
    
    
    
    output$inputs_atc_search <- renderUI({
      
      req(input$selected_language)
      req(input$value_atc_search_string)
      
      if (input$value_search_type == "tekstihaku"){
        datalist <- create_data()
        if (input$value_region == "Koko Suomi"){
          tmp1 <- datalist$atc #%>% dplyr::filter(atc_taso == 1)
          values1 <- tmp1$atc_koodi
          names(values1) <- tmp1$atc_selite
        } else {
          # SHP vaan taso 2 ja ylemmät
          tmp1 <- datalist$atc %>% dplyr::filter(atc_taso <= 2)
          values1 <- tmp1$atc_koodi
          names(values1) <- tmp1$atc_selite
        }
        
        
        
        if (is.null(input$value_atc_search_string)){
          # values2_sel <-
          values2 <- values1[1]
          # values2_sel <- values2
        } else {
          values2 <- values1[grepl(input$value_atc_search_string, names(values1), ignore.case = TRUE, perl = TRUE)]
          # values2_sel <- values2[1]
        }
        
        tagList(
          # selectInput("value_atc_1", "ATC-taso 1", choices = values1, selected = NA)
          pickerInput(inputId = ns("value_atc_search"),
                      label = i18n$t("Ja valitse kuvan/taulukon ATC-luokat hakutuloksista"),
                      choices = values2,
                      width = "100%",
                      # selected = values2_sel,
                      options = pickerOptions(
                        actionsBox = TRUE,
                        # liveSearchPlaceholder = "R03",
                        # liveSearch = TRUE,
                        size = 10,
                        deselectAllText = i18n$t("Ei mitään"),
                        selectAllText = i18n$t("Kaikki"),
                        noneSelectedText = i18n$t("Ei yhtään valittuna")
                      ),
                      multiple = TRUE)
        )
      } else {
        
        tagList()
        
      }
    })
    
    output$inputs_atc1 <- renderUI({
      
      req(input$selected_language)
      req(input$value_search_type)
      
      
      if (input$value_search_type == "valikkohaku"){
        datalist <- create_data()
        tmp1 <- datalist$atc %>% dplyr::filter(atc_taso == 1)
        values1 <- tmp1$atc_koodi
        names(values1) <- tmp1$atc_selite
        
        tagList(
          # selectInput("value_atc_1", "ATC-taso 1", choices = values1, selected = NA)
          pickerInput(inputId = ns("value_atc_1"),
                      label = i18n$t("Valitse ATC-luokka taso 1"),
                      choices = values1,
                      width = "100%",
                      # selected = kela,
                      options = pickerOptions(
                        actionsBox = TRUE,#liveSearch = TRUE,
                        size = 10,
                        deselectAllText = i18n$t("Ei mitään"),
                        selectAllText = i18n$t("Kaikki"),
                        noneSelectedText = i18n$t("Ei yhtään valittuna")
                      ),
                      multiple = TRUE)
        )
      } else {
        tagList()
      }
    })
    
    output$inputs_atc2 <- renderUI({
      
      req(input$selected_language)
      req(input$value_atc_1)
      
      if (is.null(input$value_atc_1)){
        tagList()
      } else {
        
        datalist <- create_data()
        tmp1 <- datalist$atc %>%
          filter(atc_taso == 2,
                 grepl(glue("^{paste(input$value_atc_1, collapse = '|')}"), atc_koodi)
                 # grepl("A", atc_koodi)
          )
        values1 <- tmp1$atc_koodi
        names(values1) <- tmp1$atc_selite
        
        tagList(
          # selectInput("value_atc_1", "ATC-luokka taso 1", choices = values1, selected = NA)
          pickerInput(inputId = ns("value_atc_2"),
                      label = i18n$t("Valitse ATC-luokka taso 2"),
                      choices = values1,
                      width = "100%",
                      # selected = kela,
                      options = pickerOptions(
                        actionsBox = TRUE,#liveSearch = TRUE,
                        size = 10,
                        deselectAllText = i18n$t("Ei mitään"),
                        selectAllText = i18n$t("Kaikki"),
                        noneSelectedText = i18n$t("Ei yhtään valittuna")
                      ),
                      multiple = TRUE)
        )
      }
    })
    
    output$inputs_atc3 <- renderUI({
      
      req(input$selected_language)
      req(input$value_atc_2)
      
      if (is.null(input$value_atc_1) | input$value_region != "Koko Suomi"){
        tagList()
      } else if (is.null(input$value_atc_2)) {
        tagList()
      } else {
        
        datalist <- create_data()
        tmp1 <- datalist$atc %>%
          filter(atc_taso == 3,
                 grepl(glue("^{paste(input$value_atc_2, collapse = '|')}"), atc_koodi)
                 # grepl("A", atc_koodi)
          )
        values1 <- tmp1$atc_koodi
        names(values1) <- tmp1$atc_selite
        
        tagList(
          # selectInput("value_atc_1", "ATC-luokka taso 1", choices = values1, selected = NA)
          pickerInput(inputId = ns("value_atc_3"),
                      label = i18n$t("Valitse ATC-luokka taso 3"),
                      choices = values1,
                      width = "100%",
                      # selected = kela,
                      options = pickerOptions(
                        actionsBox = TRUE,#liveSearch = TRUE,
                        size = 10,
                        deselectAllText = i18n$t("Ei mitään"),
                        selectAllText = i18n$t("Kaikki"),
                        noneSelectedText = i18n$t("Ei yhtään valittuna")
                      ),
                      multiple = TRUE)
        )
      }
    })
    
    output$inputs_atc4 <- renderUI({
      
      req(input$selected_language)
      req(input$value_atc_3)
      
      if (is.null(input$value_atc_1) | input$value_region != "Koko Suomi"){
        tagList()
      } else if (is.null(input$value_atc_2)) {
        tagList()
      } else if (is.null(input$value_atc_3)) {
        tagList()
      } else {
        
        datalist <- create_data()
        tmp1 <- datalist$atc %>%
          filter(atc_taso == 4,
                 grepl(glue("^{paste(input$value_atc_3, collapse = '|')}"), atc_koodi)
                 # grepl("A", atc_koodi)
          )
        values1 <- tmp1$atc_koodi
        names(values1) <- tmp1$atc_selite
        
        tagList(
          # selectInput("value_atc_1", "ATC-luokka taso 1", choices = values1, selected = NA)
          pickerInput(inputId = ns("value_atc_4"),
                      label = i18n$t("Valitse ATC-luokka taso 4"),
                      choices = values1,
                      width = "100%",
                      # selected = kela,
                      options = pickerOptions(
                        actionsBox = TRUE,#liveSearch = TRUE,
                        size = 10,
                        deselectAllText = i18n$t("Ei mitään"),
                        selectAllText = i18n$t("Kaikki"),
                        noneSelectedText = i18n$t("Ei yhtään valittuna")
                      ),
                      multiple = TRUE)
        )
      }
    })
    
    output$inputs_atc5 <- renderUI({
      
      req(input$selected_language)
      req(input$value_atc_4)
      
      if (is.null(input$value_atc_1) | input$value_region != "Koko Suomi"){
        tagList()
      } else if (is.null(input$value_atc_2)) {
        tagList()
      } else if (is.null(input$value_atc_3)) {
        tagList()
      } else if (is.null(input$value_atc_4)) {
        tagList()
      } else {
        
        datalist <- create_data()
        tmp1 <- datalist$atc %>%
          filter(atc_taso == 5,
                 grepl(glue("^{paste(input$value_atc_4, collapse = '|')}"), atc_koodi)
                 # grepl("A", atc_koodi)
          )
        values1 <- tmp1$atc_koodi
        names(values1) <- tmp1$atc_selite
        
        tagList(
          # selectInput("value_atc_1", "ATC-luokka taso 1", choices = values1, selected = NA)
          pickerInput(inputId = ns("value_atc_5"),
                      label = i18n$t("Valitse ATC-luokka taso 5"),
                      choices = values1,
                      width = "100%",
                      # selected = kela,
                      options = pickerOptions(
                        actionsBox = TRUE,#liveSearch = TRUE,
                        size = 10,
                        deselectAllText = i18n$t("Ei mitään"),
                        selectAllText = i18n$t("Kaikki"),
                        noneSelectedText = i18n$t("Ei yhtään valittuna")
                      ),
                      multiple = TRUE)
        )
      }
    })
    
    ### alue_input ----
    output$inputs_region_level <- renderUI({
      
      req(input$selected_language)
      
      regio_choices <- c("Koko Suomi", "Hyvinvointialueet")
      names(regio_choices) <- c(i18n$t("Koko Suomi"),i18n$t("Hyvinvointialueet"))
      
      tagList(
        selectInput(inputId = ns("value_region"),
                    label = i18n$t("Valitse aluetaso"),
                    choices = regio_choices,
                    selected = regio_choices[1])
      )
      
      
    })
    
    ### muuttuja_input ----
    output$inputs_varname <- renderUI({
      
      req(input$selected_language)
      
      var_choices <- c("var_kustannus", "var_n_ostot", "var_n_henkilot")
      names(var_choices) <- c(i18n$t("Kustannukset"),
                              i18n$t("Ostomäärät"),
                              i18n$t("Ostajien määrät"))
      
      tagList(
        radioButtons(inputId = ns("value_varname"),
                     label = i18n$t("Valitse kuvan muuttuja"),inline = TRUE,
                     choices = var_choices,
                     selected = var_choices[1])
      )
    })
    
    
    
    
    output$ui_bookmark <- renderUI({
      
      if (grepl("el", Sys.info()[["release"]])){
        shiny::bookmarkButton(label = i18n$t("Jaa valintasi"),
                              icon = icon("share-alt"))
      } else {
        " "
      }
    })
    
    output$data_updated <- renderUI({
      req(input$selected_language)
      tagList(
        i18n$t("Data päivitetty:"), tags$code(textOutput(ns("data_date"), inline = TRUE))
      )
    })
    
    ### luo_ladatattava_data ----
    create_download_csv <- reactive({
      # output$tbl_1 <- renderDataTable({
      
      datalist <- create_data()
      tmp_atc <- datalist$atc
      tmp_data <- datalist$data
      
      
      if (input$value_search_type == "tekstihaku"){
        
        if (is.null(input$value_atc_search)){
          df <- tmp_data %>%
            filter(atc_taso == 0) %>%
            mutate(atc_selite = i18n$t("Kaikki ATC-luokat yhdessä")) %>%
            select(aluenimi, viikko, everything())
          
        } else {
          
          df <- tmp_data %>%
            filter(atc_koodi %in% input$value_atc_search) %>%
            select(aluenimi, viikko, everything())
        }
        
      } else {
        
        if (is.null(input$value_atc_1)){
          df <- tmp_data %>%
            filter(atc_taso == 0) %>%
            mutate(atc_selite = i18n$t("Kaikki ATC-luokat yhdessä")) %>%
            select(aluenimi, viikko, everything())
          
          # } else {
        } else if (is.null(input$value_atc_2)){
          
          df <- tmp_data %>%
            filter(atc_taso == 1,
                   grepl(glue("^{paste(input$value_atc_1, collapse = '|')}"), atc_koodi)
                   # grepl("^R", atc_koodi)
            ) %>%
            group_by(vuosi, aluenimi, atc_koodi, atc_selite,viikko,variable) %>%
            summarise(arvo = sum(arvo, na.rm = TRUE)) %>%
            ungroup() %>%
            select(aluenimi, viikko, everything())
        } else if (is.null(input$value_atc_3)){
          
          df <- tmp_data %>%
            filter(atc_taso == 2,
                   grepl(glue("^{paste(input$value_atc_2, collapse = '|')}"), atc_koodi)) %>%
            group_by(vuosi, aluenimi, atc_koodi, atc_selite,viikko,variable) %>%
            summarise(arvo = sum(arvo, na.rm = TRUE)) %>%
            ungroup() %>%
            select(aluenimi, viikko, everything())
          
        } else if (is.null(input$value_atc_4)){
          
          df <- tmp_data %>%
            filter(atc_taso == 3,
                   grepl(glue("^{paste(input$value_atc_3, collapse = '|')}"), atc_koodi)) %>%
            group_by(vuosi, aluenimi, atc_koodi, atc_selite,viikko,variable) %>%
            summarise(arvo = sum(arvo, na.rm = TRUE)) %>%
            ungroup() %>%
            select(aluenimi, viikko, everything())
          
        } else  if (is.null(input$value_atc_5)){
          
          df <- tmp_data %>%
            filter(atc_taso == 4,
                   grepl(glue("^{paste(input$value_atc_4, collapse = '|')}"), atc_koodi)) %>%
            group_by(vuosi, aluenimi, atc_koodi, atc_selite,viikko,variable) %>%
            summarise(arvo = sum(arvo, na.rm = TRUE)) %>%
            ungroup() %>%
            select(aluenimi, viikko, everything())
        } else {
          
          df <- tmp_data %>%
            filter(atc_taso == 5,
                   grepl(glue("^{paste(input$value_atc_5, collapse = '|')}"), atc_koodi)) %>%
            group_by(vuosi, aluenimi, atc_koodi, atc_selite,viikko,variable) %>%
            summarise(arvo = sum(arvo, na.rm = TRUE)) %>%
            ungroup() %>%
            select(aluenimi, viikko, everything())
        }
      }
      
      metadata <- create_metadata()
      
      df2 <- df %>%
        left_join(metadata %>% mutate(code = tolower(code)) %>%
                    select(code,name),
                  by = c("variable" = "code")) %>%
        select(-variable) %>%
        rename(muuttuja = name) %>%
        select(aluenimi, viikko, muuttuja, everything())
      
      return(df2)
      # df
    })
    
    ### luo_kuviodata ----
    create_plot_data <- reactive({
      
      # req(input$tbl1_rows_selected)
      
      datalist <- create_data()
      tmp_atc <- datalist$atc
      tmp_data <- datalist$data
      
      
      if (input$value_search_type == "tekstihaku"){
        
        
        if (is.null(input$value_atc_search)){
          datplot <- tmp_data %>%
            filter(atc_taso == 0) %>%
            mutate(atc_selite = i18n$t("Kaikki ATC-luokat yhdessä"))
          
          plottitle = i18n$t("Kaikki ATC-luokat yhdessä")
          subtitle = NULL
          
        } else {
          
          groups <- paste(input$value_atc_search, collapse = '|')
          
          datplot <- tmp_data %>%
            # filter(grepl(glue("^{groups}"), atc_koodi)) %>%
            filter(atc_koodi %in% input$value_atc_search) %>%
            group_by(vuosi, aluenimi, atc_koodi, atc_selite,viikko,variable) %>%
            summarise(arvo = sum(arvo, na.rm = TRUE)) %>%
            ungroup()
          
          plottitle = glue(i18n$t("Valitut ATC-luokat eri tasoilta"))
          subtitle <- paste(tmp_atc[tmp_atc$atc_koodi %in% input$value_atc_1, ]$atc_selite, collapse = "\n")
          
        }
        
      } else {
        
        if (is.null(input$value_atc_1)){
          # if (input$value_region == "Koko Suomi"){
          datplot <- tmp_data %>%
            filter(atc_taso == 0) %>%
            mutate(atc_selite = i18n$t("Kaikki ATC-luokat yhdessä"))
          
          plottitle = i18n$t("Kaikki ATC-luokat yhdessä")
          subtitle = NULL
          
          # } else {
        } else if (is.null(input$value_atc_2)){
          
          groups <- paste(input$value_atc_1, collapse = '|')
          
          datplot <- tmp_data %>%
            filter(atc_taso == 1,
                   grepl(glue("^{groups}"), atc_koodi)) %>%
            group_by(vuosi, aluenimi, atc_koodi, atc_selite,viikko,variable) %>%
            summarise(arvo = sum(arvo, na.rm = TRUE)) %>%
            ungroup()
          
          plottitle = glue("{i18n$t('ATC-luokat tasolla')} 1")
          subtitle <- paste(tmp_atc[tmp_atc$atc_koodi %in% input$value_atc_1, ]$atc_selite, collapse = "\n")
        } else if (is.null(input$value_atc_3)){
          
          groups <- paste(input$value_atc_2, collapse = '|')
          
          datplot <- tmp_data %>%
            filter(atc_taso == 2,
                   grepl(glue("^{groups}"), atc_koodi)) %>%
            group_by(vuosi, aluenimi, atc_koodi, atc_selite,viikko,variable) %>%
            summarise(arvo = sum(arvo, na.rm = TRUE)) %>%
            ungroup()
          
          plottitle = glue("{i18n$t('ATC-luokat tasolla')} 2")
          subtitle =  subtitle <- paste(tmp_atc[tmp_atc$atc_koodi %in% input$value_atc_2, ]$atc_selite, collapse = "\n")
          
        } else if (is.null(input$value_atc_4)){
          
          groups <- paste(input$value_atc_3, collapse = '|')
          
          datplot <- tmp_data %>%
            filter(atc_taso == 3,
                   grepl(glue("^{groups}"), atc_koodi)) %>%
            group_by(vuosi, aluenimi, atc_koodi, atc_selite,viikko,variable) %>%
            summarise(arvo = sum(arvo, na.rm = TRUE)) %>%
            ungroup()
          
          plottitle = glue("{i18n$t('ATC-luokat tasolla')} 3")
          subtitle =  subtitle <- paste(tmp_atc[tmp_atc$atc_koodi %in% input$value_atc_3, ]$atc_selite, collapse = "\n")
          
        } else if (is.null(input$value_atc_5)){
          
          groups <- paste(input$value_atc_4, collapse = '|')
          
          datplot <- tmp_data %>%
            filter(atc_taso == 4,
                   grepl(glue("^{groups}"), atc_koodi)) %>%
            group_by(vuosi, aluenimi, atc_koodi, atc_selite,viikko,variable) %>%
            summarise(arvo = sum(arvo, na.rm = TRUE)) %>%
            ungroup()
          
          plottitle = glue("{i18n$t('ATC-luokat tasolla')} 4")
          subtitle =  subtitle <- paste(tmp_atc[tmp_atc$atc_koodi %in% input$value_atc_3, ]$atc_selite, collapse = "\n")
          
        } else {
          
          groups <- paste(input$value_atc_5, collapse = '|')
          
          datplot <- tmp_data %>%
            filter(atc_taso == 5,
                   grepl(glue("^{groups}"), atc_koodi)) %>%
            group_by(vuosi, aluenimi, atc_koodi, atc_selite,viikko,variable) %>%
            summarise(arvo = sum(arvo, na.rm = TRUE)) %>%
            ungroup()
          
          plottitle = glue("{i18n$t('ATC-luokat tasolla')} 5")
          subtitle =  subtitle <- paste(tmp_atc[tmp_atc$atc_koodi %in% input$value_atc_6, ]$atc_selite, collapse = "\n")
          
          
        }
      }
      
      metadata <- create_metadata()
      
      datplot2 <- datplot %>%
        left_join(metadata %>% mutate(code = tolower(code)) %>%
                    select(code,name,description),
                  by = c("variable" = "code")) %>%
        filter(!is.na(aluenimi))
      
      dat_plot_list <- list("datplot" = datplot2,
                            "plottitle" = plottitle,
                            "groups" = groups,
                            "facet_n" = length(unique(datplot$atc_selite)))
      
      return(dat_plot_list)
      
    })
    
    ### luo_kuvio ----
    #' create_plot_alt_text
    #' 
    #' @export
    create_plot_alt_text <- function(varname){
      
      
      dat_plot_list <- create_plot_data()
      
      if (varname == "var_kustannus"){
        plot_subtitle <- i18n$t("Apteekkien välityksellä korvattujen, tarkastelujakson aikana ostettujen lääkkeiden kustannukset. Kustannuksella tarkoitetaan lääkkeen hinnasta ja apteekin toimitusmaksusta koostuvaa summaa, josta ei ole vielä vähennetty sairausvakuutuskorvausta")
        plot_ytitle <- paste(i18n$t("Kustannukset"), "€")
      } else if (varname == "var_n_henkilot"){
        plot_subtitle <- i18n$t("Niiden henkilöiden lukumäärät, jotka ovat tarkastelujakson aikana ostaneet lääkkeitä. Nämä lääkeostot on joko korvattu apteekeissa tai niiden kustannukset ovat jääneet alle 50 euron omavastuun, jolloin ostot ovat vain kerryttäneet omavastuuta\n\nOstajien kumulatiivinen kokonaismäärä muodostetaan viikoittain määriteltyjen eri ostajien lukumäärien summana. Koska sama henkilö voi olla ostanut lääkettä useammalla eri viikolla, ei ostajien kumulatiivinen kokonaismäärä vastaa eri ostajien määrää")
        plot_ytitle <-i18n$t("Ostajien määrät")
      } else if (varname == "var_n_ostot"){
        plot_subtitle <- i18n$t("Ostolla tarkoitetaan yhdellä kertaa apteekista toimitettua tietyn lääkevalmisteen erää. Vuodeksi määrätty lääkevalmiste kirjautuu tilastoon yleensä useana ostona, koska potilas noutaa lääkkeensä tavallisesti kolmen kuukauden välein")
        plot_ytitle <-i18n$t("Ostomäärät")
      }
      
      # Lisätään vielä huomio skaaloista
      scale_note <- i18n$t("\n\nJokaisen paneelin ylempi kuva näyttää viikottaiset määrät ja alempi kertymän vuoden alusta (huomaa vaihtelevat y-akselit)")
      
      datplot <- dat_plot_list[["datplot"]]
      
      atcs <- unique(datplot$atc_selite)
      alt_teksti <- glue("{plot_subtitle}.\n\n{i18n$t('Tiedot näytetään aluetasolla')} {i18n$t(input$value_region)}. {i18n$t('Tiedot ovat viikkotasolla vuosilta 2019-2021.')} \n\n{i18n$t('Mukana ovat seuraavien ATC-luokkien lääkeaineet')}: {glue::glue_collapse(atcs, sep = ', ')}")
      return(alt_teksti)
    }
    
    
    

    create_plot <- function(varname){
      
      # Sairaahoitopiirien nimikäännökset geofacettiin
      
      # shpt <- structure(list(code = c("9", "15", "10", "25", "11", "19", "5",
      #                                 "17", "14", "8", "21", "20", "6", "12", "18", "13", "7", "4",
      #                                 "16", "3", "0"), name_fi = c("Etelä-Karjalan SHP", "Etelä-Pohjanmaan SHP",
      #                                                              "Etelä-Savon SHP", "Helsingin ja Uudenmaan SHP", "Itä-Savon SHP",
      #                                                              "Kainuun SHP", "Kanta-Hämeen SHP", "Keski-Pohjanmaan SHP", "Keski-Suomen SHP",
      #                                                              "Kymenlaakson SHP", "Lapin SHP", "Länsi-Pohjan SHP", "Pirkanmaan SHP",
      #                                                              "Pohjois-Karjalan SHP", "Pohjois-Pohjanmaan SHP", "Pohjois-Savon SHP",
      #                                                              "Päijät-Hämeen SHP", "Satakunnan SHP", "Vaasan SHP", "Varsinais-Suomen SHP",
      #                                                              "Ahvenanmaa"), name_sv = c("Södra Karelens SVD", "Syd-Österbottens SVD",
      #                                                                                         "Södra Savolax SVD", "Helsingfors och Nylands SVD", "Östra Savolax SVD",
      #                                                                                         "Kajanalands SVD", "Centrala Tavastlands SVD", "Mellersta Österbottens SVD",
      #                                                                                         "Mellersta Finlands SVD", "Kymmenedalens SVD", "Lapplands SVD",
      #                                                                                         "Länsi-Pohja SVD", "Birkalands SVD", "Norra Karelens SVD", "Norra Österbottens SVD",
      #                                                                                         "Norra Savolax SVD", "Päijät-Häme SVD", "Satakunta SVD", "Vasa SVD",
      #                                                                                         "Egentliga Finlands SVD", "Åland"), name_en = c("South Karelia Hospital District",
      #                                                                                                                                         "South Ostrobothnia Hospital District", "South Savo Hospital District",
      #                                                                                                                                         "Helsinki and Uusimaa Hospital District", "Itä-Savo Hospital District",
      #                                                                                                                                         "Kainuu Hospital District", "Kanta-Häme Hospital District",
      #                                                                                                                                         "Central Ostrobothnia Hospital District", "Central Finland Hospital District",
      #                                                                                                                                         "Kymenlaakso Hospital District", "Lappi Hospital District", "Länsi-Pohja Hospital District",
      #                                                                                                                                         "Pirkanmaa Hospital District", "North Karelia Hospital District",
      #                                                                                                                                         "North Ostrobothnia Hospital District", "North Savo Hospital District",
      #                                                                                                                                         "Päijät-Häme Hospital District", "Satakunta Hospital District",
      #                                                                                                                                         "Vaasa Hospital District", "Southwest Finland Hospital District",
      #                                                                                                                                         "Åland")), row.names = c(NA, -21L), class = c("tbl_df", "tbl",
      #                                                                                                                                                                                       "data.frame"))
      shpt <- structure(list(ALUEKOODI = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 
                                           12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 90, 91, 99), ALUENIMI_FI = c("Itä-Uusimaa", 
                                                                                                                "Keski-Uusimaa", "Länsi-Uusimaa", "Vantaa ja Kerava", "Varsinais-Suomi", 
                                                                                                                "Satakunta", "Kanta-Häme", "Pirkanmaa", "Päijät-Häme", "Kymenlaakso", 
                                                                                                                "Etelä-Karjala", "Etelä-Savo", "Pohjois-Savo", "Pohjois-Karjala", 
                                                                                                                "Keski-Suomi", "Etelä-Pohjanmaa", "Pohjanmaa", "Keski-Pohjanmaa", 
                                                                                                                "Pohjois-Pohjanmaa", "Kainuu", "Lappi", "Helsinki", "Ahvenanmaa", 
                                                                                                                "Koko Suomi"), ALUENIMI_SV = c("Östra Nyland", "Mellersta Nyland", 
                                                                                                                                               "Västra Nyland", "Vanda och Kervo", "Egentliga Finland", "Satakunta", 
                                                                                                                                               "Egentliga Tavastland", "Birkaland", "Päijänne-Tavastland", 
                                                                                                                                               "Kymmenedalen", "Södra Karelen", "Södra Savolax", "Norra Savolax", 
                                                                                                                                               "Norra Karelen", "Mellersta Finland", "Södra Österbotten", 
                                                                                                                                               "Österbotten", "Mellersta Österbotten", "Norra Österbotten", 
                                                                                                                                               "Kajanaland", "Lappland", "Helsingfors", "Åland", "Whole Finland"
                                                                                                                ), ALUENIMI_EN = c("East Uusimaa", "Central Uusimaa", "West Uusimaa", 
                                                                                                                                   "Vantaa and Kerava", "Southwest Finland", "Satakunta", "Kanta-Häme", 
                                                                                                                                   "Pirkanmaa", "Päijät-Häme", "Kymenlaakso", "South Karelia", 
                                                                                                                                   "South Savo", "North Savo", "North Karelia", "Central Finland", 
                                                                                                                                   "South Ostrobothnia", "Ostrobothnia", "Central Ostrobothnia", 
                                                                                                                                   "North Ostrobothnia", "Kainuu", "Lapland", "Helsinki", "Åland", 
                                                                                                                                   "Hela Finland")), row.names = c(NA, -24L), class = c("tbl_df", 
                                                                                                                                                                                        "tbl", "data.frame"))
      
      names(shpt) <- c("code","name_fi","name_sv","name_en")
      mygrid <- data.frame(
        name = c("Lappi", "Kainuu", "Pohjois-Pohjanmaa", "Keski-Pohjanmaa", 
                 "Pohjanmaa", "Etelä-Pohjanmaa", "Pohjois-Savo", "Keski-Suomi", 
                 "Pohjois-Karjala", "Pirkanmaa", "Satakunta", "Etelä-Savo", "Päijät-Häme", 
                 "Kanta-Häme", "Etelä-Karjala", "Kymenlaakso", "Varsinais-Suomi", 
                 "Ahvenanmaa", "Länsi-Uusimaa", "Keski-Uusimaa", "Itä-Uusimaa", 
                 "Vantaa ja Kerava", "Helsinki"),
        code = c("21", "20", "19", "18", "17", "16", "13", "15", 
                 "14", "8", "6", "12", "9", "7", "11", "10", "5", "91", "3", 
                 "2", "1", "4", "90"),
        row = c(1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7), 
        col = c(3, 4, 3, 2, 1, 1, 3, 2, 4, 2, 1, 4, 3, 2, 4, 3, 1, 1, 2, 3, 4, 3, 2),
        stringsAsFactors = FALSE
      )
      
      grid_geo <- left_join(mygrid %>% mutate(code = as.character(as.integer(code))),
                            shpt %>% mutate(code = as.character(as.integer(code)))) %>%
        filter(!is.na(name_fi))
      
      
      dat_plot_list <- create_plot_data()
      
      if (varname == "var_kustannus"){
        plot_subtitle <- i18n$t("Apteekkien välityksellä korvattujen, tarkastelujakson aikana ostettujen lääkkeiden kustannukset. Kustannuksella tarkoitetaan lääkkeen hinnasta ja apteekin toimitusmaksusta koostuvaa summaa, josta ei ole vielä vähennetty sairausvakuutuskorvausta")
        plot_ytitle <- paste(i18n$t("Kustannukset"), "€")
      } else if (varname == "var_n_henkilot"){
        plot_subtitle <- i18n$t("Niiden henkilöiden lukumäärät, jotka ovat tarkastelujakson aikana ostaneet lääkkeitä. Nämä lääkeostot on joko korvattu apteekeissa tai niiden kustannukset ovat jääneet alle 50 euron omavastuun, jolloin ostot ovat vain kerryttäneet omavastuuta\n\nOstajien kumulatiivinen kokonaismäärä muodostetaan viikoittain määriteltyjen eri ostajien lukumäärien summana. Koska sama henkilö voi olla ostanut lääkettä useammalla eri viikolla, ei ostajien kumulatiivinen kokonaismäärä vastaa eri ostajien määrää")
        plot_ytitle <-i18n$t("Ostajien määrät")
      } else if (varname == "var_n_ostot"){
        plot_subtitle <- i18n$t("Ostolla tarkoitetaan yhdellä kertaa apteekista toimitettua tietyn lääkevalmisteen erää. Vuodeksi määrätty lääkevalmiste kirjautuu tilastoon yleensä useana ostona, koska potilas noutaa lääkkeensä tavallisesti kolmen kuukauden välein")
        plot_ytitle <-i18n$t("Ostomäärät")
      }
      
      # Lisätään vielä huomio skaaloista
      scale_note <- i18n$t("\n\nJokaisen paneelin ylempi kuva näyttää viikottaiset määrät ja alempi kertymän vuoden alusta (huomaa vaihtelevat y-akselit)")
      
      plot_subtitle <- paste0(add_line_break2(plot_subtitle, 120),
                              add_line_break2(scale_note, 120))
      
      datplot <- dat_plot_list[["datplot"]]
      datplot <- datplot[datplot$variable %in% varname,]
      plottitle <- dat_plot_list[["plottitle"]]
      groups <- dat_plot_list[["groups"]]
      facet_n <- dat_plot_list[["facet_n"]]
      
      # plot_subtitle = unique(datplot$description)
      # plot_ytitle <- unique(datplot$name)
      plot_xtitle <- i18n$t("viikko")
      
      vkos <- 1:52
      breaksit <- vkos[vkos %% 4 == 0]
      
      
      
      if (input$value_region == "Koko Suomi"){
        
        
        
        
        
        # lisätään kertymäkuvat
        plotlist <- list()
        atcs <- unique(datplot$atc_selite)
        for (i in 1:length(atcs)){
          datplot2 <- datplot[datplot$atc_selite %in% atcs[i],]
          
          # if (i %in% 1:2) legend_position <- "top" else legend_position <- "none"
          legend_position <- "top"
          
          yrs <- unique(datplot2$vuosi)
          atc_color_palette_ss <- create_color_palette(years = yrs)
          
          
          p1 <- ggplot(datplot2, aes(x = viikko, y = arvo, fill = factor(vuosi), color = factor(vuosi), group = vuosi)) +
            geom_path(alpha = .8) +
            geom_point(shape = 21, color = "white", size = 2, stroke = 1, alpha = .8) +
            labs(fill = NULL,
                 color = NULL,
                 title = add_line_break2(atcs[i], n = 40),
                 y = plot_ytitle,
                 x = NULL) +
            scale_fill_manual(values = atc_color_palette_ss) +
            scale_color_manual(values = atc_color_palette_ss) +
            hrbrthemes::theme_ipsum(base_family = "Lato", base_size = 11, axis_title_size = 12, plot_title_size = 14) +
            theme(legend.position = legend_position,
                  panel.grid.minor = element_blank(),
                  axis.text.x = element_blank(),
                  legend.text = element_text(size = 12),
                  legend.key.size = unit(20, "mm"),
                  panel.spacing = unit(0, "lines"),
                  panel.spacing.y = unit(0, "lines"),
                  plot.margin = margin(10, 10, 6, 10)
            ) +
            scale_y_continuous(labels = function(x) format(x, big.mark = " ",
                                                           scientific = FALSE),
                               limits = c(0,NA)) +
            scale_x_continuous(breaks = breaksit,
                               limits = c(1,max(datplot2$viikko)))
          
          datplot2 <- datplot2 %>% group_by(vuosi) %>% mutate(cumarvo = cumsum(arvo))
          
          p2 <- ggplot(datplot2, aes(x = viikko, y = cumarvo, fill = factor(vuosi), color = factor(vuosi), group = vuosi)) +
            geom_path(alpha = .8) +
            geom_point(shape = 21, color = "white", size = 2, stroke = 1, alpha = .8) +
            labs(fill = NULL,
                 color = NULL,
                 #title = atcs[i],
                 #subtitle = add_line_break2(plot_subtitle, n = 90),
                 y = NULL,
                 x = plot_xtitle) +
            scale_fill_manual(values = atc_color_palette_ss) +
            scale_color_manual(values = atc_color_palette_ss) +
            hrbrthemes::theme_ipsum(base_family = "Lato", base_size = 11, axis_title_size = 12) +
            theme(panel.grid.minor = element_blank(),
                  legend.position = "none",
                  legend.text = element_text(size = 12),
                  legend.key.size = unit(20, "mm"),
                  panel.spacing = unit(0, "lines"),
                  panel.spacing.y = unit(0, "lines"),
                  plot.margin = margin(10, 10, 6, 10)
            ) +
            # scale_y_log10(labels = function(x) format(x, big.mark = " ",
            #                                                scientific = FALSE)#,
            #                    # limits = c(0,NA)
            #               )
            scale_y_continuous(labels = function(x) format(x, big.mark = " ",
                                                           scientific = FALSE),
                               limits = c(0,NA)
            ) +
            scale_x_continuous(breaks = breaksit,
                               limits = c(1,max(datplot2$viikko)))
          
          p1 + p2 +
            plot_layout(
              ncol = 1,
              heights = c(1, 1)
            ) -> plotlist[[i]]
          
          wrap_plots(plotlist, ncol = 2) +
            plot_annotation(
              title = plot_ytitle,
              subtitle = plot_subtitle,
              caption = Sys.time(),
              theme = hrbrthemes::theme_ipsum(base_family = "Lato", base_size = 15)
            )  -> p
        }
        
        # facet_wrap(~add_line_break2(atc_selite, 35) , scales = "free", ncol = 2) +
        
        
        
      } else {
        
        # datplot$aluenimi <-
        
        if (input$selected_language == "fi") grid_geo$name <- grid_geo$name_fi
        if (input$selected_language == "sv") grid_geo$name <- grid_geo$name_sv
        if (input$selected_language == "en") grid_geo$name <- grid_geo$name_en
        
        yrs <- unique(datplot$vuosi)
        atc_color_palette_ss <- create_color_palette(years = yrs)
        
        
        p <-  ggplot(datplot, aes(x = viikko, y = arvo,
                                  fill = factor(vuosi),
                                  color = factor(vuosi),
                                  group = paste0(atc_selite,vuosi),
                                  shape = atc_selite)) +
          geom_line(alpha = .7) +
          geom_point(size = 1.1, stroke = .4, alpha = 1, color = "dim grey") +
          geofacet::facet_geo(~aluenimi, grid = grid_geo, scales = "free") +
          # facet_wrap(~alue) +
          hrbrthemes::theme_ipsum(base_family = "Lato", base_size = 10, axis_title_size = 12) +
          theme(legend.position = "top",
                panel.grid.minor = element_blank(),
                legend.text = element_text(size = 12),
                axis.text.x = element_text(size = 7),
                legend.key.height = unit(5, "mm"),
                legend.key.width = unit(15, "mm"),
                legend.direction = "vertical"
          ) +
          scale_shape_manual(values=c(21, 22, 23, 24, 25)) +
          scale_x_continuous(breaks = breaksit) +
          labs(fill = NULL,
               color = NULL,
               title = plottitle,
               subtitle = add_line_break2(plot_subtitle, n = 90),
               shape = NULL,
               y = plot_ytitle,
               x = plot_xtitle) +
          scale_fill_manual(values = atc_color_palette_ss) +
          scale_color_manual(values = atc_color_palette_ss) +
          scale_y_continuous(labels = function(x) format(x, big.mark = " ",
                                                         scientific = FALSE),
                             limits = c(0,NA))
        
        
      }
      return(p)
    }
  
    
    funk <- eventReactive({
      input$nappula
    }, {
      create_plot(input$value_varname)
    }, ignoreNULL = FALSE)
    
    output$timeseries_plot <- renderPlot({
      funk()
    })
    
    
    output$plot_main <- renderPlot({
      # create_plot(input$value_varname)
      funk()
    }, alt = reactive({create_plot_alt_text(input$value_varname)}) )
    
    
    dimsReactive <- eventReactive({
      input$nappula
    }, {
      if (input$nappula == 0){
        dims <- c("100%","700px")
      } else {
        dat_plot_list <- create_plot_data()
        facet_n <- dat_plot_list[["facet_n"]]
        width = "100%"
        # if (TRUE){
        if (input$value_region == "Koko Suomi"){
          height = paste0(200 + ceiling(facet_n/2) * 550, "px")
        } else {
          height = "1300px"
          width = "150%"
        }
        dims <- c(width,height)
      }
      return(dims)
    }, ignoreNULL = FALSE)
    
    
    
    output$plot_main_ui <- renderUI({
      width_height <- dimsReactive()
      tagList(
      tags$div(class = "plot", style='width:100%; overflow-x: auto; overflow-y: hidden;',
      plotOutput(ns("plot_main"), width = width_height[1], height = width_height[2])
      )
      )
    })
    
    output$ui_download_csv <- renderUI({
      req(input$selected_language)
      tagList(
        downloadButton(ns("download_csv"), i18n$t("Lataa .csv-data"))
      )
    })
    
    
    output$download_csv <- downloadHandler(
      filename = function() {
        data_name <- "data.csv"
        return(data_name)
      },
      content = function(file) {
        readr::write_excel_csv2(x = create_download_csv(), file = file)
      }
    )
    
    
    
    output$ui_download_pdf <- renderUI({
      req(input$selected_language)
      tagList(
        downloadButton(ns("download_pdf"), i18n$t("Lataa .pdf-kuva"))
      )
    })
    
    output$ui_download_png <- renderUI({
      req(input$selected_language)
      tagList(
        downloadButton(ns("download_png"), i18n$t("Lataa .png-kuva"))
      )
    })
    
    output$ui_download_svg <- renderUI({
      req(input$selected_language)
      tagList(
        downloadButton(ns("download_svg"), i18n$t("Lataa .svg-kuva"))
      )
    })
    
    
    output$download_png <- downloadHandler(
      filename = function() {
        plot_name <- "kuvio.png"
        return(plot_name)
      },
      content = function(file) {
        
        dat_plot_list <- create_plot_data()
        facet_n <- dat_plot_list[["facet_n"]]
        # facet_n = 4
        
        width = 13
        if (input$value_region == "Koko Suomi"){
          height = 3 + ceiling(facet_n/2) * 5.2
        } else {
          height = 28
          width = 32
        }
        
        # if (TRUE) plot_varname <- "var_kustannus"
        p1 <- create_plot(input$value_varname)
        
        ggsave(file, plot = p1, device = "png", width = width, height = height, limitsize = FALSE, dpi = 90)
      }
    )
    
    output$download_pdf <- downloadHandler(
      
      filename = function() {
        plot_name <- "kuvio.pdf"
        return(plot_name)
      },
      content = function(file) {
        
        dat_plot_list <- create_plot_data()
        facet_n <- dat_plot_list[["facet_n"]]
        # facet_n = 4
        
        width = 13
        if (input$value_region == "Koko Suomi"){
          height = 3 + ceiling(facet_n/2) * 5.2
        } else {
          height = 18
          width = 22
        }
        
        # if (TRUE) plot_varname <- "var_kustannus"
        
        p1 <- create_plot(input$value_varname)
        ggsave(file, plot = p1, device = cairo_pdf, width = width, height = height, limitsize = FALSE)
      }
    )
    
    output$download_svg <- downloadHandler(
      
      filename = function() {
        plot_name <- "kuvio.svg"
        return(plot_name)
      },
      content = function(file) {
        
        dat_plot_list <- create_plot_data()
        facet_n <- dat_plot_list[["facet_n"]]
        # facet_n = 4
        
        width = 13
        if (input$value_region == "Koko Suomi"){
          height = 3 + ceiling(facet_n/2) * 5.2
        } else {
          height = 18
          width = 22
        }
        
        if (TRUE) plot_varname <- "var_kustannus"
        
        p1 <- create_plot(input$value_varname)
        ggsave(file, plot = p1, #device = cairo_pdf,
               width = width, height = height, limitsize = FALSE)
      }
    )
    
    # palautetaan kaikki inputit
    return(input)
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
