shinyServer(function(input, output, session) {
  
  i18n <- Translator$new(translation_json_path='translations/translation.json')
  i18n$set_translation_language('fi')
  
  observeEvent(input$selected_language, {
    update_lang(session, input$selected_language)
  })
  
  
  ### inputit ----
  output$ui_language_selection <- renderUI({
    tagList(
      selectInput(inputId = 'selected_language',
                  # label = i18n$t("kieli"),
                  label = icon(name = "language", class = "ikoni"),
                  choices = i18n$get_languages(),
                  selected = input$selected_language)
    )
  })
  
  output$ui_text_title <- renderUI({
    req(input$selected_language)
    tagList(
      i18n$t("Reseptilääkkeiden ostot ATC-luokittain")
    )
  })
  
  output$ui_text_sidebartitle <- renderUI({
    tagList(
      tags$p(i18n$t("Valitse sivu"))
    )
  })
  
  ### datafunktiot ----
  create_metadata <- reactive({
    
    try_error <- try(df2 <- readr::read_csv2("https://github.com/kelaresearchandanalytics/korona_atc_data/raw/master/metadata_viikko.csv"))
    if ("try-error" %in% class(try_error)){
      df2 <- readRDS("./data/metadata_viikko.RDS")
    }
    names(df2) <- tolower(names(df2))
    return(df2)
  })
  
  
  output$data_date <- renderText({
    meta <- create_metadata()
    as.character(unique(meta$updated))
  })
  
  create_data <- reactive({
    
    try_error <- try(df2 <- readr::read_csv2("https://github.com/kelaresearchandanalytics/korona_atc_data/raw/master/data_viikko_2019.csv"))
    if ("try-error" %in% class(try_error)){
      df2 <- readRDS("./data/data_viikko.RDS")
    } else {
      df3 <- readr::read_csv2("https://github.com/kelaresearchandanalytics/korona_atc_data/raw/master/data_viikko_2020.csv")
      df4 <- readr::read_csv2("https://github.com/kelaresearchandanalytics/korona_atc_data/raw/master/data_viikko_2021.csv")
      df2 <- bind_rows(df2,df3,df4)
    }
    names(df2) <- tolower(names(df2))
    
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
      radioButtons(inputId = "value_search_type",
                   label = i18n$t("Hakutyyppi"),
                   choices = search_choices,
                   selected = search_choices[1])
    )
  })
  
  
  output$inputs_search_box <- renderUI({
    
    req(input$selected_language)
    req(input$value_search_type)
    
    if (input$value_search_type == "tekstihaku"){
      
      tagList(
        textInput(inputId = "value_atc_search_string",
                  width = "100%",
                  label = i18n$t("Rajaa ATC-luokka nimen/koodin perusteella (erota hakuehdot |-merkillä)"),
                  value ="R03|A04")
      )
      
    } else {
      tagList()
    }
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
        pickerInput("value_atc_search",
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
        pickerInput("value_atc_1",
                    label = i18n$t("ATC-luokka taso 1"),
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
        pickerInput("value_atc_2",
                    label = i18n$t("ATC-luokka taso 2"),
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
        pickerInput("value_atc_3",
                    label = i18n$t("ATC-luokka taso 3"),
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
        pickerInput("value_atc_4",
                    label = i18n$t("ATC-luokka taso 4"),
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
        pickerInput("value_atc_5",
                    label = i18n$t("ATC-luokka taso 5"),
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
    
    regio_choices <- c("Koko Suomi", "Sairaanhoitopiirit")
    names(regio_choices) <- c(i18n$t("Koko Suomi"),i18n$t("Sairaanhoitopiirit"))
    
    tagList(
      selectInput(inputId = "value_region", 
                  label = i18n$t("Aluetaso"),
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
      radioButtons(inputId = "value_varname",
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
      i18n$t("Data päivitetty:"), tags$code(textOutput("data_date", inline = TRUE))
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
          select(aluenimi, viikko, everything()) %>%
          pivot_wider(names_from = vuosi,values_from = arvo) %>%
          mutate(ero = round(`2020` - `2019`),
                 ero_pros = round(ero / `2019`*100,1))
        
      } else {
        
        df <- tmp_data %>%
          filter(atc_koodi %in% input$value_atc_search) %>%
          select(aluenimi, viikko, everything()) %>%
          pivot_wider(names_from = vuosi,values_from = arvo) %>%
          mutate(ero = round(`2020` - `2019`),
                 ero_pros = round(ero / `2019`*100,1))
      }
      
    } else {
      
      if (is.null(input$value_atc_1)){
        df <- tmp_data %>% 
          filter(atc_taso == 0) %>% 
          mutate(atc_selite = i18n$t("Kaikki ATC-luokat yhdessä")) %>% 
          select(aluenimi, viikko, everything()) %>%
          pivot_wider(names_from = vuosi,values_from = arvo) %>%
          mutate(ero = round(`2020` - `2019`),
                 ero_pros = round(ero / `2019`*100,1))
        
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
          select(aluenimi, viikko, everything()) %>%
          pivot_wider(names_from = vuosi,values_from = arvo) %>%
          mutate(ero = round(`2020` - `2019`),
                 ero_pros = round(ero / `2019`*100,1))
      } else if (is.null(input$value_atc_3)){
        
        df <- tmp_data %>%
          filter(atc_taso == 2,
                 grepl(glue("^{paste(input$value_atc_2, collapse = '|')}"), atc_koodi)) %>%
          group_by(vuosi, aluenimi, atc_koodi, atc_selite,viikko,variable) %>%
          summarise(arvo = sum(arvo, na.rm = TRUE)) %>%
          ungroup() %>%
          select(aluenimi, viikko, everything()) %>%
          pivot_wider(names_from = vuosi,values_from = arvo) %>%
          mutate(ero = round(`2020` - `2019`),
                 ero_pros = round(ero / `2019`*100,1))
        
      } else if (is.null(input$value_atc_4)){
        
        df <- tmp_data %>%
          filter(atc_taso == 3,
                 grepl(glue("^{paste(input$value_atc_3, collapse = '|')}"), atc_koodi)) %>%
          group_by(vuosi, aluenimi, atc_koodi, atc_selite,viikko,variable) %>%
          summarise(arvo = sum(arvo, na.rm = TRUE)) %>%
          ungroup() %>%
          select(aluenimi, viikko, everything()) %>%
          pivot_wider(names_from = vuosi,values_from = arvo) %>%
          mutate(ero = round(`2020` - `2019`),
                 ero_pros = round(ero / `2019`*100,1))
        
      } else  if (is.null(input$value_atc_5)){
        
        df <- tmp_data %>%
          filter(atc_taso == 4,
                 grepl(glue("^{paste(input$value_atc_4, collapse = '|')}"), atc_koodi)) %>%
          group_by(vuosi, aluenimi, atc_koodi, atc_selite,viikko,variable) %>%
          summarise(arvo = sum(arvo, na.rm = TRUE)) %>%
          ungroup() %>%
          select(aluenimi, viikko, everything()) %>%
          pivot_wider(names_from = vuosi,values_from = arvo) %>%
          mutate(ero = round(`2020` - `2019`),
                 ero_pros = round(ero / `2019`*100,1))
        
      } else {
        
        df <- tmp_data %>%
          filter(atc_taso == 5,
                 grepl(glue("^{paste(input$value_atc_5, collapse = '|')}"), atc_koodi)) %>%
          group_by(vuosi, aluenimi, atc_koodi, atc_selite,viikko,variable) %>%
          summarise(arvo = sum(arvo, na.rm = TRUE)) %>%
          ungroup() %>%
          select(aluenimi, viikko, everything()) %>%
          pivot_wider(names_from = vuosi,values_from = arvo) %>%
          mutate(ero = round(`2020` - `2019`),
                 ero_pros = round(ero / `2019`*100,1))
        
      }
    }
    
    metadata <- create_metadata()
    
    df2 <- df %>% 
      filter(`2019` > 0,
             `2020` > 0) %>%
      left_join(metadata %>% mutate(code = tolower(code)) %>% 
                  select(code,name), 
                by = c("variable" = "code")) %>% 
      select(-variable) %>% 
      rename(muuttuja = name) %>% 
      # select(aluenimi, viikko, muuttuja, everything())
      select(aluenimi,viikko,muuttuja,atc_koodi,atc_selite,`2019`,`2020`,ero,ero_pros)
    
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
  create_plot <- function(varname){
    
    # Sairaahoitopiirien nimikäännökset geofacettiin
    
    shpt <- structure(list(code = c("9", "15", "10", "25", "11", "19", "5", 
                                    "17", "14", "8", "21", "20", "6", "12", "18", "13", "7", "4", 
                                    "16", "3", "0"), name_fi = c("Etelä-Karjalan SHP", "Etelä-Pohjanmaan SHP", 
                                                                   "Etelä-Savon SHP", "Helsingin ja Uudenmaan SHP", "Itä-Savon SHP", 
                                                                   "Kainuun SHP", "Kanta-Hämeen SHP", "Keski-Pohjanmaan SHP", "Keski-Suomen SHP", 
                                                                   "Kymenlaakson SHP", "Lapin SHP", "Länsi-Pohjan SHP", "Pirkanmaan SHP", 
                                                                   "Pohjois-Karjalan SHP", "Pohjois-Pohjanmaan SHP", "Pohjois-Savon SHP", 
                                                                   "Päijät-Hämeen SHP", "Satakunnan SHP", "Vaasan SHP", "Varsinais-Suomen SHP", 
                                                                   "Ahvenanmaa"), name_sv = c("Södra Karelens SVD", "Syd-Österbottens SVD", 
                                                                                              "Södra Savolax SVD", "Helsingfors och Nylands SVD", "Östra Savolax SVD", 
                                                                                              "Kajanalands SVD", "Centrala Tavastlands SVD", "Mellersta Österbottens SVD", 
                                                                                              "Mellersta Finlands SVD", "Kymmenedalens SVD", "Lapplands SVD", 
                                                                                              "Länsi-Pohja SVD", "Birkalands SVD", "Norra Karelens SVD", "Norra Österbottens SVD", 
                                                                                              "Norra Savolax SVD", "Päijät-Häme SVD", "Satakunta SVD", "Vasa SVD", 
                                                                                              "Egentliga Finlands SVD", "Åland"), name_en = c("South Karelia Hospital District", 
                                                                                                                                              "South Ostrobothnia Hospital District", "South Savo Hospital District", 
                                                                                                                                              "Helsinki and Uusimaa Hospital District", "Itä-Savo Hospital District", 
                                                                                                                                              "Kainuu Hospital District", "Kanta-Häme Hospital District", 
                                                                                                                                              "Central Ostrobothnia Hospital District", "Central Finland Hospital District", 
                                                                                                                                              "Kymenlaakso Hospital District", "Lappi Hospital District", "Länsi-Pohja Hospital District", 
                                                                                                                                              "Pirkanmaa Hospital District", "North Karelia Hospital District", 
                                                                                                                                              "North Ostrobothnia Hospital District", "North Savo Hospital District", 
                                                                                                                                              "Päijät-Häme Hospital District", "Satakunta Hospital District", 
                                                                                                                                              "Vaasa Hospital District", "Southwest Finland Hospital District", 
                                                                                                                                              "Åland")), row.names = c(NA, -21L), class = c("tbl_df", "tbl", 
                                                                                                                                                                                            "data.frame"))
    grid_geo <- left_join(geofi::grid_sairaanhoitop %>% mutate(code = as.character(as.integer(code))), 
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
    
    
    if (input$value_region == "Koko Suomi"){
      
      
      
      
      
      # lisätään kertymäkuvat
      plotlist <- list()
      atcs <- unique(datplot$atc_selite)
      for (i in 1:length(atcs)){
        datplot2 <- datplot[datplot$atc_selite %in% atcs[i],]
        
        if (i %in% 1:2) legend_position <- "top" else legend_position <- "none"
        
        p1 <- ggplot(datplot2, aes(x = viikko, y = arvo, fill = factor(vuosi), color = factor(vuosi), group = vuosi)) +
          geom_path(alpha = .8) +
          geom_point(shape = 21, color = "white", size = 2, stroke = 1, alpha = .8) +
          labs(fill = NULL, 
               color = NULL, 
               title = add_line_break2(atcs[i], n = 40),
               y = plot_ytitle,
               x = NULL) +
          scale_fill_manual(values = atc_color_palette) +
          scale_color_manual(values = atc_color_palette) +
          hrbrthemes::theme_ipsum(base_family = "PT Sans", base_size = 11, axis_title_size = 12, plot_title_size = 14) +
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
          scale_x_continuous(breaks = (1:max(datplot2$viikko))[is.even(1:max(datplot2$viikko))], 
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
          scale_fill_manual(values = atc_color_palette) +
          scale_color_manual(values = atc_color_palette) +
          hrbrthemes::theme_ipsum(base_family = "PT Sans", base_size = 11, axis_title_size = 12) +
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
          scale_x_continuous(breaks = (1:max(datplot2$viikko))[is.even(1:max(datplot2$viikko))], 
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
            theme = hrbrthemes::theme_ipsum(base_family = "PT Sans", base_size = 15)
          )  -> p
      }
      
      # facet_wrap(~add_line_break2(atc_selite, 35) , scales = "free", ncol = 2) +
      
      
      
    } else {
      
      # datplot$aluenimi <- 
      
      if (input$selected_language == "fi") grid_geo$name <- grid_geo$name_fi
      if (input$selected_language == "sv") grid_geo$name <- grid_geo$name_sv
      if (input$selected_language == "en") grid_geo$name <- grid_geo$name_en
      
      
      p <-  ggplot(datplot, aes(x = viikko, y = arvo, 
                                fill = factor(vuosi), 
                                color = factor(vuosi), 
                                group = paste0(atc_selite,vuosi), 
                                shape = atc_selite)) +
        geom_line(alpha = .7) +
        geom_point(size = 1.8, stroke = .4, alpha = .7, color = "dim grey") +
        geofacet::facet_geo(~aluenimi, grid = grid_geo, scales = "free") +
        # facet_wrap(~alue) +
        hrbrthemes::theme_ipsum(base_family = "PT Sans", base_size = 10, axis_title_size = 12) +
        theme(legend.position = "top", 
              panel.grid.minor = element_blank(),
              legend.text = element_text(size = 12),
              legend.key.height = unit(5, "mm"),
              legend.key.width = unit(15, "mm"),
              legend.direction = "vertical"
        ) +
        scale_shape_manual(values=c(21, 22, 23, 24, 25)) +
        scale_x_continuous(breaks = (1:max(datplot$viikko))[is.even(1:max(datplot$viikko))]) +
        labs(fill = NULL, 
             color = NULL, 
             title = plottitle, 
             subtitle = add_line_break2(plot_subtitle, n = 90),
             shape = NULL,
             y = plot_ytitle,
             x = plot_xtitle) +
        scale_fill_manual(values = atc_color_palette) +
        scale_color_manual(values = atc_color_palette) +
        scale_y_continuous(labels = function(x) format(x, big.mark = " ",
                                                       scientific = FALSE),
                           limits = c(0,NA))
      
      
    }
    return(p)
  }
  
  
  output$plot_main <- renderPlot({
    create_plot(input$value_varname)
  })
  output$plot_main_ui <- renderUI({
    
    req(input$selected_language)
    
    dat_plot_list <- create_plot_data()
    facet_n <- dat_plot_list[["facet_n"]]
    width = "100%"
    if (input$value_region == "Koko Suomi"){
      height = paste0(200 + ceiling(facet_n/2) * 550, "px")
    } else {
      height = "1100px"
    }
    plotOutput("plot_main", width = width, height = height)
  })
  
  
 
  
  
  output$ui_download_csv <- renderUI({
    req(input$selected_language)
    tagList(
      downloadButton("download_csv", i18n$t("Lataa .csv-data"))
    )
  })  
  
  
  output$download_csv <- downloadHandler(
    filename = function() {
      data_name <- "data.csv"
      return(data_name)
    },
    content = function(file) {
      readr::write_excel_csv2(x = create_download_csv(), path = file)
    }
  )
  
  
  
  output$ui_download_pdf <- renderUI({
    req(input$selected_language)
    tagList(
      downloadButton("download_pdf", i18n$t("Lataa .pdf-kuva"))
    )
  })
  
  output$ui_download_png <- renderUI({
    req(input$selected_language)
    tagList(
      downloadButton("download_png", i18n$t("Lataa .png-kuva"))
    )
  })
  
  output$ui_download_svg <- renderUI({
    req(input$selected_language)
    tagList(
      downloadButton("download_svg", i18n$t("Lataa .svg-kuva"))
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
        height = 16
        width = 16
      }
      
      if (TRUE) plot_varname <- "var_kustannus"
       p1 <- create_plot(plot_varname)
      
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
        height = 16
        width = 16
      }
      
      if (TRUE) plot_varname <- "var_kustannus"
      
      p1 <- create_plot(plot_varname)
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
        height = 16
        width = 16
      }
      
      if (TRUE) plot_varname <- "var_kustannus"

      p1 <- create_plot(plot_varname)
      ggsave(file, plot = p1, #device = cairo_pdf, 
             width = width, height = height, limitsize = FALSE)
    }
  )
  
  ### luo_navigaatio ----
  # create_navigation <- function()
  create_navigation <- function(lang){
    
    
    if (lang == "fi"){
      
      taglst <-  tagList(
        tags$html(HTML('
    <a class="sr-only sr-only-focusable" href="#content">Siirry sisältöön</a>

    <nav class="navbar navbar-kela bg-kela navbar-light sticky-top">
    <!--div class="container"-->
      <a class="navbar-brand" role="brand" href="#"><img src = "https://www.kela.fi/image/layout_set_logo?img_id=2174196&t=1585229282595" style = "height: 35px; padding-right: 0px;" alt = "Kelan logo"></a>
      <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarResponsive" aria-controls="navbarResponsive" aria-expanded="false" aria-label="Avaa valikko">
        <span class="navbar-toggler-icon"></span>
      </button>
      <div role = "navigation" class="collapse navbar-collapse justify-content-between" id="navbarResponsive">
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
        </ul>
      </div>
    <!--/div-->
  </nav>'))
      )
    } else if (lang == "en"){
      
      taglst <-  tagList(
        tags$html(HTML('
    <a class="sr-only sr-only-focusable" href="#content">Siirry sisältöön</a>

    <nav class="navbar navbar-kela bg-kela navbar-light sticky-top">
    <!--div class="container"-->
      <a class="navbar-brand" role="brand" href="#"><img src = "https://www.kela.fi/image/layout_set_logo?img_id=2174196&t=1585229282595" style = "height: 35px; padding-right: 0px;" alt = "Kelan logo"></a>
      <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarResponsive" aria-controls="navbarResponsive" aria-expanded="false" aria-label="Avaa valikko">
        <span class="navbar-toggler-icon"></span>
      </button>
      <div role = "navigation" class="collapse navbar-collapse justify-content-between" id="navbarResponsive">
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
        </ul>
      </div>
    <!--/div-->
  </nav>'))
      )
    } else if (lang == "sv"){
      
      taglst <-  tagList(
        tags$html(HTML('
    <a class="sr-only sr-only-focusable" href="#content">Siirry sisältöön</a>

    <nav class="navbar navbar-kela bg-kela navbar-light sticky-top">
    <!--div class="container"-->
      <a class="navbar-brand" role="brand" href="#"><img src = "https://www.kela.fi/image/layout_set_logo?img_id=2174196&t=1585229282595" style = "height: 35px; padding-right: 0px;" alt = "Kelan logo"></a>
      <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarResponsive" aria-controls="navbarResponsive" aria-expanded="false" aria-label="Avaa valikko">
        <span class="navbar-toggler-icon"></span>
      </button>
      <div role = "navigation" class="collapse navbar-collapse justify-content-between" id="navbarResponsive">
        <ul class="navbar-nav ml-auto">
          <li class="nav-item">
            <a class="nav-link" href="#ohje">Instruktioner</a>
          </li>
          <li class="nav-item">
            <a class="nav-link" href="#info">Källkod</a>
          </li>
          <li class="nav-item">
            <a class="nav-link" href="#saavutettavuus">Tillgänglighet</a>
          </li>
        </ul>
      </div>
    <!--/div-->
  </nav>'))      )
    }
    
    return(taglst) 
  }
  
  output$ui_navigation <- renderUI({
    
    req(input$selected_language)
    
    taglst <- create_navigation(lang = input$selected_language)
    tagList(
      taglst
    )
  })
  
  
  
  ### luo_infoteksti ----
  create_info_text <- function(lang){
    
    if (lang == "fi"){
      
      taglst <-  tagList(
        tags$h1("Reseptilääkkeiden ostot ATC-luokittain", id = "ohje"), 
        tags$p("Voit verrata sairausvakuutuksesta korvattavien reseptilääkkeiden kustannuksia, ostomääriä ja ostajien määriä viikkotasolla vuosien 2019 ja 2020 välillä ATC-luokituksen tasoilla 1-5. Tiedot perustuvat apteekkien päivittäin Kelaan toimittamiin ostotietoihin. Aineistosta on poistettu ne lääkeaineet, joita on yhtenä tai useampana viikkona ostanut alle 10 henkilöä."),
        tags$h2("Näin käytät sovellusta"),
        tags$p("Alkunäkymässä on kaikkien ATC-luokkien yhteenlasketut tiedot.",tags$br(),
               "Sovelluksessa on kaksi hakuvaihtoehtoa: ",tags$strong("valikkohaku"),"ja",tags$strong("tekstihaku."), "Valikkohaku etenee hierkkisesti ATC-tasoja ylhäältä alas. Tekstihaussa voit hakea yhdellä tai useammalla hakutermillä kaikilta ATC-tasoilta. Molemmissa hakutyypeissä voit valita tarkasteluun yhden tai useamman luokan.",
               tags$br(),
               "Voit valita muuttujaksi joko kustannukset, ostomäärät tai ostajien määrät. Voit tallentaa sekä kuvion että datan laitteellesi. Datassa näytetään aina kaikki muuttujat.",tags$br(),tags$br(),
               "Datan dokumentaatio ja latauslinkit löytyvät",tags$a(href ='https://github.com/kelaresearchandanalytics/korona_atc_data', "Githubista."))
      )
    } else if (lang == "en"){
      
      taglst <-  tagList(
        tags$h1("Purchased prescription medicines in Finland", id = "ohje"),
        tags$p("This application provides information on reimbursable prescription medicines in terms of costs, number of purchases and number of patients on a weekly basis in 2019 and 2020. Medicines are classified in groups at five levels of the ATC-classification system. Information is based on daily data provided by Finnish community pharmacies. ATC groups including less than 10 patients in one or more weeks have been removed from the data."),
        tags$h2("Instructions"),
        tags$p("Results including all ATC groups are shown combined on a start screen.",tags$br(),
               "There are two search options: hierarchical ",tags$strong("menu search"),"and text-based",tags$strong("text search."), "One or multiple ATC groups may be chosen at the same time.",
               tags$br(),
               "You can choose a variable from  costs, number of purchases and number of patients. You can save data and images on your device .",tags$br(),tags$br(),
               "Documentation and upload links are in ",tags$a(href ='https://github.com/kelaresearchandanalytics/korona_atc_data', "Github."))
      )
    } else if (lang == "sv"){
      
      taglst <-  tagList(
        tags$h1("Receptbelagda läkemedel enligt ATC-systemet", id = "ohje"),
        tags$p("Rapporter innehåller veckoliga kostnaderna och antalet inköp av läkemedel som har ersatts på apoteken, och antal personer som har köpt dessa läkemedel i 2019 och 2020. Läkemedel klassificeras enligt ATC-systemet i fem olika nivåer. Data kommer från apotek. Rapporten innehåller inte läkemedelsgrupper som bestått av färre än 10 personer i någon vecka."),
        tags$h2("Så här använder du applikationen"),
        tags$p("På startsidan finns alla ATC grupper tillsammans.",tags$br(),
               "Du kan använda två olika sökalternativ: hierarkisk ",tags$strong("meny sök"),"och textbaserad",tags$strong("text sök,"), " och välja en eller flera läkemedelsgrupper samtidigt.",
               tags$br(),
               "Du kan välja kostnader, antal inköp eller antal köpare som en variabel. Du kan spara data och diagram på din enheten.",tags$br(),tags$br(),
               "Dokumentation och data finns på ",tags$a(href ='https://github.com/kelaresearchandanalytics/korona_atc_data', "Github"))
      )
    }
    
    return(taglst) 
  }
  
  output$ui_info_text <- renderUI({
    
    req(input$selected_language)
    
    taglst <- create_info_text(lang = input$selected_language)
    tagList(
      taglst
    )
  })
  
  create_atc_box <- function(lang){
    
    if (lang == "fi"){
      
      taglst <-  tagList(
        tags$div(class = "card",
                 tags$h5("ATC-luokitus", class = "card-header"),
                 tags$div(class = "card-body",
                          
                          tags$p("ATC-luokituksessa luokituksessa lääkkeet on jaettu ryhmiin sen mukaan, mihin elimeen tai elinjärjestelmään ne vaikuttavat sekä niiden kemiallisten, farmakologisten ja terapeuttisten ominaisuuksien mukaan.", class = "card-text"),
                          tags$p("ATC-järjestelmässä lääkkeet on luokiteltu ryhmiin viiteen eri tasoon. Lääkkeet on jaoteltu 14 pääryhmään (1. taso) ja edelleen neljään alatasoon. Näistä 2. ja 3. taso ovat terapeuttisia/ farmakologisia alaryhmiä, 4. taso ilmaisee joko farmakologisen, kemiallisen tai terapeuttisen ryhmän, johon lääke kuuluu ja 5. taso yksittäisen kemiallisen aineen tai yhdistelmävalmisteen aineyhdistelmän.", class = "card-text"),
                          tags$a(href = "https://www.fimea.fi/laakehaut_ja_luettelot/atc-luokitus",
                                 "Lue lisää: ATC-luokitus, Fimea",
                                 target="_blank", class = "btn btn-secondary")
                 )
        )
      )
    } else if (lang == "en"){
      
      taglst <-  tagList(
        tags$div(class = "card",
                 tags$h5("ATC system", class = "card-header"),
                 tags$div(class = "card-body",
                          
                          tags$p("In the Anatomical Therapeutic Chemical (ATC) classification, drugs are divided into groups according to which organ or organ system they affect and according to their chemical, pharmacological and therapeutic properties.", class = "card-text"),
                          tags$p("In the ATC system, drugs are grouped into five different levels. Medicines are divided into 14 main categories (Level 1) and further into four levels. Of these, Levels 2 and 3 are therapeutic / pharmacological subgroups, level 4 indicates either pharmacological, chemical or therapeutic group, to which the drug belongs and level 5 of a single chemical substance or combination. In the second, third and fourth levels, pharmacological subgroups are used for identification when considered more appropriate than therapeutic or chemical subgroups.", class = "card-text"),
                          tags$a(href = "https://www.fimea.fi/web/en/databases_and_registers/atc-codes",
                                 "Read more: ATC system, Fimea",
                                 target="_blank", class = "btn btn-secondary")
                 ))
      )
    } else if (lang == "sv"){
      
      taglst <-  tagList(
        tags$div(class = "card",
                 tags$h5("ATC-systemet", class = "card-header"),
                 tags$div(class = "card-body",
                          tags$p("I klassificeringen Anatomical Therapeutic Chemical (ATC) är läkemedel uppdelade i grupper beroende på vilket organ eller organsystem de påverkar och enligt deras kemiska, enligt dess farmakologiska och terapeutiska egenskaper.", class = "card-text"),
                          tags$p("I ATC-systemet grupperas läkemedel i fem olika nivåer. Läkemedel är indelade i 14 huvudkategorier (Nivå 1) och upp till fyra nivåer. Av dessa är nivåer 2 och 3 terapeutiska / farmakologiska undergrupper, nivå 4 representerar antingen en farmakologisk, kemisk eller terapeutisk grupp, som läkemedlet tillhör och nivå 5 av en enda kemikalie eller kombinationsprodukt. I den andra, tredje och fjärde nivån används farmakologiska undergrupper för identifiering där det anses vara mer lämpligt än terapeutiska eller kemiska undergrupper.", class = "card-text"),
                          tags$a(href = "https://www.fimea.fi/web/sv/soktjanster_och_forteckningar/atc-kod", 
                                 "Läs mer: ATC-kod, Fimea",
                                 target="_blank", class = "btn btn-secondary")
                 ))
      )
    }
    
    return(taglst) 
  }
  
  output$ui_atc_box <- renderUI({
    
    req(input$selected_language)
    
    taglst <- create_atc_box(lang = input$selected_language)
    tagList(
      taglst
    )
  })
  
  create_accessibility_statement <- function(lang){
    
    
    if (lang == "fi"){
      
      taglst <-  tagList(
        tags$html(HTML('
<h2 id = "saavutettavuus">Saavutettavuusseloste</h2>
<p>Tämä saavutettavuusseloste koskee Reseptilääkkeiden ostot ATC-luokittain -verkkopalvelua. Seloste on laadittu 22.2.2021. Verkkosivuston saavutettavuus on arvioitu Kelassa.</p>
<h3>Miten saavutettava verkkopalvelu on?</h3>
<p>Reseptilääkkeiden ostot ATC-luokittain -verkkopalvelu on uudistettu keväällä 2021. Saavutettavuusvaatimukset on huomioitu, ja palvelu täyttää saavutettavuusvaatimukset (WCAG-kriteeristö 2.1 A- ja AA-tasot).</p>
<h3>Anna palautetta saavutettavuudesta</h3>
<ul>
<li><a href="https://beta.kela.fi/saavutettavuuspalaute">verkkolomakkeella.</a></li>
</ul>
<p>Saavutettavuuspalautteet Kelassa vastaanottaa Kelan tekninen tuki.</p>
<h3>Saavutettavuuden valvonta</h3>
<p>Jos huomaat sivustolla saavutettavuuteen liittyviä ongelmia, anna ensin palautetta meille. Vastaamme 2 viikon sisällä.</p>
<p>Jos et ole tyytyväinen saamaasi vastaukseen tai jos et saa vastausta 2 viikon aikana, <a href="https://www.saavutettavuusvaatimukset.fi/oikeutesi/">voit tehdä ilmoituksen Etelä-Suomen aluehallintovirastoon</a>. Etelä-Suomen aluehallintoviraston sivulla kerrotaan tarkasti, miten voit tehdä ilmoituksen ja miten asia käsitellään.</p>
<h3>Valvontaviranomaisen yhteystiedot</h3>
<p><strong>Etelä-Suomen aluehallintovirasto</strong><br>
Saavutettavuuden valvonnan yksikkö<br>
<a href="https://www.saavutettavuusvaatimukset.fi/">www.saavutettavuusvaatimukset.fi&nbsp;</a><br>
saavutettavuus(at)avi.fi<br>
puhelinnumero vaihde 0295 016 000</p>
<h3>Teemme jatkuvasti työtä saavutettavuuden parantamiseksi</h3>
<p>Olemme sitoutuneet parantamaan verkkopalveluiden saavutettavuutta. Päivitämme tätä selostetta sitä mukaa kuin korjaamme puutteita.</p>
'))
      )    } else if (lang == "en"){
      
      taglst <-  tagList(
        tags$html(HTML('
<h2 id = "saavutettavuus">Accessibility statement</h2>
<p>This is accessibility statement for Purchased prescription medicines in Finland -web application. Seloste on laadittu 22.2.2021. Verkkosivuston saavutettavuus on arvioitu Kelassa.</p>
<h3>Miten saavutettava verkkopalvelu on?</h3>
<p>Reseptilääkkeiden ostot ATC-luokittain -verkkopalvelu on uudistettu keväällä 2021. Saavutettavuusvaatimukset on huomioitu, ja palvelu täyttää saavutettavuusvaatimukset (WCAG-kriteeristö 2.1 A- ja AA-tasot).</p>
<h3>Anna palautetta saavutettavuudesta</h3>
<ul>
<li><a href="https://beta.kela.fi/saavutettavuuspalaute">verkkolomakkeella.</a></li>
</ul>
<p>Saavutettavuuspalautteet Kelassa vastaanottaa Kelan tekninen tuki.</p>
<h3>Saavutettavuuden valvonta</h3>
<p>Jos huomaat sivustolla saavutettavuuteen liittyviä ongelmia, anna ensin palautetta meille. Vastaamme 2 viikon sisällä.</p>
<p>Jos et ole tyytyväinen saamaasi vastaukseen tai jos et saa vastausta 2 viikon aikana, <a href="https://www.saavutettavuusvaatimukset.fi/oikeutesi/">voit tehdä ilmoituksen Etelä-Suomen aluehallintovirastoon</a>. Etelä-Suomen aluehallintoviraston sivulla kerrotaan tarkasti, miten voit tehdä ilmoituksen ja miten asia käsitellään.</p>
<h3>Valvontaviranomaisen yhteystiedot</h3>
<p><strong>Etelä-Suomen aluehallintovirasto</strong><br>
Saavutettavuuden valvonnan yksikkö<br>
<a href="https://www.saavutettavuusvaatimukset.fi/">www.saavutettavuusvaatimukset.fi&nbsp;</a><br>
saavutettavuus(at)avi.fi<br>
puhelinnumero vaihde 0295 016 000</p>
<h3>Teemme jatkuvasti työtä saavutettavuuden parantamiseksi</h3>
<p>Olemme sitoutuneet parantamaan verkkopalveluiden saavutettavuutta. Päivitämme tätä selostetta sitä mukaa kuin korjaamme puutteita.</p>
                       '))
      )
    } else if (lang == "sv"){
      
      taglst <-  tagList(
        tags$html(HTML('
<h2 id = "saavutettavuus">Tillgänglighetsutlåtande</h2>
<p>Den är tillgänglighetsutlåtande för webbplatsen Receptbelagda läkemedel enligt ATC-systemet. Seloste on laadittu 22.2.2021. Verkkosivuston saavutettavuus on arvioitu Kelassa.</p>
<h3>Miten saavutettava verkkopalvelu on?</h3>
<p>Reseptilääkkeiden ostot ATC-luokittain -verkkopalvelu on uudistettu keväällä 2021. Saavutettavuusvaatimukset on huomioitu, ja palvelu täyttää saavutettavuusvaatimukset (WCAG-kriteeristö 2.1 A- ja AA-tasot).</p>
<h3>Anna palautetta saavutettavuudesta</h3>
<ul>
<li><a href="https://beta.kela.fi/saavutettavuuspalaute">verkkolomakkeella.</a></li>
</ul>
<p>Saavutettavuuspalautteet Kelassa vastaanottaa Kelan tekninen tuki.</p>
<h3>Saavutettavuuden valvonta</h3>
<p>Jos huomaat sivustolla saavutettavuuteen liittyviä ongelmia, anna ensin palautetta meille. Vastaamme 2 viikon sisällä.</p>
<p>Jos et ole tyytyväinen saamaasi vastaukseen tai jos et saa vastausta 2 viikon aikana, <a href="https://www.saavutettavuusvaatimukset.fi/oikeutesi/">voit tehdä ilmoituksen Etelä-Suomen aluehallintovirastoon</a>. Etelä-Suomen aluehallintoviraston sivulla kerrotaan tarkasti, miten voit tehdä ilmoituksen ja miten asia käsitellään.</p>
<h3>Valvontaviranomaisen yhteystiedot</h3>
<p><strong>Etelä-Suomen aluehallintovirasto</strong><br>
Saavutettavuuden valvonnan yksikkö<br>
<a href="https://www.saavutettavuusvaatimukset.fi/">www.saavutettavuusvaatimukset.fi&nbsp;</a><br>
saavutettavuus(at)avi.fi<br>
puhelinnumero vaihde 0295 016 000</p>
<h3>Teemme jatkuvasti työtä saavutettavuuden parantamiseksi</h3>
<p>Olemme sitoutuneet parantamaan verkkopalveluiden saavutettavuutta. Päivitämme tätä selostetta sitä mukaa kuin korjaamme puutteita.</p>
'))
      )
    }
    return(taglst) 
  }
  
  output$ui_accessibility_statement <- renderUI({
    
    req(input$selected_language)
    
    taglst <- create_accessibility_statement(lang = input$selected_language)
    tagList(
      taglst
    )
  })
  
  
  create_about_app <- function(lang){
    
    if (lang == "fi"){
      
      taglst <-  tagList(
        tags$h2("Lähdekoodi", id = "info"),
        tags$html(HTML('
<strong>Reseptilääkkeiden ostot ATC-luokittain -verkkosovellus</strong>
<p>Sovellusversio
<code>0.5.0</code><br/>
</p>
<p>Tämä verkkosovellus on tehty
<a href="https://www.r-project.org/">R</a>-kielellä
<a href="https://shiny.rstudio.com">Shiny</a>-kirjaston avulla. 
Sovelluksen lähdekoodi on avoimesti lisensöity ja saatavilla 
<a href="https://github.com/kelaresearchandanalytics/korona_atc_app">Github</a>-palvelusta.</p>

<p>Mikäli löysit sovelluksesta bugin tai sinulla on idea tai toteutus uudesta ominaisuudesta voit:</p>
<ul>
<li>
toteuttaa ominaisuuden/korjauksen ja jättää
<a href="https://github.com/kelaresearchandanalytics/korona_atc_app/pulls">pull requestin</a>Github-palvelussa,
</li>
<li>
avata uuden <a href="https://github.com/kelaresearchandanalytics/korona_atc_app/issues">issuen</a> Github-palvelussa 
ja kuvata bugin/ominaisuuden siinä tai
</li>
<li>
laittaa meiliä osoitteeseen 
<a href="mailto:markus.kainu@kela.fi"><code>markus.kainu@kela.fi</code></a>
</li>
</ul>
'))
        
        
        
      )
    } else if (lang == "en"){
      
      taglst <-  tagList(
        tags$h2("Source code", id = "info"),
        tags$html(HTML('
<strong>Purchased prescription medicines in Finland -web application</strong>
<p>Version
<code>0.5.0</code><br/>
</p>
<p>This applications is written using
<a href="https://www.r-project.org/">R</a>-language with 
<a href="https://shiny.rstudio.com">Shiny</a>-library. 
Source code is available free and open at 
<a href="https://github.com/kelaresearchandanalytics/korona_atc_app">Github</a>.</p>

<p>If you encoutered a bug or would like a new feature, please:</p>
<ul>
<li>
implementent the fix and leave
<a href="https://github.com/kelaresearchandanalytics/korona_atc_app/pulls">pull request</a> at Github,
</li>
<li>
create a <a href="https://github.com/kelaresearchandanalytics/korona_atc_app/issues">issue</a> at Github 
and describe the bug/feature in it
</li>
<li>
send email to
<a href="mailto:markus.kainu@kela.fi"><code>markus.kainu@kela.fi</code></a>
</li>
</ul>
'))
      )
    } else if (lang == "sv"){
      
      taglst <-  tagList(
        tags$h2("Källkod", id = "info"),
        tags$html(HTML('
<strong>Receptbelagda läkemedel enligt ATC-systemet -webbapplikation</strong>
<p>Version
<code>0.5.0</code><br/>
</p>
<p>This applications is written using
<a href="https://www.r-project.org/">R</a>-language with 
<a href="https://shiny.rstudio.com">Shiny</a>-library. 
Source code is available free and open at 
<a href="https://github.com/kelaresearchandanalytics/korona_atc_app">Github</a>.</p>

<p>If you encoutered a bug or would like a new feature, please:</p>
<ul>
<li>
implementent the fix and leave
<a href="https://github.com/kelaresearchandanalytics/korona_atc_app/pulls">pull request</a> at Github,
</li>
<li>
create a <a href="https://github.com/kelaresearchandanalytics/korona_atc_app/issues">issue</a> at Github 
and describe the bug/feature in it
</li>
<li>
send email to
<a href="mailto:markus.kainu@kela.fi"><code>markus.kainu@kela.fi</code></a>
</li>
</ul>
'))
      )
    }
    return(taglst) 
  }
  
  output$ui_about_app <- renderUI({
    
    req(input$selected_language)
    
    taglst <- create_about_app(lang = input$selected_language)
    tagList(
      taglst
    )
  })
  
}
)


