


#' create_navigation
#' 
#' @export
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


#' create_atc_box
#' 
#' @export
create_atc_box <- function(lang){
  
  if (lang == "fi"){
    
    taglst <-  tagList(
      tags$div(class = "card",
               tags$strong("ATC-luokitus", class = "card-header lead"),
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
               tags$strong("ATC system", class = "card-header lead"),
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
               tags$strong("ATC-systemet", class = "card-header lead"),
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




#' create_accessibility_statement
#' 
#' @export
create_accessibility_statement <- function(lang){
  
  
  if (lang == "fi"){
    
    taglst <-  tagList(
      tags$html(HTML('
<h2 id = "saavutettavuus">Saavutettavuusseloste</h2>
<p>Tämä saavutettavuusseloste koskee Reseptilääkkeiden ostot ATC-luokittain -verkkopalvelua. Seloste on laadittu 9.4.2021. Verkkosivuston saavutettavuus on arvioitu Kelassa.</p>
<h3>Miten saavutettava verkkopalvelu on?</h3>
<p>Reseptilääkkeiden ostot ATC-luokittain -verkkopalvelu on uudistettu keväällä 2021. Saavutettavuusvaatimukset on huomioitu, ja palvelu täyttää kriittiset saavutettavuusvaatimukset (WCAG-kriteeristö 2.1 A- ja AA-tasot).</p>

<h3>Sisällöt, jotka eivät ole saavutettavia</h3>
<p>Käyttäjät saattavat edelleen kohdata sivustolla joitakin saavutettavuusongelmia.
Seuraavana on luettelo ongelmista, jotka ovat tiedossamme.
Jos huomaat sivustolla ongelman, joka ei ole luettelossa, ilmoitathan siitä meille.</p>
<p>Sisällöt tai toiminnot, jotka eivät ole vielä täysin saavutettavia:</p>
<ul>
<li>Sovelluksen piirtämän kuvan tekstivastine on puutteellinen. (WCAG 1.1.1)</li>
</ul>
<p>Korjaamme yllä listatut puutteet kevään 2021 aikana.</p>

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
<p>This is accessibility statement for Purchased prescription medicines in Finland -web application.
Seloste on laadittu 9.4.2021. The assessment was carried out by Kela.</p>

<p>Purchased prescription medicines in Finland -web application was update in Spring 2021.
Our application meets the critical A and AA level accessibility criteria (WCAG criteria 2.1).</p>

<h3>Parts of the content that are not accessible</h3>
<p>Users may still encounter some accessibility issues on the website. The known issues are listed below.
If you encounter a website issue not listed below, please tell us about it.</p>
<p>Content and functions not yet fully accessible:</p>
<ul>
<li>The text alternative for the image created by application is not sufficient. (WCAG 1.1.1)</li>
</ul>
<p>We will correct the shortcomings listed above in spring 2021.</p>

<h3>Give us feedback on the accessibility</h3>
<p>Did you find an accessibility issue in our service? Please let us know about it, and we will do our best to correct it.</p>
<p>To give feedback on the accessibility, please use the</p>
<ul>
<li><a href="https://beta.kela.fi/saavutettavuuspalaute" target="_blank">online form (Opens in a new tab)</a></li>
</ul>
<p>Feedback on the accessibility will be received and addressed by the Kela technical support.</p>
<h3>Accessibility monitoring</h3>
<p>If you notice any accessibility issues on the website, please send feedback to us first. We will respond within two weeks.</p>
<p>If you are not satisfied with the response you have received or do not receive a response within two weeks,<a href="https://www.eu-healthcare.fi/recommends/saavutettavuusvaatimukset-aluehallintovirasto/" data-eafl-id="917192" class="eafl-link eafl-link-text eafl-link-cloaked" target="_blank"> you may file a report with the Regional State Administrative Agency for Southern Finland (site in Finnish and Swedish only)<span class="screen-reader-text"> (Opens in a new tab)</span><span class="icon-ext-link" aria-hidden="true"></span></a>. The website of the Regional State Administrative Agency for Southern Finland provides detailed information on how to file a report and how the matter will be processed.</p>
<h4>Contact information for the supervisory authority:</h4>
<p>Regional State Administrative Agency for Southern Finland<br>
Accessibility monitoring unit<br>
<a href="https://www.eu-healthcare.fi/recommends/saavutettavuusvaatimukset-aluehallintovirasto/" data-eafl-id="917192" class="eafl-link eafl-link-text eafl-link-cloaked" target="_blank">www.saavutettavuusvaatimukset.fi (Opens in a new tab)</a><br>
saavutettavuus@avi.fi<br>
Telephone (switchboard): +358 295 016 000</p>
'))
      )
    } else if (lang == "sv"){
      
      taglst <-  tagList(
        tags$html(HTML('
<h2 id = "saavutettavuus">Tillgänglighetsutlåtande</h2>
<p>Detta tillgänglighetsutlåtande gäller för webbplatsen Receptbelagda läkemedel enligt ATC-systemet.
Utlåtandet beskriver situationen per den 9.4.2021. Bedömningen har gjorts av Fpa.</p>

<h3>Otillgängliga innehåll</h3>
<p>Användarna kan fortfarande stöta på vissa tillgänglighetsproblem på webbplatsen. Nedan följer en lista över de problem som vi har vetskap om. Om du upptäcker ett problem på webbplatsen, som inte finns med på listan, vänligen meddela oss om detta.</p>
<p>Innehåll eller funktioner vars tillgänglighet ännu är begränsad:</p>
<ul>
<li>Bilden saknas rätt textalternativ. (WCAG 1.1.1)</li>
</ul>
<p>Vi åtgärdar de ovan nämnda bristerna under våren 2021.</p>

<h3>Ge oss respons på tillgängligheten</h3>
<p>Upptäckte du någon brist i tjänstens tillgänglighet? Låt oss veta om det så gör vi vårt bästa för att åtgärda bristen.</p>
<p>Ge oss respons på tillgängligheten</p>
<ul>
<li><a href="https://beta.kela.fi/saavutettavuuspalaute" target="_blank">med webbformuläret  (öppnas i en ny flik)</a></li>
</ul>
<p>Tillgänglighetsrespons till FPA tas emot av den tekniska supporten.</p>
<h3>Tillsyn av tillgängligheten</h3>
<p>Om du upptäcker problem med webbplatsens tillgänglighet, börja med att skicka respons till oss. Vi svarar inom två veckor.</p>
<p>Om du inte är nöjd med svaret som du fått eller om du inte får något svar alls inom två veckor <a href="https://www.eu-halsovard.fi/recommends/tillganglighetskrav-regionforvaltningsverket/" data-eafl-id="917194" class="eafl-link eafl-link-text eafl-link-cloaked" target="_blank">kan du göra en anmälan till regionförvaltningsverket i Södra Finland<span class="screen-reader-text"> (öppnas i en ny flik)</span><span class="icon-ext-link" aria-hidden="true"></span></a>. På regionförvaltningsverket i Södra Finlands webbplats finns exakta anvisningar om hur du gör en anmälan och hur ärendet hanteras.</p>
<h4>Kontaktuppgifter till tillsynsmyndigheten:</h4>
<p>Regionförvaltningsverket i Södra Finland<br>
Enheten för tillgänglighetstillsyn<br>
<a href="https://www.eu-halsovard.fi/recommends/tillganglighetskrav-regionforvaltningsverket/" target="_blank">www.tillganglighetskrav.fi  (öppnas i en ny flik) </a><br>
saavutettavuus@avi.fi<br>
Telefon växel: 0295 016 000</p>

'))
      )
    }
  return(taglst)
}



#' create_about_app
#' 
#' @export
create_about_app <- function(lang, source_ver = golem::get_golem_version()){
  
  if (lang == "fi"){
    
    taglst <-  tagList(
      tags$h2("Lähdekoodi", id = "info"),
      tags$html(HTML(glue('
<strong>Reseptilääkkeiden ostot ATC-luokittain -verkkosovellus</strong>
<p>Sovellusversio
<code>{source_ver}</code><br/>
</p>
<p>Tämä verkkosovellus on tehty
<a href="https://www.r-project.org/">R</a>-kielellä
<a href="https://shiny.rstudio.com">Shiny</a>-kirjaston avulla.
Sovelluksen lähdekoodi on avoimesti lisensöity ja saatavilla
<a href="https://github.com/kelaresearchandanalytics/reseptilaakesovellus">Github</a>-palvelusta.</p>

<p>Mikäli löysit sovelluksesta bugin tai sinulla on idea tai toteutus uudesta ominaisuudesta voit:</p>
<ul>
<li>
toteuttaa ominaisuuden/korjauksen ja jättää
<a href="https://github.com/kelaresearchandanalytics/reseptilaakesovellus/pulls">pull requestin</a>  Github-palvelussa,
</li>
<li>
avata uuden <a href="https://github.com/kelaresearchandanalytics/reseptilaakesovellus/issues">issuen</a> Github-palvelussa
ja kuvata bugin/ominaisuuden siinä tai
</li>
<li>
laittaa sähköpostia osoitteeseen
<a href="mailto:markus.kainu@kela.fi">markus.kainu@kela.fi</a>
</li>
</ul>
')))
      
      
      
    )
  } else if (lang == "en"){
    
    taglst <-  tagList(
      tags$h2("Source code", id = "info"),
      tags$html(HTML(glue('
<strong>Purchased prescription medicines in Finland -web application</strong>
<p>Version
<code>{source_ver}</code><br/>
</p>
<p>This applications is written using
<a href="https://www.r-project.org/">R</a>-language with
<a href="https://shiny.rstudio.com">Shiny</a>-library.
Source code is available free and open at
<a href="https://github.com/kelaresearchandanalytics/reseptilaakesovellus">Github</a>.</p>

<p>If you encoutered a bug or would like a new feature, please:</p>
<ul>
<li>
implementent the fix and leave
<a href="https://github.com/kelaresearchandanalytics/reseptilaakesovellus/pulls">pull request</a> at Github,
</li>
<li>
create a <a href="https://github.com/kelaresearchandanalytics/reseptilaakesovellus/issues">issue</a> at Github
and describe the bug/feature in it
</li>
<li>
send email to
<a href="mailto:markus.kainu@kela.fi">markus.kainu@kela.fi</a>
</li>
</ul>
')))
    )
  } else if (lang == "sv"){
    
    taglst <-  tagList(
      tags$h2("Källkod", id = "info"),
      tags$html(HTML(glue('
<strong>Receptbelagda läkemedel enligt ATC-systemet -webbapplikation</strong>
<p>Version
<code>{source_ver}</code><br/>
</p>
<p>This applications is written using
<a href="https://www.r-project.org/">R</a>-language with
<a href="https://shiny.rstudio.com">Shiny</a>-library.
Source code is available free and open at
<a href="https://github.com/kelaresearchandanalytics/reseptilaakesovellus">Github</a>.</p>

<p>If you encoutered a bug or would like a new feature, please:</p>
<ul>
<li>
implementent the fix and leave
<a href="https://github.com/kelaresearchandanalytics/reseptilaakesovellus/pulls">pull request</a> at Github,
</li>
<li>
create a <a href="https://github.com/kelaresearchandanalytics/reseptilaakesovellus/issues">issue</a> at Github
and describe the bug/feature in it
</li>
<li>
send email to
<a href="mailto:markus.kainu@kela.fi">markus.kainu@kela.fi</a>
</li>
</ul>
')))
    )
  }
  return(taglst)
}



#' create_info_text
#' 
#' @export
create_info_text <- function(lang){
  
  if (lang == "fi"){
    
    taglst <-  tagList(
      tags$h1("Reseptilääkkeiden ostot ATC-luokittain", id = "ohje"),
      tags$p("Voit verrata sairausvakuutuksesta korvattavien reseptilääkkeiden kustannuksia, ostomääriä ja ostajien määriä viikkotasolla vuosien 2019-2021 välillä ATC-luokituksen tasoilla 1-5. Tiedot perustuvat apteekkien päivittäin Kelaan toimittamiin ostotietoihin. Aineistosta on poistettu ne lääkeaineet, joita on yhtenä tai useampana viikkona ostanut alle 10 henkilöä."),
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
      tags$p("This application provides information on reimbursable prescription medicines in terms of costs, number of purchases and number of patients on a weekly basis in 2019-2021. Medicines are classified in groups at five levels of the ATC-classification system. Information is based on daily data provided by Finnish community pharmacies. ATC groups including less than 10 patients in one or more weeks have been removed from the data."),
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
      tags$p("Rapporter innehåller veckoliga kostnaderna och antalet inköp av läkemedel som har ersatts på apoteken, och antal personer som har köpt dessa läkemedel i 2019-2021. Läkemedel klassificeras enligt ATC-systemet i fem olika nivåer. Data kommer från apotek. Rapporten innehåller inte läkemedelsgrupper som bestått av färre än 10 personer i någon vecka."),
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

