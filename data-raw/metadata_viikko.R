## code to prepare `metadata_viikko` dataset goes here

metadata_viikko <- readr::read_csv2('~/tutkimus/laaketutkimus/korona_atc_data/metadata_viikko_hva.csv')
usethis::use_data(metadata_viikko, overwrite = TRUE)

# document
karttasovellus::document_data(dat = metadata_viikko, 
                              neim = "metadata_viikko", 
                              description = paste("Githubista", Sys.Date(), "haettu metadata"))
