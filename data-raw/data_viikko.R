## code to prepare `data_viikko` dataset goes here

data_viikko <- dplyr::bind_rows(
  # readr::read_csv2('https://github.com/kelaresearchandanalytics/korona_atc_data/raw/master/data_viikko_2019.csv'),
  # readr::read_csv2('https://github.com/kelaresearchandanalytics/korona_atc_data/raw/master/data_viikko_2020.csv'),
  # readr::read_csv2('https://github.com/kelaresearchandanalytics/korona_atc_data/raw/master/data_viikko_2021.csv'),
  # readr::read_csv2('https://github.com/kelaresearchandanalytics/korona_atc_data/raw/master/data_viikko_2022.csv')
  
  arrow::read_parquet('https://github.com/kelaresearchandanalytics/korona_atc_data/raw/master/data_viikko_hva_2019.parquet'),
  arrow::read_parquet('https://github.com/kelaresearchandanalytics/korona_atc_data/raw/master/data_viikko_hva_2020.parquet'),
  arrow::read_parquet('https://github.com/kelaresearchandanalytics/korona_atc_data/raw/master/data_viikko_hva_2021.parquet'),
  arrow::read_parquet('https://github.com/kelaresearchandanalytics/korona_atc_data/raw/master/data_viikko_hva_2022.parquet'),
  arrow::read_parquet('https://github.com/kelaresearchandanalytics/korona_atc_data/raw/master/data_viikko_hva_2023.parquet'),
  arrow::read_parquet('https://github.com/kelaresearchandanalytics/korona_atc_data/raw/master/data_viikko_hva_2024.parquet')
  
  # readr::read_csv2('https://github.com/kelaresearchandanalytics/korona_atc_data/raw/master/data_viikko_hva_2019.csv'),
  # readr::read_csv2('https://github.com/kelaresearchandanalytics/korona_atc_data/raw/master/data_viikko_hva_2020.csv'),
  # readr::read_csv2('https://github.com/kelaresearchandanalytics/korona_atc_data/raw/master/data_viikko_hva_2021.csv'),
  # readr::read_csv2('https://github.com/kelaresearchandanalytics/korona_atc_data/raw/master/data_viikko_hva_2022.csv'),
  # readr::read_csv2('https://github.com/kelaresearchandanalytics/korona_atc_data/raw/master/data_viikko_hva_2023.csv'),
  # readr::read_csv2('https://github.com/kelaresearchandanalytics/korona_atc_data/raw/master/data_viikko_hva_2024.csv')
  # readr::read_csv2('~/tutkimus/laaketutkimus/korona_atc_data/data_viikko_hva_2019.csv'),
  # readr::read_csv2('~/tutkimus/laaketutkimus/korona_atc_data/data_viikko_hva_2020.csv'),
  # readr::read_csv2('~/tutkimus/laaketutkimus/korona_atc_data/data_viikko_hva_2021.csv'),
  # readr::read_csv2('~/tutkimus/laaketutkimus/korona_atc_data/data_viikko_hva_2022.csv')  
  
) %>% arrange(VUOSI,VIIKKO)

arrow::write_parquet(data_viikko, sink = "./inst/parquet/data_viikko.parquet")
usethis::use_data(data_viikko, overwrite = TRUE)
# document
# karttasovellus::document_data(dat = data_viikko, 
#                               neim = "data_viikko", 
#                               description = paste("Githubista", Sys.Date(), "haettu data"))
