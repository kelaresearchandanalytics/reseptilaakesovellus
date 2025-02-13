
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Reseptilääkesovellus

<!-- badges: start -->
<!-- badges: end -->

Sovellus Kelan julkaisemaan lääkkeiden kulutusta käsittelevän datan
selaamiseen.

Sovellus lukee sivulla:
<https://github.com/kelaresearchandanalytics/korona_atc_data> julkaistua
dataa. Sovellus on julkaistu Kelan tutkimusblogissa:
<https://tutkimusblogi.kela.fi/arkisto/5254>

Suora linkki sovellukseen:
<https://kelaresearchandanalytics.shinyapps.io/korona_atc_app/>

## Asentaminen

Voit asentaa sovelluksen kehitysversion seuraavasti:

``` r
remotes::install_github("kelaresearchandanalytics/reseptilaakesovellus")
```

## Resursseja kehittämisen tueksi

### Shiny

- [Wickham (tekeillä) *Mastering Shiny*](https://mastering-shiny.org/)
- [Youtube-haku:
  “mastering+shiny”](https://www.youtube.com/results?search_query=mastering+shiny)

### Golem

- [`golem` homepage](https://thinkr-open.github.io/golem/)
- [Colin Fay & Co (2022) *Engineering Production-Grade Shiny
  Apps*](https://engineering-shiny.org/)
- [Youtube-haku:
  “golem+shiny”](https://www.youtube.com/results?search_query=golem+shiny)

## Viittaa

``` r
citation("reseptilaakesovellus")
#> To cite package 'reseptilaakesovellus' in publications use:
#> 
#>   Kainu M, Heino P, Kari H, Saastamoinen L, Koskinen H, Rättö H, Kurko
#>   T (2025). _reseptilaakesovellus: Reseptilääkkeiden ostot
#>   ATC-luokittain_. R package version 0.10.1,
#>   <https://github.com/kelaresearchandanalytics/reseptilaakesovellus>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {reseptilaakesovellus: Reseptilääkkeiden ostot ATC-luokittain},
#>     author = {Markus Kainu and Pekka Heino and Heini Kari and Leena Saastamoinen and Hanna Koskinen and Hanna Rättö and Terhi Kurko},
#>     year = {2025},
#>     note = {R package version 0.10.1},
#>     url = {https://github.com/kelaresearchandanalytics/reseptilaakesovellus},
#>   }
```

------------------------------------------------------------------------

- Lähdekoodi on lisensoitu
  [MIT-lisenssillä](https://opensource.org/licenses/MIT)
- © 2020-2025 Markus Kainu & Kela
