# source("pkgs.R")
# lapply(pkgs, require, character.only = TRUE)

library('shiny')
library('shinyWidgets')
library('shinycssloaders')
library('shiny.i18n')
library('dplyr')
library('ggplot2')
library('tidyr')
library('glue')
library('httr')
library('patchwork')
library('svglite')
library('metathis')
library('ragg')


Sys.setlocale(locale="en_US.UTF-8")

options(scipen = 999)
options(shiny.useragg = TRUE)

app_ver <- tags$code("v0.5")
enableBookmarking(store = "server")


# random functions
add_line_break <- function(x = "very many many characters and words and sentences",
                           n = 20){
  y <- gsub(paste0("(.{1,",n,"})(\\s|$)"), "\\1<br/>", x)
  y <- sub("<br/>$", "", y)
  return(y)
}

add_line_break2 <- function(x = "very many many characters and words and sentences",
                            n = 20){
  y <- gsub(paste0("(.{1,",n,"})(\\s|$)"), "\\1\n", x)
  y <- sub("\n$", "", y)
  return(y)
}
# is.even
is.even <- function(x) x %% 2 == 0

atc_color_palette <- c('#1EE004','#AF6CE6','#F52087')

# ['#1b9e77','#d95f02','#7570b3']
