FROM rocker/shiny-verse:4.1.1
RUN install2.r rsconnect dplyr ggplot2 glue httr metathis patchwork shiny shiny.i18n shinycssloaders shinyWidgets svglite tidyr ragg RcppArmadillo Rttf2pt1 e1071 classInt extrafont extrafontdb gdtools geofacet geogrid hrbrthemes
WORKDIR ~/btsync/kela/reseptilaakesovellus
COPY ui.R ui.R
COPY server.R server.R
COPY global.R global.R
COPY pkgs.R pkgs.R
# COPY .Renviron .Renviron
COPY www/ ./www
COPY translations/ ./translations
COPY deploy.R deploy.R
CMD Rscript deploy.R
