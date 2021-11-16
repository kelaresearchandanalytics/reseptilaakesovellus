FROM rocker/shiny:4.0.4
RUN install2.r rsconnect
RUN install2.r rsconnect dplyr ggplot2 glue httr metathis patchwork shiny shiny.i18n shinycssloaders shinyWidgets svglite tidyr
WORKDIR /home/shinyusr
COPY ui.R ui.R
COPY server.R server.R
COPY global.R global.R
COPY pkgs.R pkgs.R
COPY www ./
COPY translations ./
COPY deploy.R deploy.R
CMD Rscript deploy.R
