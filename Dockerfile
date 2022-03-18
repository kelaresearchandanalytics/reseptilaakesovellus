FROM rocker/r-ver:4.1.2
RUN apt-get update && apt-get install -y  gdal-bin git-core libcairo2-dev libcurl4-openssl-dev libfribidi-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libharfbuzz-dev libicu-dev libpng-dev libproj-dev libssl-dev libtiff-dev libudunits2-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
#RUN install2.r rsconnect dplyr ggplot2 glue httr metathis patchwork shiny shiny.i18n shinycssloaders shinyWidgets svglite tidyr ragg RcppArmadillo Rttf2pt1 e1071 classInt extrafont extrafontdb gdtools geofacet geogrid hrbrthemes leaflet leaflet.extras gt sf geofi httpcache
RUN install2.r rsconnect magrittr glue bslib httr pkgload ggplot2 dplyr shiny config testthat tidyr svglite shinyWidgets shinycssloaders shiny.i18n readr ragg patchwork metathis hrbrthemes golem geofacet remotes
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
ADD . /home/shinyusr
WORKDIR /home/shinyusr
#COPY app.R app.R
#COPY DESCRIPTION DESCRIPTION
#COPY R/ ./R
#COPY data/ ./data
#COPY man/ ./man
#COPY inst/ ./inst
#COPY deploy.R deploy.R
CMD Rscript deploy.R
