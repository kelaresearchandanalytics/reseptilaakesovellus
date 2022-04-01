FROM rocker/r-ver:4.1.2
RUN apt-get update && apt-get install -y  gdal-bin git-core libcairo2-dev libcurl4-openssl-dev libfribidi-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libharfbuzz-dev libicu-dev libpng-dev libproj-dev libssl-dev libtiff-dev libudunits2-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN install2.r rsconnect magrittr glue bslib httr pkgload ggplot2 dplyr shiny config testthat tidyr svglite shinyWidgets shinycssloaders shiny.i18n readr ragg patchwork metathis hrbrthemes golem geofacet remotes
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
ADD . /home/shinyusr
WORKDIR /home/shinyusr
CMD Rscript deploy.R