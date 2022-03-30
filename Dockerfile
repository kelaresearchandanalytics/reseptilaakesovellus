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

## FROM rocker/shiny-verse:latest

## COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
## RUN chown -R shiny /var/lib/shiny-server/

# OpenShift gives a random uid for the user and some programs try to find a username from the /etc/passwd.
# Let user to fix it, but obviously this shouldn't be run outside OpenShift
## RUN chmod ug+rw /etc/passwd
## COPY fix-username.sh /usr/bin/fix-username.sh
## COPY shiny-server.sh /usr/bin/shiny-server.sh
## RUN chmod a+rx /usr/bin/shiny-server.sh

# Make sure the directory for individual app logs exists and is usable
## RUN chmod -R a+rwX /var/log/shiny-server
## RUN chmod -R a+rwX /var/lib/shiny-server

#FROM rocker/shiny-verse:latest
#RUN apt-get update && apt-get install -y  gdal-bin git-core libcairo2-dev libcurl4-openssl-dev libfribidi-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libharfbuzz-dev libicu-dev libpng-dev libproj-dev libssl-dev libtiff-dev libudunits2-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
#RUN install2.r here rsconnect magrittr glue bslib httr pkgload ggplot2 dplyr shiny config testthat tidyr svglite shinyWidgets shinycssloaders shiny.i18n readr ragg patchwork metathis hrbrthemes golem geofacet remotes
#RUN mkdir /build_zone
#ADD . /srv/code/reseptilaakesovellus
#WORKDIR /srv/code/reseptilaakesovellus
#RUN R -e 'remotes::install_local(upgrade="never")'
#RUN rm -rf /build_zone
#ADD . /home/shinyusr
#WORKDIR /home/shinyusr
#CMD Rscript deploy.R
#EXPOSE 80
#CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');reseptilaakesovellus::run_app()"

#CMD /usr/bin/shiny-server.sh
