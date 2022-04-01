FROM nexus.kela.fi:18444/muuankarski/shinyversegeospatial:latest

COPY inst/openshift/shiny-server.conf /etc/shiny-server/shiny-server.conf
RUN chown -R shiny /var/lib/shiny-server/

# OpenShift gives a random uid for the user and some programs try to find a username from the /etc/passwd.
# Let user to fix it, but obviously this shouldn't be run outside OpenShift
RUN chmod ug+rw /etc/passwd
COPY inst/openshift/fix-username.sh /usr/bin/fix-username.sh
COPY inst/openshift/shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod a+rx /usr/bin/shiny-server.sh

# Make sure the directory for individual app logs exists and is usable
RUN chmod -R a+rwX /var/log/shiny-server
RUN chmod -R a+rwX /var/lib/shiny-server

RUN echo 'options(repos = c(CRAN = "https://nexus.kela.fi/repository/r-public/"))' >> ${R_HOME}/etc/Rprofile.site
#RUN install2.r metathis shinyWidgets remotes ggplot2 glue haven processx tidyr shinycssloaders htmltools pkgload testthat config spelling golem
#RUN install2.r plyr

RUN install2.r rsconnect magrittr glue bslib httr pkgload ggplot2 dplyr shiny config testthat tidyr svglite shinyWidgets shinycssloaders shiny.i18n readr ragg patchwork metathis hrbrthemes golem geofacet remotes

ADD . /srv/code

RUN chown -R shiny /srv/code/
RUN chmod -R a+rwX /srv/code/

WORKDIR /srv/code
RUN R -e 'remotes::install_local(upgrade="never")'

CMD /usr/bin/shiny-server.sh

