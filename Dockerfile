FROM rocker/r-ver:4.2.3

# Remotes
RUN R -q -e "install.packages(c('remotes'))"

# Specific version of Shiny
RUN R -q -e "remotes::install_version('shiny', version = '1.7.5.1', repos = 'http://cran.us.r-project.org')"

# basic shiny functionality
RUN R -q -e "install.packages(c('rmarkdown', 'markdown'))"

# additional shiny functionality
RUN R -q -e "install.packages(c('shinydashboard', 'shinydashboardPlus'))"
RUN R -q -e "install.packages(c('shinyWidgets', 'shinycssloaders'))"

# other R packages
RUN R -q -e "install.packages(c('DT', 'plotly', 'scico', 'ggthemes', 'scales', 'wesanderson'))"
RUN R -q -e "install.packages(c('data.table', 'dtplyr', 'devtools', 'googlesheets4'))"

# modified version of Rnumerai
RUN R -q -e "devtools::install_github('woobe/Rnumerai')"

# copy the app to the image
WORKDIR /shinyapp
COPY --link Rprofile.site /usr/local/lib/R/etc/
COPY --link app /shinyapp/

EXPOSE 7860
CMD ["R", "-q", "-e", "shiny::runApp('/shinyapp')"]
