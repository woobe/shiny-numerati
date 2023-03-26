FROM rocker/r-ver:4.2.3

# basic shiny functionality
RUN R -q -e "install.packages(c('shiny', 'rmarkdown'))"

# additional shiny functionality
RUN R -q -e "install.packages(c('shinydashboard', 'shinydashboardPlus'))"
RUN R -q -e "install.packages(c('shinyWidgets', 'shinycssloaders'))"

# other R packages
RUN R -q -e "install.packages(c('DT', 'plotly', 'scico', 'ggthemes', 'scales', 'wesanderson'))"
RUN R -q -e "install.packages(c('data.table', 'dtplyr', 'parallel', 'Rnumerai'))"

# copy the app to the image
WORKDIR /shinyapp
COPY --link Rprofile.site /usr/local/lib/R/etc/
COPY --link app /shinyapp/

EXPOSE 7860
CMD ["R", "-q", "-e", "shiny::runApp('/shinyapp')"]
