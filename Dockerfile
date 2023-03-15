FROM rocker/r-ver

# basic shiny functionality
RUN R -q -e "install.packages(c('shiny', 'rmarkdown'))"

# additional shiny functionality
RUN R -q -e "install.packages(c('shinydashboard', 'shinydashboardPlus', 'shinyWidgets'))"
RUN R -q -e "install.packages(c('fresh', 'shinycssloaders'))"

# other R packages
RUN R -q -e "install.packages(c('data.table', 'DT', 'Rnumerai'))"

# copy the app to the image
WORKDIR /shinyapp
COPY --link Rprofile.site /usr/local/lib/R/etc/
COPY --link app /shinyapp/

EXPOSE 7860
CMD ["R", "-q", "-e", "shiny::runApp('/shinyapp')"]
