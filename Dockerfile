FROM rocker/r-ver

# basic shiny functionality
RUN R -q -e "install.packages(c('shiny', 'rmarkdown'))"

# other R packages
RUN R -q -e "install.packages(c('data.table', 'Rnumerai'))"

# copy the app to the image
WORKDIR /shinyapp
COPY --link Rprofile.site /usr/local/lib/R/etc/
COPY --link app /shinyapp/

EXPOSE 7860
CMD ["R", "-q", "-e", "shiny::runApp('/shinyapp')"]
