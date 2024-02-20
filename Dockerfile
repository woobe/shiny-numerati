FROM rocker/r-ver:4.2.3

# Remotes
RUN R -q -e "install.packages(c('remotes'))"

# Other Packages
RUN R -q -e "remotes::install_version('xfun', version = '0.42', repos = 'http://cran.us.r-project.org')"
RUN R -q -e "remotes::install_version('fansi', version = '1.0.6', repos = 'http://cran.us.r-project.org')"
RUN R -q -e "remotes::install_version('xopen', version = '1.0.0', repos = 'http://cran.us.r-project.org')"
RUN R -q -e "remotes::install_version('rlang', version = '1.1.3', repos = 'http://cran.us.r-project.org')"
RUN R -q -e "remotes::install_version('memoise', version = '2.0.1', repos = 'http://cran.us.r-project.org')"
RUN R -q -e "remotes::install_version('rematch2', version = '2.1.2', repos = 'http://cran.us.r-project.org')"
RUN R -q -e "remotes::install_version('yaml', version = '2.3.8', repos = 'http://cran.us.r-project.org')"
RUN R -q -e "remotes::install_version('gert', version = '2.0.1', repos = 'http://cran.us.r-project.org')"
RUN R -q -e "remotes::install_version('processx', version = '3.8.3', repos = 'http://cran.us.r-project.org')"
RUN R -q -e "remotes::install_version('fontawesome', version = '0.5.2', repos = 'http://cran.us.r-project.org')"
RUN R -q -e "remotes::install_version('digest', version = '0.6.34', repos = 'http://cran.us.r-project.org')"



RUN R -q -e "remotes::install_version('farver', version = '2.1.1', repos = 'http://cran.us.r-project.org')"


# RUN R -q -e "install.packages(c('devtools'))"
RUN R -q -e "remotes::install_version('devtools', version = '2.4.5', repos = 'http://cran.us.r-project.org')"


# RUN R -q -e "remotes::install_github('r-lib/later')"
#RUN R -q -e "remotes::install_version('httpuv', version = '1.6.6', repos = 'http://cran.us.r-project.org')"
# RUN R -q -e "devtools::install_github('rstudio/httpuv')"

# Specific version of Shiny
# RUN R -q -e "remotes::install_version('shiny', version = '1.7.3', repos = 'http://cran.us.r-project.org')"

# basic shiny functionality
RUN R -q -e "install.packages(c('rmarkdown', 'markdown'))"

# additional shiny functionality
RUN R -q -e "install.packages(c('shinydashboard', 'shinydashboardPlus'))"
RUN R -q -e "install.packages(c('shinyWidgets', 'shinycssloaders'))"

# other R packages
RUN R -q -e "install.packages(c('DT', 'plotly', 'scico', 'ggthemes', 'scales', 'wesanderson'))"
RUN R -q -e "install.packages(c('data.table', 'dtplyr', 'googlesheets4'))"

# modified version of Rnumerai
# RUN R -q -e "devtools::install_github('woobe/Rnumerai')"
RUN R -q -e "remotes::install_github('woobe/Rnumerai')"


# copy the app to the image
WORKDIR /shinyapp
COPY --link Rprofile.site /usr/local/lib/R/etc/
COPY --link app /shinyapp/

EXPOSE 7860
CMD ["R", "-q", "-e", "shiny::runApp('/shinyapp')"]
