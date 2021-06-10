FROM rocker/shiny-verse:4.0.3

Label maintainer="Chris Jones <cmjone25@ncsu.edu>"

## system libraries of general use
## install debian packages
RUN apt-get update -qq \
    && apt-get install -y --no-install-recommends \
      libxml2-dev \
      libcairo2-dev \
      libsqlite3-dev \
      libpq-dev \
      libssh2-1-dev \
      unixodbc-dev \
      libcurl4-openssl-dev \
      libssl-dev \
      pandoc \
      pandoc-citeproc \
      libssl-dev \
      libblas-dev \
      liblapack-dev \
      libudunits2-dev \
      libproj-dev \
      libgeos-dev \
      libgdal-dev \
      libglpk-dev \
      git \
      qpdf

# copy necessary files
## data and app folders
COPY /shiny_app ./app

## Instal R packages
RUN install2.r --error --skipinstalled \
    shiny \
    shinytest \
    reshape2 \
    dplyr \
    magrittr \
    ade4

RUN r -e "install.packages('adegenet', dependencies = c('Depends', 'Imports'))"
RUN ["installGithub.r", "grunwaldlab/poppr@261515d79674abb2f3ee5d07f0ecab4460e74067"]

## open port 3838 to traffic
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
