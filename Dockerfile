FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y  libcurl4-openssl-dev libicu-dev libssl-dev make pandoc zlib1g-dev libgdal-dev libgeos-dev libproj-dev  && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/

RUN R -e 'install.packages("pak"); pak::pak("cimentadaj/demographydash")'

EXPOSE 8180

CMD ["Rscript", "-e", "options(shiny.port=8180, shiny.host='0.0.0.0'); library(demographydash); demographydash::run_app()"]
