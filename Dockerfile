FROM rocker/shiny:latest

RUN R -e 'install.packages("pak"); pak::pak("cimentadaj/demographydash")'

ARG PORT
RUN echo "SHN_PORT=${PORT}" >> /usr/local/lib/R/etc/Renviron.site

EXPOSE ${PORT}

CMD ["Rscript", "-e", "port <- as.numeric(Sys.getenv('SHN_PORT')); cat('Starting Shiny app on port:', port, '\n'); options(shiny.port=port, shiny.host='0.0.0.0'); demographydash::run_app()"]
