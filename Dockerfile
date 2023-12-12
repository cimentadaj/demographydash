# TODO: A better would be that the install step directly takes it from DESCRIPTION on each run such that you don't have to update the version of each package manually.

FROM rocker/verse:4.2.2

RUN apt-get update && apt-get install -y  libcurl4-openssl-dev libicu-dev libssl-dev make pandoc zlib1g-dev libgdal-dev libgeos-dev libproj-dev  && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/

RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4, timeout = max(300, getOption('timeout')))" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site

RUN R -e 'install.packages("remotes")'

RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "1.1.2")'
RUN Rscript -e 'remotes::install_version("scales",upgrade="never", version = "1.3.0")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.3.3")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.8.0")'
RUN Rscript -e 'remotes::install_version("data.table",upgrade="never", version = "1.14.8")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.4.4")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.1.6")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("shiny.semantic",upgrade="never", version = "0.4.3")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.4.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.30")'
RUN Rscript -e 'remotes::install_github("daattali/shinyalert@452b946f3ba9144cf45fa2f85f5857a815609db1")'
RUN Rscript -e 'remotes::install_github("plotly/plotly.R@d76e87e573a5317100d3bcf1cea2b8cb6c1d3c93")'
RUN Rscript -e 'remotes::install_github("cimentadaj/untheme")'
RUN Rscript -e 'remotes::install_github("PPgp/OPPPserver")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone

RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 3838
CMD R -e "options('shiny.port'=3838, shiny.host='0.0.0.0');library(demographydash); demographydash::run_app()"
