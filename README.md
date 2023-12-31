
<!-- README.md is generated from README.Rmd. Please edit that file -->

# demographydash

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This package aims to create a shiny web app to make demographic analysis
available to users which are not familiar with R. The web application
allows to download WPP data and make demographic projections.

## Installation

You can install the development version of demographydash from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cimentadaj/demographydash")
```

## How to launch the app

To launch the app locally, run:

``` r
library(demographydash)
run_app()
```

To launch the app through docker, save a `.env` file in the root of the
repository containing the environment variable `GITHUB_PAT` and run this
in the terminal at the root of the repository:

``` bash
docker build -t demoapp .
docker run --env-file .env -p 3838:3838 -d demoapp
```

And test the app in <http://localhost:3838>.,

## Brief explanation

The app is composed of several modules. Here’s a one line for each
module so it’s easier to understand:

    R/
    ├── app_displays.R # All plots / tables in the app have a function that produces it here
    ├── app_forecast.R # Functions related to running the forecast
    ├── app_handles.R # handle_* functions control aspects of the app (navigation, customization, etc..)
    ├── app_show.R # show_* functions render different UI pages of the app
    ├── app_server.R # The server-side logic
    ├── app_ui.R # UI logic of the app
    ├── app_config.R
    ├── _disable_autoload.R
    ├── run_app.R
    └── utils-pipe.R
