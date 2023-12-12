
<!-- README.md is generated from README.Rmd. Please edit that file -->

# demographydash

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This package aims to create a shiny webapp to make demographic analysis
avialable to users which are not familiar with R. The web application
allows to upload data, validate it and conduct demographic analysis such
as life tables and projections.

## Installation

You can install the development version of demographydash from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cimentadaj/demographydash")
```

## Example

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
