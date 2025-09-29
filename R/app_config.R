TAB_NAMES <- c(
  "Population Pyramid By Age and Sex",
  "Population by Broad Age Groups",
  "Population Over Time",
  "Projected Total Fertility Rate",
  "Population Growth Rate by Age",
  "Deaths and Births",
  "YADR and OADR",
  "Population Size and Aging",
  "CDR and Life Expectancy",
  "CBR and TFR",
  "Life Expectancy Over Time",
  "Projected Net Migration"
)

COMPARE_TAB_NAMES <- c(
  "Population by Broad Age Groups",
  "Population Over Time",
  "Projected Total Fertility Rate",
  "Population Growth Rate by Age",
  "Deaths and Births",
  "YADR and OADR",
  "Population Size and Aging",
  "CDR and Life Expectancy",
  "CBR and TFR",
  "Life Expectancy Over Time",
  "Projected Net Migration"
)

# Font size of text from a plot
DOWNLOAD_PLOT_SIZE <- list(title = 9, font = 10)

PLOTLY_TEXT_SIZE <- shiny::reactiveValues()

PLOTLY_LEGEND_OPTS <- list(
  xanchor = "bottom",
  y = -0.32,
  xref = "container",
  yanchor = "bottom",
  orientation = "h"
)

update_plotly_legend_opts <- function(PLOTLY_LEGEND_OPTS) {
  if (PLOTLY_TEXT_SIZE$type == "tablet") {
    PLOTLY_LEGEND_OPTS$x <- 0.2
  }

  PLOTLY_LEGEND_OPTS
}

JS_CODE_SCREEN_SIZE <- '$(document).on("shiny:connected", function(e) {
  var jsWidth = screen.width;
  Shiny.onInputChange("get_screen_width",jsWidth);
});
'

#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "demographydash")
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @noRd
get_golem_config <- function(
    value,
    config = Sys.getenv(
      "GOLEM_CONFIG_ACTIVE",
      Sys.getenv(
        "R_CONFIG_ACTIVE",
        "default"
      )
    ),
    use_parent = TRUE,
    # Modify this if your config file is somewhere else
    file = app_sys("golem-config.yml")) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}
