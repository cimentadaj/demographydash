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
  "Net Migration (Estimates and Projection)"
)

COMPARE_TAB_NAMES <- c(
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
  "Net Migration (Estimates and Projection)"
)

# Font size of text from a plot
DOWNLOAD_PLOT_SIZE <- list(title = 9, font = 10)

PLOTLY_TEXT_SIZE <- shiny::reactiveValues()

PLOTLY_LEGEND_OPTS <- list(
  x = 0.5,
  xanchor = "center",
  y = -0.2,
  yanchor = "top",
  orientation = "h"
)

update_plotly_legend_opts <- function(PLOTLY_LEGEND_OPTS) {
  if (PLOTLY_TEXT_SIZE$type == "tablet") {
    PLOTLY_LEGEND_OPTS$x <- 0.2
  }

  PLOTLY_LEGEND_OPTS
}

#' Apply legend options to a plotly object
#'
#' Uses layout() for standard properties, then directly sets xref and
#' title.side which the R plotly package strips from layout() calls.
#' @param p A plotly object
#' @param opts Legend options list (defaults to PLOTLY_LEGEND_OPTS)
#' @return The modified plotly object
apply_plotly_legend <- function(p, opts = PLOTLY_LEGEND_OPTS) {
  p <- p %>% plotly::layout(legend = opts)
  p$x$layout$legend$xref <- "paper"
  p$x$layout$legend$title$side <- "top"
  p
}

#' Fix plotly legend for comparison plots with ribbon traces
#'
#' After ggplotly conversion, ribbon traces get messy legend entries.
#' This function labels each ribbon trace with its simulation name
#' and the PI label, so users can toggle each simulation's CI
#' independently in the plotly legend.
#' @param p A plotly object
#' @param pi_label Label for the prediction interval ribbon
#' @param sim_names Character vector of simulation names (in order)
#' @return The modified plotly object
fix_plotly_ribbon_legend <- function(p, pi_label = "95% UN PI", sim_names = NULL) {
  ribbon_idx <- 0
  for (i in seq_along(p$x$data)) {
    trace <- p$x$data[[i]]
    is_ribbon <- !is.null(trace$fill) && trace$fill == "toself"
    if (is_ribbon) {
      ribbon_idx <- ribbon_idx + 1
      if (!is.null(sim_names) && ribbon_idx <= length(sim_names)) {
        label <- paste0(sim_names[ribbon_idx], ", ", pi_label)
      } else {
        label <- pi_label
      }
      p$x$data[[i]]$name <- label
      p$x$data[[i]]$showlegend <- TRUE
      p$x$data[[i]]$legendgroup <- label
    }
  }
  p
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
