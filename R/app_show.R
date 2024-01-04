#' Generate the UI for the input page.
#'
#' @return A div containing the input page UI elements.
#' @importFrom untheme create_field_set
#' @export
#'
show_input_ui <- function() {
  div(
    create_field_set(
      "globe",
      "Select a country",
      "wpp_country",
      OPPPserver::get_wpp_countries(),
      NULL
    ),
    div(
      class = "two fields",
      create_field_set("calendar", "Starting Year", "wpp_starting_year", 2023:2099, 2023),
      create_field_set("calendar", "Ending Year", "wpp_ending_year", 2024:2100, 2100)
    )
  )
}


#' Generate the UI for the page containing the results of the forecast.
#'
#' @param input Internal parameter for `{shiny}`
#' @return A tabset UI component for the application
#' @importFrom shiny.semantic tabset multiple_radio
#' @importFrom shiny actionButton div
#' @importFrom untheme plotWithDownloadButtonsUI
#' @export
#'
show_forecast_results_ui <- function(input) {
  req(input$select_id) # Ensure input$select_id is not NULL

  # Empty list to store the UI elements
  ui_elements <- list()

  # Loop through each tab name and use switch to determine the UI element
  for (tab in TAB_NAMES) {
    ui_elements[[tab]] <- switch(
      tab,
      # Pages with some widget on the sidebar
      "Pop Pyramid" = plotWithDownloadButtonsUI(tab, uiOutput("pop_age_sex_years_ui"), width = "800px"),
      "Pop by Age" = plotWithDownloadButtonsUI(tab, multiple_radio("radio_population_by_broad_age_group", "Scale Type", choices = c("Percent", "Absolute"), type = "inline")),
      "Pop Over Time" = plotWithDownloadButtonsUI(tab, uiOutput("age_pop_time_ui")),
      "Deaths and Births" = plotWithDownloadButtonsUI(tab, multiple_radio("radio_death_births", "Type of plot", choices = c("Birth counts", "Birth rates", "Death counts", "Death rates"), type = "inline")),
      "YADR and OADR" = plotWithDownloadButtonsUI(tab, multiple_radio("radio_yadr_oadr", "Type of plot", choices = c("YADR", "OADR"), type = "inline")),
      # Pages with no widget on the sidebar
      plotWithDownloadButtonsUI(tab) # Default case
    )
  }

  # Select the appropriate UI element based on input
  selected_page <- ifelse(
    !is.null(ui_elements[[input$select_id]]),
    ui_elements[[input$select_id]],
    NULL
  )

  selected_page
}

#' Generate the UI for the population page.
#'
#' @return A div containing UI elements for step one
#' @importFrom shiny actionButton tableOutput
#' @importFrom shiny.semantic grid
#' @importFrom plotly plotlyOutput
#' @export
#'
show_pop_results_ui <- function() {
  div(
    class = "ui raised very padded container segment",
    style = "display: flex; align-items: flex-start; gap: 10px; width: 70%",
    div(
      style = "flex: 2;",
      plotlyOutput("plot_pop", height = "600px", width = "85%")
    ),
    div(
      style = "flex: 1;",
      shiny.semantic::semantic_DTOutput("table_pop", height = "400px")
    )
  )
}

#' Generate the UI for the TFR page.
#'
#' @return A div containing UI elements for step two
#' @importFrom shiny actionButton tableOutput
#' @importFrom shiny.semantic grid
#' @importFrom plotly plotlyOutput
#' @export
#'
show_tfr_results_ui <- function() {
  div(
    class = "ui raised very padded container segment",
    style = "display: flex; align-items: flex-start; gap: 10px; width: 75%",
    div(
      style = "flex: 3;",
      plotlyOutput("plot_tfr", height = "600px", width = "85%")
    )
  )
}

#' Show and compute the TFR page
#'
#' @param reactive_tfr A reactive function returning the TFR data frame
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param input,output Internal parameters for `{shiny}`.
#'
#' @importFrom shinyjs hide show
#' @importFrom shiny.semantic hide_modal
#' @export
#'
show_tfr <- function(reactive_tfr, wpp_ending_year, input, output) {
  hide_modal("modal_passtfr")
  hide("pop_page")
  show("tfr_page")
  compute_tfr(reactive_tfr, wpp_ending_year, input, output)
}

#' Compute the TFR page
#'
#' @param reactive_tfr A reactive function returning the TFR data frame
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param input,output Internal parameters for `{shiny}`.
#' @importFrom shiny renderUI
#' @export
#'
compute_tfr <- function(reactive_tfr, wpp_ending_year, input, output) {
  # Repeated the create_tfr_plot here because it allows the spinner
  # around the page to register the time spent
  create_tfr_plot(reactive_tfr(), end_year = wpp_ending_year(), country = input$wpp_country)
  output$show_tfr_results_ui <- renderUI(show_tfr_results_ui())
}
