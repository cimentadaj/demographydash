#' Generate the UI for the input page.
#'
#' @return A div containing the input page UI elements.
#' @importFrom untheme create_field_set
#' @importFrom shiny.semantic toggle
#' @export
#'
show_input_ui <- function(i18n) {
  div(
    multiple_radio(
      input_id = "toggle_region",
      label = i18n$t("Which geographic aggregation do you want to work with?"),
      choices = c(
        "Country",
        "Region"
      ),
      selected = "Country",
      position = "grouped",
      type = "checkbox"
    ),
    br(),
    uiOutput("location_selector"),
    br(),
    div(
      class = "two fields",
      create_field_set("calendar", i18n$t("Starting Year"), "wpp_starting_year", 1965:2099, 2024),
      create_field_set("calendar", i18n$t("Ending Year"), "wpp_ending_year", 2024:2100, 2100)
    )
  )
}

#' Generate the location selector UI based on the toggle state
#'
#' @param input The input object from the Shiny server function
#' @return A UI element for selecting a country or region
#' @export
#'
location_selector_ui <- function(input, i18n) {
  print("location_selector_ui")
  print(input$toggle_region)
  if (input$toggle_region == "Region") {
    create_field_set(
      "globe",
      i18n$t("Select a region"),
      "wpp_country",
      OPPPserver::get_wpp_regions(),
      NULL
    )
  } else {
    create_field_set(
      "globe",
      i18n$t("Select a country"),
      "wpp_country",
      OPPPserver::get_wpp_countries(),
      NULL
    )
  }
}

#' Generate the UI for the page containing the results of the forecast.
#'
#' @param input Internal parameter for `{shiny}`
#' @return A tabset UI component for the application
#' @importFrom shiny.semantic tabset multiple_radio
#' @importFrom shiny downloadButton
#' @importFrom untheme plotWithDownloadButtonsUI
#' @export
#'
show_forecast_results_ui <- function(input, i18n) {
  req(input$select_id) # Ensure input$select_id is not NULL

  # Empty list to store the UI elements
  ui_elements <- list()

  # Loop through each tab name and use switch to determine the UI element
  for (tab in TAB_NAMES) {
    ui_elements[[tab]] <- switch(
      tab,
      # Pages with some widget on the sidebar
      "Population Pyramid By Age and Sex" = plotWithDownloadButtonsUI(tab, list(uiOutput("pop_age_sex_years_ui"), br(), downloadButton("all_pop_data", label = i18n$translate("Download All Population Data"))), i18n = i18n),
      "Population by Broad Age Groups" = plotWithDownloadButtonsUI(tab, multiple_radio("radio_population_by_broad_age_group", i18n$translate("Scale Type"), choices = c(i18n$translate("Percent"), i18n$translate("Absolute")), type = "inline"), i18n = i18n),
      "Population Over Time" = plotWithDownloadButtonsUI(tab, uiOutput("age_pop_time_ui"), i18n = i18n),
      "Deaths and Births" = plotWithDownloadButtonsUI(tab, multiple_radio("radio_death_births", i18n$translate("Type of plot"), choices = c(i18n$translate("Birth Counts"), i18n$translate("Birth Rates"), i18n$translate("Death Counts"), i18n$translate("Death Rates")), type = "inline"), i18n = i18n),
      "YADR and OADR" = plotWithDownloadButtonsUI(tab, multiple_radio("radio_yadr_oadr", i18n$translate("Type of plot"), choices = c("YADR", "OADR"), type = "inline"), i18n = i18n),
      "Life Expectancy Over Time" = plotWithDownloadButtonsUI(tab, uiOutput("sex_e0_time_ui"), i18n = i18n),
      # Pages with no widget on the sidebar
      plotWithDownloadButtonsUI(tab, i18n = i18n) # Default case
    )
  }

  # Select the appropriate UI element based on input
  selected_page <- ui_elements[[input$select_id]]
  selected_page
}

#' Generate the UI for the population page.
#'
#' @return A div containing UI elements for step one
#' @importFrom shiny actionButton tableOutput
#' @importFrom shiny.semantic grid
#' @importFrom DT DTOutput
#' @importFrom plotly plotlyOutput
#' @export
#'
show_pop_results_ui <- function() {
  div(
    class = "ui raised very padded container segment responsive-container",
    style = "display: flex; align-items: flex-start; gap: 10px",
    div(
      style = "flex: 2;",
      plotlyOutput("plot_pop", height = "600px", width = "100%")
    ),
    div(
      style = "flex: 1;",
      DTOutput("table_pop")
    )
  )
}

#' Generate the UI for the TFR page.
#'
#' @return A div containing UI elements for TFR page
#' @importFrom shiny actionButton tableOutput
#' @importFrom shiny.semantic grid
#' @importFrom plotly plotlyOutput
#' @export
#'
show_tfr_results_ui <- function() {
  div(
    class = "ui raised very padded container segment",
    style = "display: flex; align-items: flex-start; gap: 10px;",
    div(
      style = "flex: 3;",
      plotlyOutput("plot_tfr_custom", height = "600px", width = "100%")
    )
  )
}

#' Generate the UI for the e0 page.
#'
#' @return A div containing UI elements for e0 page
#' @importFrom shiny actionButton tableOutput
#' @importFrom shiny.semantic grid
#' @importFrom plotly plotlyOutput
#' @export
#'
show_e0_results_ui <- function() {
  div(
    class = "ui raised very padded container segment",
    style = "display: flex; align-items: flex-start; gap: 10px;",
    div(
      style = "flex: 3;",
      plotlyOutput("plot_e0_custom", height = "600px", width = "100%")
    )
  )
}


show_mig_results_ui <- function() {
  div(
    class = "ui raised very padded container segment",
    style = "display: flex; align-items: flex-start; gap: 10px;",
    div(
      style = "flex: 3;",
      plotlyOutput("plot_mig_custom", height = "600px", width = "100%")
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

#' Show and compute the e0 page
#'
#' @param reactive_e0 A reactive function returning the e0 data frame
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param input,output Internal parameters for `{shiny}`.
#'
#' @importFrom shinyjs hide show
#' @importFrom shiny.semantic hide_modal
#' @export
#'
show_e0 <- function(reactive_e0, wpp_ending_year, input, output) {
  hide("tfr_page")
  show("e0_page")
  compute_e0(reactive_e0, wpp_ending_year, input, output)
}

show_mig <- function(reactive_mig, wpp_ending_year, input, output) {
  hide("e0_page")
  show("mig_page")
  compute_mig(reactive_mig, wpp_ending_year, input, output)
}


#' Compute the e0 page
#'
#' @param reactive_e0 A reactive function returning the e0 data frame
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param input,output Internal parameters for `{shiny}`.
#' @importFrom shiny renderUI
#' @export
#'
compute_e0 <- function(reactive_e0, wpp_ending_year, input, output) {
  # Repeated the create_tfr_plot here because it allows the spinner
  # around the page to register the time spent
  create_e0_plot(reactive_e0(), end_year = wpp_ending_year(), country = input$wpp_country)
  output$show_e0_results_ui <- renderUI(show_e0_results_ui())
}


compute_mig <- function(reactive_mig, wpp_ending_year, input, output) {
  create_mig_plot(reactive_mig(), end_year = wpp_ending_year(), country = input$wpp_country)
  output$show_mig_results_ui <- renderUI(show_mig_results_ui())
}


adjust_title_and_font <- function(device, title) {
  # Define maximum characters for each device type and corresponding font sizes
  max_chars <- list(mobile = c(45, 9, 8), tablet = c(45, 11, 10), screen = c(80, 13, 11))
  device_settings <- max_chars[[device]]
  if (is.null(device_settings)) {
    device_settings <- c(25, 18, 14) # Default values
  }

  max_char_count <- device_settings[1]
  large_font <- device_settings[2]
  small_font <- device_settings[3]

  # Split and join the title if it exceeds the maximum character count
  if (nchar(title) > max_char_count) {
    words <- strsplit(title, " ")[[1]]
    split_title <- ""
    current_line <- ""
    for (word in words) {
      if (nchar(paste0(current_line, word)) + 1 <= max_char_count) {
        current_line <- paste0(current_line, word, " ")
      } else {
        split_title <- paste0(split_title, current_line, "\n")
        current_line <- paste0(word, " ")
      }
    }
    modified_title <- paste0(split_title, current_line)
    font_size <- small_font
  } else {
    modified_title <- title
    font_size <- large_font
  }

  return(list(title = modified_title, font_size = font_size))
}
