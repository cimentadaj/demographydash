#' Generate the UI for the input page.
#'
#' @param i18n The internationalization object for translations.
#' @return A div containing the input page UI elements.
#' @importFrom untheme create_field_set
#' @importFrom shiny.semantic toggle
#' @importFrom shiny tags textInput
#' @export
#'
show_input_ui <- function(i18n) {
  div(
    uiOutput("toggle_region_ui"),
    br(),
    uiOutput("location_selector"),
    br(),
    div(
      class = "two fields",
      create_field_set("calendar", i18n$translate("Starting Year"), "wpp_starting_year", 1965:2099, 2024),
      create_field_set("calendar", i18n$translate("Ending Year"), "wpp_ending_year", 2024:2100, 2100)
    )
  )
}

#' Generate the location selector UI based on the toggle state
#'
#' @param input The input object from the Shiny server function
#' @param i18n The internationalization object for translations.
#' @return A UI element for selecting a country or region
#' @export
#'
location_selector_ui <- function(input, i18n) {
  if (length(input$toggle_region != 0) && input$toggle_region == i18n$translate("Region")) {
    create_field_set(
      "globe",
      i18n$translate("Select a region"),
      "wpp_country",
      OPPPserver::get_wpp_regions(),
      NULL
    )
  } else {
    create_field_set(
      "globe",
      i18n$translate("Select a country"),
      "wpp_country",
      OPPPserver::get_wpp_countries(),
      NULL
    )
  }
}

#' Generate the UI for the page containing the results of the forecast.
#'
#' @param input Internal parameter for `\{shiny\}`
#' @param i18n The internationalization object for translations.
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

  # Get current translations
  tabs_trans <- i18n$translate(TAB_NAMES)
  
  # Find which tab is currently selected
  current_index <- NULL
  for (i in seq_along(TAB_NAMES)) {
    if (input$select_id == i18n$translate(TAB_NAMES[i])) {
      current_index <- i
      break
    }
  }
  
  # If we couldn't find a match, default to first tab
  if (is.null(current_index)) {
    current_index <- 1
  }

  # Loop through each tab name and use switch to determine the UI element
  for (i in seq_along(tabs_trans)) {
    tab <- tabs_trans[i]
    english_name <- TAB_NAMES[i]  # Get English name directly by index

    # Pages with some widget on the sidebar
    if (i == 1) {  # Population Pyramid
      ids <- untheme::generate_ids(english_name)
      ui_elements[[tab]] <- untheme::sidebar_layout_responsive(
        sidebar = shiny::div(
          uiOutput("pop_age_sex_years_ui"),
          br(),
          downloadButton("all_pop_data", label = i18n$translate("Download All Population Data")),
          shiny::tags$div(style = "margin-bottom: 1px;"),
          shiny::div(
            style = "display: block;",
            shiny::downloadButton(ids$download_plot_id, label = i18n$translate("Download Plot")),
            shiny::br(),
            shiny::downloadButton(ids$download_data_id, label = i18n$translate("Download Data"))
          )
        ),
        main_panel = shiny::div(
          shinycssloaders::withSpinner(plotly::plotlyOutput(ids$plot_id, height = "600px", width = "auto"))
        )
      )
    } else if (i == 2) {  # Population by Broad Age Groups
      ui_elements[[tab]] <- plotWithDownloadButtonsUI(english_name, multiple_radio("radio_population_by_broad_age_group", i18n$translate("Scale Type"), choices = c(i18n$translate("Percent"), i18n$translate("Absolute")), type = "inline"), i18n = i18n)
    } else if (i == 3) {  # Population Over Time
      ids <- untheme::generate_ids(english_name)
      ui_elements[[tab]] <- untheme::sidebar_layout_responsive(
        sidebar = shiny::div(
          uiOutput("age_pop_time_ui"),
          shiny::tags$div(style = "margin-bottom: 1px;"),
          shiny::div(
            style = "display: block;",
            shiny::downloadButton(ids$download_plot_id, label = i18n$translate("Download Plot")),
            shiny::br(),
            shiny::downloadButton(ids$download_data_id, label = i18n$translate("Download Data"))
          )
        ),
        main_panel = shiny::div(
          shinycssloaders::withSpinner(plotly::plotlyOutput(ids$plot_id, height = "600px", width = "auto"))
        )
      )
    } else if (i == 6) {  # Deaths and Births
      ui_elements[[tab]] <- plotWithDownloadButtonsUI(english_name, multiple_radio("radio_death_births", i18n$translate("Type of plot"), choices = c(i18n$translate("Birth Counts"), i18n$translate("Birth Rates"), i18n$translate("Death Counts"), i18n$translate("Death Rates")), type = "inline"), i18n = i18n)
    } else if (i == 7) {  # YADR and OADR
      ui_elements[[tab]] <- plotWithDownloadButtonsUI(english_name, multiple_radio("radio_yadr_oadr", i18n$translate("Type of plot"), choices = c("YADR", "OADR"), type = "inline"), i18n = i18n)
    } else if (i == 11) {  # Life Expectancy Over Time
      ids <- untheme::generate_ids(english_name)
      ui_elements[[tab]] <- untheme::sidebar_layout_responsive(
        sidebar = shiny::div(
          uiOutput("sex_e0_time_ui"),
          shiny::tags$div(style = "margin-bottom: 1px;"),
          shiny::div(
            style = "display: block;",
            shiny::downloadButton(ids$download_plot_id, label = i18n$translate("Download Plot")),
            shiny::br(),
            shiny::downloadButton(ids$download_data_id, label = i18n$translate("Download Data"))
          )
        ),
        main_panel = shiny::div(
          shinycssloaders::withSpinner(plotly::plotlyOutput(ids$plot_id, height = "600px", width = "auto"))
        )
      )
    } else {
      ui_elements[[tab]] <- plotWithDownloadButtonsUI(english_name, i18n = i18n) # Default case
    }
  }

  # Select the appropriate UI element based on input
  selected_tab <- tabs_trans[current_index]
  selected_page <- ui_elements[[selected_tab]]
  selected_page
}

#' Generate the UI for the population page.
#'
#' @param data_source Character string indicating the data source ("UN Data" or "Custom Data")
#' @param i18n The internationalization object for translations
#' @return A div containing UI elements for step one
#' @importFrom shiny actionButton tableOutput div tags
#' @importFrom shiny.semantic grid
#' @importFrom shiny.fluent TooltipHost
#' @importFrom DT DTOutput
#' @importFrom plotly plotlyOutput
#' @export
#'
show_pop_results_ui <- function(data_source = "UN Data", i18n = NULL) {
  # Get translated text
  label_text <- if (!is.null(i18n)) i18n$t(data_source) else data_source
  
  # Get tooltip text based on data source
  tooltip_text <- if (!is.null(i18n)) {
    if (data_source == "UN Data") {
      i18n$t("Starting population data from UN sources. Use 'Customize' to paste custom data or edit UN data.")
    } else {
      i18n$t("Starting population data from custom source. Use 'Customize' to modify or switch back to UN data.")
    }
  } else {
    if (data_source == "UN Data") {
      "Starting population data from UN sources. Use 'Customize' to paste custom data or edit UN data."
    } else {
      "Starting population data from custom source. Use 'Customize' to modify or switch back to UN data."
    }
  }
  
  div(
    class = "ui raised very padded container segment responsive-container",
    style = "display: flex; flex-direction: column; gap: 10px",
    # Data source indicator with tooltip
    div(
      style = "display: flex; justify-content: center; margin-bottom: 10px;",
      TooltipHost(
        content = tooltip_text,
        tags$span(
          style = paste0(
            "display: inline-block; ",
            "padding: 6px 14px; ",
            "border: none; ",
            "border-radius: 4px; ",
            "font-size: 0.9em; ",
            "color: #ffffff; ",
            "background-color: #2185d0; ",
            "cursor: help; ",
            "transition: all 0.2s ease; ",
            "font-weight: 500;"
          ),
          onmouseover = "this.style.backgroundColor='#1678c2';",
          onmouseout = "this.style.backgroundColor='#2185d0';",
          label_text
        )
      )
    ),
    # Main content
    div(
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
#' @param input,output Internal parameters for `\{shiny\}`.
#' @param i18n The internationalization object for translations.
#'
#' @importFrom shinyjs hide show
#' @importFrom shiny.semantic hide_modal
#' @export
#'
show_tfr <- function(reactive_tfr, wpp_ending_year, input, output, i18n) {
  hide_modal("modal_passtfr")
  hide("pop_page")
  show("tfr_page")
  compute_tfr(reactive_tfr, wpp_ending_year, input, output, i18n)
}

#' Compute the TFR page
#'
#' @param reactive_tfr A reactive function returning the TFR data frame
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param input,output Internal parameters for `\{shiny\}`.
#' @param i18n The internationalization object for translations.
#' @importFrom shiny renderUI
#' @export
#'
compute_tfr <- function(reactive_tfr, wpp_ending_year, input, output, i18n) {
  # Repeated the create_tfr_plot here because it allows the spinner
  # around the page to register the time spent
  create_tfr_plot(reactive_tfr(), end_year = wpp_ending_year(), country = input$wpp_country, i18n)
  output$show_tfr_results_ui <- renderUI(show_tfr_results_ui())
}

#' Show and compute the e0 page
#'
#' @param reactive_e0 A reactive function returning the e0 data frame
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param input,output Internal parameters for `\{shiny\}`.
#' @param i18n The internationalization object for translations.
#'
#' @importFrom shinyjs hide show
#' @importFrom shiny.semantic hide_modal
#' @export
#'
show_e0 <- function(reactive_e0, wpp_ending_year, input, output, i18n) {
  hide("tfr_page")
  show("e0_page")
  compute_e0(reactive_e0, wpp_ending_year, input, output, i18n)
}

show_mig <- function(reactive_mig, wpp_ending_year, input, output, i18n) {
  hide("e0_page")
  show("mig_page")
  compute_mig(reactive_mig, wpp_ending_year, input, output, i18n)
}


#' Compute the e0 page
#'
#' @param reactive_e0 A reactive function returning the e0 data frame
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param input,output Internal parameters for `\{shiny\}`.
#' @param i18n The internationalization object for translations.
#' @importFrom shiny renderUI
#' @export
#'
compute_e0 <- function(reactive_e0, wpp_ending_year, input, output, i18n) {
  # Repeated the create_tfr_plot here because it allows the spinner
  # around the page to register the time spent
  create_e0_plot(
    reactive_e0(),
    end_year = wpp_ending_year(),
    country = input$wpp_country,
    i18n
  )

  output$show_e0_results_ui <- renderUI(show_e0_results_ui())
}


compute_mig <- function(reactive_mig, wpp_ending_year, input, output, i18n) {
  create_mig_plot(
    reactive_mig(),
    end_year = wpp_ending_year(),
    country = input$wpp_country,
    i18n
  )
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
