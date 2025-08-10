# File validation rules for each data type
validation_rules <- list(
  pop = list(
    expected_cols = 3,
    col_types = c("numeric", "numeric", "numeric"),
    col_names = c("age", "popF", "popM")
  ),
  tfr = list(
    expected_cols = 2,
    col_types = c("numeric", "numeric"),
    col_names = c("year", "tfr")
  ),
  e0 = list(
    expected_cols = 3,
    col_types = c("numeric", "numeric", "numeric"),
    col_names = c("year", "e0M", "e0F")
  ),
  mig = list(
    expected_cols = 2,
    col_types = c("numeric", "numeric"),
    col_names = c("year", "mig")
  )
)

data_source <- shiny::reactiveValues(
  tfr = "downloaded", # "downloaded" or "uploaded"
  e0 = "downloaded",
  mig = "downloaded"
)


# Generic validation functions
check_column_count <- function(data, expected_cols) {
  ncol(data) == expected_cols
}

check_column_types <- function(data, expected_types) {
  actual_types <- sapply(data, class)
  all(mapply(function(actual, expected) {
    if (expected == "numeric") {
      return(is.numeric(data[[actual]]))
    }
    if (expected == "character") {
      return(is.character(data[[actual]]))
    }
    return(FALSE)
  }, names(data), expected_types))
}

# Main file parser function
file_parser <- function(file, data_type, i18n = NULL) {
  if (!data_type %in% names(validation_rules)) {
    return(list(
      success = FALSE,
      message = if (!is.null(i18n)) i18n$t("Unknown data type") else "Unknown data type"
    ))
  }

  rules <- validation_rules[[data_type]]

  tryCatch(
    {
      # Try reading the CSV
      data <- readr::read_csv(file$datapath)

      # Check number of columns
      if (!check_column_count(data, rules$expected_cols)) {
        return(list(
          success = FALSE,
          message = sprintf(
            if (!is.null(i18n)) i18n$t("Expected %d columns but found %d") else "Expected %d columns but found %d",
            rules$expected_cols, ncol(data)
          )
        ))
      }

      # Check column types
      if (!check_column_types(data, rules$col_types)) {
        return(list(
          success = FALSE,
          message = if (!is.null(i18n)) i18n$t("One or more columns have incorrect data types") else "One or more columns have incorrect data types"
        ))
      }

      # If all checks pass
      return(list(
        success = TRUE,
        message = if (!is.null(i18n)) i18n$t("Validation successful") else "Validation successful",
        data = data
      ))
    },
    error = function(e) {
      return(list(
        success = FALSE,
        message = paste(if (!is.null(i18n)) i18n$t("Error reading file:") else "Error reading file:", e$message)
      ))
    }
  )
}

#' The Application Server-Side Logic
#'
#' This function defines the server logic for the Shiny application,
#' managing data processing, UI rendering, and routing.
#'
#' @param input,output,session Internal parameters for `\{shiny\}`.
#'
#' @importFrom shiny reactive reactiveVal observeEvent renderUI observe downloadHandler
#' @importFrom shinyjs hide show
#' @importFrom utils read.csv
#' @importFrom untheme detect_font_size
#' @importFrom shiny.i18n update_lang
#' @importFrom shiny.semantic multiple_radio
#' @importFrom OPPPserver get_wpp_pop get_wpp_tfr get_wpp_e0 get_wpp_mig
#' @importFrom rintrojs introjs
#' @export
#'
app_server <- function(input, output, session) {

  i18n <- usei18n_local()

  # Render the toggle_region UI with translated options
  output$toggle_region_ui <- renderUI({
    multiple_radio(
      input_id = "toggle_region",
      label = i18n$translate("Which geographic aggregation do you want to work with?"),
      choices = i18n$translate(c("Country", "Region")),
      selected = i18n$translate("Country"),
      position = "grouped",
      type = "radio"
    )
  })

  # At the top with other reactives
  selected_tab_index <- reactiveVal(1)  # Start with first tab

  # Define a reactiveVal to store simulation results
  simulation_results <- reactiveVal()

  # Add a reactive expression to track the current tab
  current_tab_name <- reactive({
    # Return the current tab name in the current language
    i18n$translate(TAB_NAMES[selected_tab_index()])
  })

  # Language change observer
  observeEvent(c(input$selected_language), {
    if (!is.null(input$selected_language)) {
      # Before changing language, store which tab we're on
      if (!is.null(input$select_id)) {
        # Find which tab is currently selected (in old language)
        old_lang <- i18n$get_translation_language()
        for (i in seq_along(TAB_NAMES)) {
          if (input$select_id == i18n$translate(TAB_NAMES[i])) {
            selected_tab_index(i)
            break
          }
        }
      }
      
      # Update language
      update_lang(input$selected_language)
      i18n$set_translation_language(input$selected_language)
      
      # Force UI update with new language
      output$select_plot_tab <- renderUI({
        selectInput(
          "select_id",
          i18n$translate("Results"),
          choices = i18n$translate(TAB_NAMES),
          selected = current_tab_name()
        )
      })
    }
  })

  # Initial UI render
  output$select_plot_tab <- renderUI({
    selectInput(
      "select_id",
      i18n$translate("Results"),
      choices = i18n$translate(TAB_NAMES),
      selected = current_tab_name()
    )
  })

  current_tab <- reactiveVal()

  observe({
    req(input$get_screen_width)
    sizes <- detect_font_size(input$get_screen_width)
    PLOTLY_TEXT_SIZE$title <- sizes$title
    PLOTLY_TEXT_SIZE$font <- sizes$font
    PLOTLY_TEXT_SIZE$type <- sizes$type
  })

  # Make the www folder available for loading images and icons
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  # Some functions need the years in number. Coerce them from
  # the beginning and  use from these reactive expressions
  wpp_starting_year <- reactive(as.numeric(input$wpp_starting_year))
  wpp_ending_year <- reactive(as.numeric(input$wpp_ending_year))

  observeEvent(input$pop_help, {
    if (current_tab() == "pop_page") {
      rintrojs::introjs(session, options = list(
        steps = data.frame(
          element = c("#customize_pop", "#show_pop_results_ui", "#forward_tfr_page"),
          intro = c(
            i18n$translate("Click here to upload your own data for the starting year of the chosen country"),
            i18n$translate("This is the current population values for the starting year of the chosen country"),
            i18n$translate("When ready, click here to go to the next steps. If you want to upload your own data for other indicators, the interface will be similar to this one.")
          )
        )
      ))
    }
  })

  observeEvent(input$customize_help, {
    if (grepl("modal_", current_tab())) {
      # Extract the base name (everything after "modal_")
      base_name <- sub("modal_", "", current_tab())

      # Dynamically construct widget IDs
      dt_output_id <- paste0("tmp_", base_name, "_dt")
      download_id <- paste0("download_", base_name)
      # The file input widget for some reason is no recognized
      # for that reason we simply pick the parent class from the footer
      file_input_id <- "footer-container"

      steps_df <- data.frame(
        element = c(
          paste0("#", dt_output_id),
          paste0("#", download_id),
          paste0("#", file_input_id)
        ),
        intro = c(
          i18n$translate("This is the starting year data for this indicator. If you paste new data, it should have exactly this format: same number of columns, same order of columns and importantly, the same metric. Some of these indicators are expressed in thousands, for example."),
          i18n$translate("To use your own data, download the current data to see the correct format. You can then copy your data from Excel or another source and paste it directly into the table."),
          i18n$translate("Edit the table directly or paste your data from Excel. The data should be formatted exactly as shown above. Click 'Apply' when done to save your changes.")
        )
      )

      rintrojs::introjs(session, options = list(
        steps = steps_df
      ))
    }
  })

  observeEvent(input$forecast_help, {
    if (current_tab() == "forecast_page") {
      rintrojs::introjs(session, options = list(
        steps = data.frame(
          element = c("#select_id", "#show_forecast_results_ui"),
          intro = c(
            i18n$translate("Use this dropdown menu to explore different aspects of your population projection. You can view various demographic indicators like population pyramids, age structures, dependency ratios, and other key metrics that help understand the projected demographic changes."),
            i18n$translate("Here you'll find the projection plots and ways to interact with it. On the left sidebar you can filter your projections and download a variety of results, either individual results of a combined package of the results.")
          )
        )
      ))
    }
  })

  # TODO: remove this and import directly run_forecast. Need to fix
  # dependency issue
  library(OPPPserver)

  # ReactiveVals to store data committed from modals
  committed_pop_rv <- reactiveVal(NULL)
  committed_tfr_rv <- reactiveVal(NULL)
  committed_e0_rv  <- reactiveVal(NULL)
  committed_mig_rv <- reactiveVal(NULL)
  
  # Track data source for population (UN Data or Custom Data)
  pop_data_source <- reactiveVal("UN Data")

  # Observe changes to wpp_countries and reset committed data
  observeEvent(list(input$wpp_country, input$wpp_starting_year, input$wpp_ending_year), {
    committed_pop_rv(NULL)
    committed_tfr_rv(NULL)
    committed_e0_rv(NULL)
    committed_mig_rv(NULL)
    pop_data_source("UN Data")  # Reset to UN Data when country/year changes
    # Reset data_source if still relevant to distinguish default from committed
    data_source$tfr <- "downloaded" # or "default"
    data_source$e0  <- "downloaded" # or "default"
    data_source$mig <- "downloaded" # or "default"
  })

  reactive_pop <- reactive({
    user_data <- committed_pop_rv()
    if (!is.null(user_data)) {
      print("[PLOT-DATA] Preparing population data for plotting after Apply:")
      print(paste("[PLOT-DATA] Input columns:", paste(names(user_data), collapse=", ")))
      print(paste("[PLOT-DATA] First few rows of input data:"))
      print(head(user_data))
      
      # Ensure the data has the expected column names
      # Our standard is age, popM, popF but create_pop_pyramid_plot expects age, popF, popM
      # So we need to reorder for compatibility
      if (all(c("age", "popM", "popF") %in% names(user_data))) {
        # Reorder to what create_pop_pyramid_plot expects
        user_data <- user_data[, c("age", "popF", "popM")]
      } else {
        # Fallback naming if columns are in different order
        names(user_data) <- c("age", "popF", "popM")
      }
      
      print(paste("[PLOT-DATA] Output columns after reordering:", paste(names(user_data), collapse=", ")))
      print(paste("[PLOT-DATA] Total rows:", nrow(user_data)))
      
      # Convert to data.table as expected by create_pop_pyramid_plot
      return(data.table::as.data.table(user_data))
    } else {
      # Fallback to default WPP data
      wpp_data <- get_wpp_pop(input$wpp_country, wpp_starting_year())
      print("[PLOT-DATA] Using default WPP data (no Apply clicked)")
      print(paste("[PLOT-DATA] WPP data columns:", paste(names(wpp_data), collapse=", ")))
      # Convert to data.table
      return(data.table::as.data.table(wpp_data))
    }
  })

  reactive_tfr <- reactive({
    user_data <- committed_tfr_rv()
    if (!is.null(user_data)) {
      names(user_data) <- c("year", "tfr")
      data_source$tfr <- "custom" # Indicate data is from modal commit
      return(user_data)
    } else {
      res <- get_wpp_tfr(input$wpp_country)
      data_source$tfr <- "downloaded" # Default WPP data
      return(res)
    }
  })

  reactive_e0 <- reactive({
    user_data <- committed_e0_rv()
    if (!is.null(user_data)) {
      names(user_data) <- c("year", "e0M", "e0F")
      data_source$e0 <- "custom" # Indicate data is from modal commit
      return(user_data)
    } else {
      res <- get_wpp_e0(input$wpp_country)
      data_source$e0 <- "downloaded" # Default WPP data
      return(res)
    }
  })

  reactive_mig <- reactive({
    user_data <- committed_mig_rv()
    if (!is.null(user_data)) {
      names(user_data) <- c("year", "mig")
      data_source$mig <- "custom" # Indicate data is from modal commit
      return(user_data)
    } else {
      res <- get_wpp_mig(input$wpp_country)
      data_source$mig <- "downloaded" # Default WPP data
      return(res)
    }
  })

  # This is a very weird thing. If I use this in the title of the TFR customize
  # tab, weird things start to happen. I think it's because of the circularity
  # that this reactive_tfr is updated from the same modal that uploads a new
  # reactive tfr. The weirdness comes when uploading a new file. I still
  # use this for generating the file name but not in the title of the modal.
  tfr_starting_year <- reactive(min(reactive_tfr()[[1]], na.rm = TRUE))

  # Handle any checks on inputs to make sure everything is correct
  handle_validity_checks(wpp_starting_year, wpp_ending_year, output, i18n)

  # Handle pop/tfr plots/tables before analysis
  handle_before_analysis_plots(
    reactive_pop,
    reactive_tfr,
    reactive_e0,
    reactive_mig,
    wpp_starting_year,
    wpp_ending_year,
    input,
    output,
    i18n
  )

  # Handle navigation between steps
  handle_navigation(
    simulation_results = simulation_results,
    reactive_pop = reactive_pop,
    reactive_tfr = reactive_tfr,
    reactive_e0 = reactive_e0,
    reactive_mig = reactive_mig,
    pop_data_source = pop_data_source,
    wpp_starting_year = wpp_starting_year,
    wpp_ending_year = wpp_ending_year,
    current_tab = current_tab,
    input = input,
    output = output,
    i18n = i18n
  )

  # Handle all customize actions
  handle_customize_data(
    current_pop_reactive = reactive_pop,
    current_tfr_reactive = reactive_tfr,
    current_e0_reactive = reactive_e0,
    current_mig_reactive = reactive_mig,
    pop_to_commit_rv = committed_pop_rv,
    tfr_to_commit_rv = committed_tfr_rv,
    e0_to_commit_rv = committed_e0_rv,
    mig_to_commit_rv = committed_mig_rv,
    pop_data_source = pop_data_source,
    tfr_starting_year,
    wpp_starting_year,
    wpp_ending_year,
    current_tab,
    input,
    output,
    session,
    i18n
  )


  # Everything that doesn't fit into other handles is here look tooltip server side code.
  handle_misc(wpp_starting_year, wpp_ending_year, input, output, i18n)

  # Begin simulation on button click
  observeEvent(input$begin, {
    hide("mig_page")
    show("forecast_page")
    current_tab("forecast_page") # Assuming current_tab is defined in server

    # Begin forecast. This can take a up to a minute of calculation
    begin_forecast(
      reactive_pop = reactive_pop,
      reactive_tfr = reactive_tfr,
      reactive_e0 = reactive_e0,
      reactive_mig = reactive_mig,
      wpp_starting_year = wpp_starting_year,
      wpp_ending_year = wpp_ending_year,
      input = input,
      output = output,
      simulation_results = simulation_results,
      i18n = i18n
    )

  })

  # Download handler for the report
  output$download_report <- downloadHandler(
    filename = function() {
      paste0(
        "demographic_projection_report_",
        gsub(" ", "_", input$wpp_country),
        "_",
        wpp_starting_year(),
        "-",
        wpp_ending_year(),
        ".pdf"
      )
    },
    content = function(file) {
      showNotification(i18n$t("Generating report... Please wait."), type = "message", duration = NULL, id = "report-gen-notification")

      req(simulation_results()) # Ensure simulation results are available
      results <- simulation_results()

      if (is.null(results)) {
        removeNotification("report-gen-notification")
        showNotification(i18n$t("Please run the projection first to generate a report."), type = "error", duration = 7)
        return(NULL) # Stop execution if results are not available
      }

      # Determine current selections for plot generation
      current_pop_year <- if (!is.null(input$pop_age_sex_years)) input$pop_age_sex_years else (wpp_starting_year() + 1)
      current_age_group <- if (!is.null(input$age_pop_time) && length(unique(results$population_by_time$age)) > 0) input$age_pop_time else unique(results$population_by_time$age)[1]

      # Generate all plots non-reactively
      plots <- tryCatch({
        create_all_report_plots(
          simulation_results = results,
          country = input$wpp_country,
          start_year = wpp_starting_year(),
          end_year = wpp_ending_year(),
          pop_year = current_pop_year,
          age_group = current_age_group,
          i18n = i18n
        )
      }, error = function(e) {
        removeNotification("report-gen-notification")
        showNotification(paste(i18n$t("Error generating plots for report:"), e$message), type = "error", duration = 10)
        return(NULL)
      })

      if (is.null(plots)) {
        removeNotification("report-gen-notification") # Ensure notification is removed if plot generation fails
        return(NULL)
      }

      # Generate the PDF report
      pdf_file_path <- tryCatch({
        generate_demography_report(
          plot_list = plots,
          country = input$wpp_country,
          start_year = wpp_starting_year(),
          end_year = wpp_ending_year(),
          i18n = i18n
        )
      }, error = function(e) {
        removeNotification("report-gen-notification")
        showNotification(paste(i18n$t("Error creating PDF report:"), e$message), type = "error", duration = 10)
        return(NULL)
      })

      removeNotification("report-gen-notification")
      if (!is.null(pdf_file_path) && file.exists(pdf_file_path)) {
        file.copy(pdf_file_path, file, overwrite = TRUE)
        showNotification(i18n$t("Report downloaded successfully!"), type = "message", duration = 5)
        # utils::browseURL(pdf_file_path) # No longer needed, browser handles download
      } else {
        # If pdf_file_path is NULL (error during generation) or file doesn't exist
        showNotification(i18n$t("Report generation failed. PDF could not be created or found."), type = "error", duration = 7)
      }
    }
  )

}

#' Create a translator object for internationalization
#'
#' @return A Translator object initialized with the package's translation file
#' @importFrom shiny.i18n Translator
#' @export
usei18n_local <- function() {
  translation_path <- system.file("extdata", "translation.json", package = "demographydash")
  if (translation_path == "") {
    stop("Could not find translation.json in package. Please ensure the package is installed correctly.")
  }
  i18n <- Translator$new(translation_json_path = translation_path)
  i18n$set_translation_language("en")
  i18n
}