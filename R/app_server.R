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
file_parser <- function(file, data_type) {
  if (!data_type %in% names(validation_rules)) {
    return(list(
      success = FALSE,
      message = "Unknown data type"
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
            "Expected %d columns but found %d",
            rules$expected_cols, ncol(data)
          )
        ))
      }

      # Check column types
      if (!check_column_types(data, rules$col_types)) {
        return(list(
          success = FALSE,
          message = "One or more columns have incorrect data types"
        ))
      }

      # If all checks pass
      return(list(
        success = TRUE,
        message = "Validation successful",
        data = data
      ))
    },
    error = function(e) {
      return(list(
        success = FALSE,
        message = paste("Error reading file:", e$message)
      ))
    }
  )
}

#' The Application Server-Side Logic
#'
#' This function defines the server logic for the Shiny application,
#' managing data processing, UI rendering, and routing.
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#'
#' @importFrom shiny reactive reactiveVal observeEvent renderUI observe
#' @importFrom shinyjs hide show
#' @importFrom utils read.csv
#' @importFrom untheme detect_font_size
#' @importFrom OPPPserver get_wpp_pop get_wpp_tfr get_wpp_e0 get_wpp_mig
#' @export
#'
app_server <- function(input, output, session) {
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

  # TODO: remove this and import directly run_forecast. Need to fix
  # dependency issue
  library(OPPPserver)

  # Step 1: Create a reactiveVal to manage each file input state
  file_input_pop <- reactiveVal(NULL)
  file_input_tfr <- reactiveVal(NULL)
  file_input_e0 <- reactiveVal(NULL)
  file_input_mig <- reactiveVal(NULL)

  # Step 2: Observe changes to wpp_countries and reset each file input
  observeEvent(list(input$wpp_country, input$wpp_starting_year, input$wpp_ending_year), {
    file_input_pop(NULL)
    file_input_tfr(NULL)
    file_input_e0(NULL)
    file_input_mig(NULL)
  })

  # Modified observers using the new parser
  observeEvent(input$upload_pop, {
    result <- file_parser(input$upload_pop, "pop")
    if (result$success) {
      file_input_pop(input$upload_pop)
    } else {
      shinyalert(
        title = "Error",
        text = paste("Population file validation failed:", result$message),
        type = "error"
      )
    }
  })

  observeEvent(input$upload_tfr, {
    result <- file_parser(input$upload_tfr, "tfr")
    if (result$success) {
      file_input_tfr(input$upload_tfr)
    } else {
      shinyalert(
        title = "Error",
        text = paste("TFR file validation failed:", result$message),
        type = "error"
      )
    }
  })

  observeEvent(input$upload_e0, {
    result <- file_parser(input$upload_e0, "e0")
    if (result$success) {
      file_input_e0(input$upload_e0)
    } else {
      shinyalert(
        title = "Error",
        text = paste("Life expectancy file validation failed:", result$message),
        type = "error"
      )
    }
  })

  observeEvent(input$upload_mig, {
    result <- file_parser(input$upload_mig, "mig")
    if (result$success) {
      file_input_mig(input$upload_mig)
    } else {
      shinyalert(
        title = "Error",
        text = paste("Migration file validation failed:", result$message),
        type = "error"
      )
    }
  })

  # Step 4: Define the get_file_input function for each type of data
  get_file_input_pop <- reactive({
    file_input_pop()
  })
  get_file_input_tfr <- reactive({
    file_input_tfr()
  })
  get_file_input_e0 <- reactive({
    file_input_e0()
  })
  get_file_input_mig <- reactive({
    file_input_mig()
  })

  # Step 5: Define the reactive functions using the specific get_file_input function
  reactive_pop <- reactive({
    if (!is.null(get_file_input_pop())) {
      res <- data.table(readr::read_csv(get_file_input_pop()$datapath))
      names(res) <- c("age", "popF", "popM")
    } else {
      res <- get_wpp_pop(input$wpp_country, wpp_starting_year())
    }

    res
  })

  reactive_tfr <- reactive({
    if (!is.null(get_file_input_tfr())) {
      res <- data.table(readr::read_csv(get_file_input_tfr()$datapath))
      names(res) <- c("year", "tfr")
    } else {
      res <- get_wpp_tfr(input$wpp_country)
    }

    res
  })

  reactive_e0 <- reactive({
    if (!is.null(get_file_input_e0())) {
      res <- data.table(readr::read_csv(get_file_input_e0()$datapath))
      names(res) <- c("year", "e0M", "e0F")
    } else {
      res <- get_wpp_e0(input$wpp_country)
    }

    res
  })

  reactive_mig <- reactive({
    if (!is.null(get_file_input_mig())) {
      res <- data.table(readr::read_csv(get_file_input_mig()$datapath))
      names(res) <- c("year", "mig")
    } else {
      res <- get_wpp_mig(input$wpp_country)
    }

    res
  })

  # This is a very weird thing. If I use this in the title of the TFR customize
  # tab, weird things start to happen. I think it's because of the circularity
  # that this reactive_tfr is updated from the same modal that uploads a new
  # reactive tfr. The weirdness comes when uploading a new file. I still
  # use this for generating the file name but not in the title of the modal.
  tfr_starting_year <- reactive(min(reactive_tfr()[[1]], na.rm = TRUE))

  # Handle any checks on inputs to make sure everything is correct
  handle_validity_checks(wpp_starting_year, wpp_ending_year, output)

  # Handle pop/tfr plots/tables before analysis
  handle_before_analysis_plots(
    reactive_pop,
    reactive_tfr,
    reactive_e0,
    reactive_mig,
    wpp_starting_year,
    wpp_ending_year,
    input,
    output
  )

  # Handle navigation between steps
  handle_navigation(
    reactive_pop,
    reactive_tfr,
    reactive_e0,
    reactive_mig,
    wpp_starting_year,
    wpp_ending_year,
    input,
    output
  )

  # Handle all customize actions
  handle_customize_data(
    reactive_pop,
    reactive_tfr,
    reactive_e0,
    reactive_mig,
    tfr_starting_year,
    wpp_starting_year,
    wpp_ending_year,
    input,
    output
  )

  # Everything that doesn't fit into other handles is here look tooltip server side code.
  handle_misc(wpp_starting_year, wpp_ending_year, input, output)

  # Begin simulation on button click
  observeEvent(input$begin, {
    hide("mig_page")
    show("forecast_page")

    # Define a reactiveVal to store simulation results
    simulation_results <- reactiveVal()

    # Begin forecast. This can take a up to a minute of calculation
    begin_forecast(
      reactive_pop,
      reactive_tfr,
      reactive_e0,
      reactive_mig,
      wpp_starting_year,
      wpp_ending_year,
      input,
      output,
      simulation_results
    )
  })
}
