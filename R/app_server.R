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
#' @param input,output,session Internal parameters for `{shiny}`.
#'
#' @importFrom shiny reactive reactiveVal observeEvent renderUI observe
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
          i18n$translate("This is the starting year data for this indicator. If you upload new data, it should exactly this format: same number of columns, same order of columns and importantly, the same metric. Some of these indicators are in expressed in thousands, for example."),
          i18n$translate("If you want to upload your own data, a good strategy is to download the current data and adapt it to your needs. That way you can keep the expected format and only add your own values."),
          i18n$translate("Finally, when your data is ready, click on the 'Browse' button to upload your CSV file. We expect a CSV file formatted exactly as the table above. Once uploaded, the table should update with your new values.")
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
    result <- file_parser(input$upload_pop, "pop", i18n)
    if (result$success) {
      file_input_pop(input$upload_pop)
    } else {
      shinyalert(
        title = i18n$t("Error"),
        text = paste(i18n$translate("Population file validation failed:"), result$message),
        type = "error"
      )
    }
  })

  observeEvent(input$upload_tfr, {
    result <- file_parser(input$upload_tfr, "tfr", i18n)
    if (result$success) {
      file_input_tfr(input$upload_tfr)
    } else {
      shinyalert(
        title = i18n$t("Error"),
        text = paste(i18n$translate("TFR file validation failed:"), result$message),
        type = "error"
      )
    }
  })

  observeEvent(input$upload_e0, {
    result <- file_parser(input$upload_e0, "e0", i18n)
    if (result$success) {
      file_input_e0(input$upload_e0)
    } else {
      shinyalert(
        title = i18n$t("Error"),
        text = paste(i18n$translate("Life expectancy file validation failed:"), result$message),
        type = "error"
      )
    }
  })

  observeEvent(input$upload_mig, {
    result <- file_parser(input$upload_mig, "mig", i18n)
    if (result$success) {
      file_input_mig(input$upload_mig)
    } else {
      shinyalert(
        title = i18n$translate("Error"),
        text = paste(i18n$translate("Migration file validation failed:"), result$message),
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
      data_source$tfr <- "uploaded"
    } else {
      res <- get_wpp_tfr(input$wpp_country)
      data_source$tfr <- "downloaded"
    }

    res
  })

  reactive_e0 <- reactive({
    if (!is.null(get_file_input_e0())) {
      res <- data.table(readr::read_csv(get_file_input_e0()$datapath))
      names(res) <- c("year", "e0M", "e0F")
      data_source$e0 <- "uploaded"
    } else {
      res <- get_wpp_e0(input$wpp_country)
      data_source$e0 <- "downloaded"
    }

    res
  })

  reactive_mig <- reactive({
    if (!is.null(get_file_input_mig())) {
      res <- data.table(readr::read_csv(get_file_input_mig()$datapath))
      names(res) <- c("year", "mig")
      data_source$mig <- "uploaded"
    } else {
      res <- get_wpp_mig(input$wpp_country)
      data_source$mig <- "downloaded"
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
    reactive_pop,
    reactive_tfr,
    reactive_e0,
    reactive_mig,
    wpp_starting_year,
    wpp_ending_year,
    current_tab,
    input,
    output,
    i18n
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
    current_tab,
    input,
    output,
    i18n
  )


  # Everything that doesn't fit into other handles is here look tooltip server side code.
  handle_misc(wpp_starting_year, wpp_ending_year, input, output, i18n)

  # Begin simulation on button click
  observeEvent(input$begin, {
    hide("mig_page")
    show("forecast_page")

    # Define a reactiveVal to store simulation results
    simulation_results <- reactiveVal()

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




## library(rintrojs)
## library(shiny)
## library(shiny.semantic)

## # Define UI for application that draws a histogram
## ui <- semanticPage(
##   introjsUI(),

##   # Application title
##   introBox(
##     titlePanel("Old Faithful Geyser Data"),
##     data.step = 1,
##     data.intro = "This is the title panel"
##   ),

##   # Sidebar with a slider input for number of bins
##   sidebarLayout(
##     sidebarPanel(
##       introBox(
##         introBox(
##           shiny.semantic::slider_input(
##             "bins",
##             value = 30,
##             min = 1,
##             max = 50,
##           ),
##           data.step = 3,
##           data.intro = "This is a slider"
##         ),
##         introBox(
##           actionButton("help", "Press for instructions"),
##           data.step = 4,
##           data.intro = "This is a button"
##         ),
##         data.step = 2,
##         data.intro = "This is the sidebar. Look how intro elements can nest"
##       )
##     ),

##     # Show a plot of the generated distribution
##     mainPanel(
##       introBox(
##         plotOutput("distPlot"),
##         data.step = 5,
##         data.intro = "This is the main plot"
##       )
##     )
##   )
## )

## # Define server logic required to draw a histogram
## server <- function(input, output, session) {
##   output$distPlot <- renderPlot({
##     # generate bins based on input$bins from ui.R
##     x <- faithful[, 2]
##     bins <- seq(min(x), max(x), length.out = input$bins + 1)


##     # draw the histogram with the specified number of bins
##     hist(x,
##       breaks = bins,
##       col = "darkgray",
##       border = "white"
##     )
##   })

##   # start introjs when button is pressed with custom options and events
##   observeEvent(
##     input$help,
##     introjs(
##       session,
##       options = list(
##         "nextLabel" = "Onwards and Upwards",
##         "prevLabel" = "Did you forget something?",
##         "skipLabel" = "Don't be a quitter"
##       )
##     )
##   )
## }

## # Run the application
## shinyApp(ui = ui, server = server)

## create_modal_ui <- function(modal_id, header_title, output_id, file_input_id, download_button_id, hide_button_id, additional_header = NULL) {
##   modal(
##     id = modal_id,
##     header = div(
##       div(
##         style = "display: flex; justify-content: space-between;",
##         header_title,
##         action_button("customize_help", "Instructions", class = "ui red button")
##       ),
##       additional_header,
##     ),
##     introBox(
##       DTOutput(output_id),
##       data.step = 4,
##       data.intro = "This is the starting year data for this indicator. If you upload new data, it should exactly this format: same number of columns, same order of columns and importantly, the same metric. Some of these indicators are in expressed in thousands, for example."
##     ),
##     footer = div(
##       div(
##         class = "footer-container",
##         div(
##           class = "file-input-container",
##           div(
##             style = "display: flex; align-items: center; gap: 5px;",
##             introBox(
##               shiny.semantic::fileInput(file_input_id, label = NULL, placeholder = "Upload CSV file", width = "100%"),
##               data.step = 5,
##               data.intro = "Finally, when your data is ready, click here to upload your CSV file. We expect a CSV file formatted exactly as the table above. Once uploaded, the table should update with your new values."
##             )
##           )
##         ),
##         div(
##           class = "button-container",
##           div(
##             style = "display: flex; gap: 5px",
##             introBox(
##               shiny::downloadButton(download_button_id, "Download", class = "ui blue button"),
##               data.step = {,
##               data.intro = "If you want to upload your own data, a good strategy is to download the current data and adapt it to your needs. That way you can keep the expected format and only add your own values."
##             )
##           )
##         ),
##       ),
##       div("Uploaded data must match exactly the format, column names and ordering shown in the table above", style = "color: #8B0000; font-weight: bold; font-size: 12px;")
##     ),
##     class = "small"
##   )
## }
