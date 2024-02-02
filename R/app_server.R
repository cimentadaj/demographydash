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
#' @importFrom OPPPserver get_wpp_pop get_wpp_tfr
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

  # These are the two main  sources of data: population and tfr data.
  # If these are uploaded, they reactively are read from the upload.
  # If not, they are used downloaded directly from WPP.
  reactive_pop <- reactive({
    if (!is.null(input$upload_pop) && nrow(input$upload_pop) > 0) {
      res <- data.table(read.csv(input$upload_pop$datapath))
      names(res) <- c("age", "popF", "popM")
    } else {
      res <- get_wpp_pop(input$wpp_country, wpp_starting_year())
    }
    res
  })

  reactive_tfr <- reactive({
    if (!is.null(input$upload_tfr) && nrow(input$upload_tfr) > 0) {
      res <- data.table(read.csv(input$upload_tfr$datapath))
      names(res) <- c("year", "tfr")
    } else {
      res <- get_wpp_tfr(input$wpp_country)
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
  handle_pop_tfr_plots(reactive_pop, reactive_tfr, wpp_starting_year, wpp_ending_year, input, output)

  # Handle navigation between steps
  handle_navigation(reactive_pop, reactive_tfr, wpp_starting_year, wpp_ending_year, input, output)

  # Handle all customize actions
  handle_customize_data(reactive_pop, reactive_tfr, tfr_starting_year, wpp_starting_year, wpp_ending_year, input, output)

  # Everything that doesn't fit into other handles is here look tooltip server side code.
  handle_misc(wpp_starting_year, wpp_ending_year, input, output)

  # Begin simulation on button click
  observeEvent(input$begin, {
    hide("tfr_page")
    show("forecast_page")

    # Define a reactiveVal to store simulation results
    simulation_results <- reactiveVal()

    # Begin forecast. This can take a up to a minute of calculation
    begin_forecast(
      reactive_pop,
      reactive_tfr,
      wpp_starting_year,
      wpp_ending_year,
      input,
      output,
      simulation_results
    )
  })
}
