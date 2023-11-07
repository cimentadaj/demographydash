#' Generate a Tab UI Component for the Application
#'
#' @param tab_name The name to be displayed on the tab.
#' @param plot_ui_id The UI id for the plot.
#' @param extra_ui An optional extra UI component.
#' @importFrom shiny div
#' @importFrom untheme plotWithDownloadButtonsUI
#' @return A list representing a tab UI component.
#' @export
create_tab <- function(tab_name, plot_ui_id, extra_ui = NULL) {
  list(
    menu = tab_name,
    content = plotWithDownloadButtonsUI(plot_ui_id, extra_ui),
    id = paste0(tolower(gsub(" ", "_", tab_name)), "_tab")
  )
}

#' Generate a tabset for the application
#'
#' @return A tabset UI component for the application
#' @importFrom shiny.semantic tabset
#' @importFrom shiny actionButton div
#' @export
app_tabset <- function() {
  tabs <- list(
    create_tab("Projected Population", "plot1", uiOutput("pop_age_sex_years_ui")),
    create_tab("Population by Age Group", "plot2", shiny.semantic::multiple_radio("radio_population_by_broad_age_group", "Scale Type", choices = c("Percent", "Absolute"), type = "inline")),
    create_tab("Population Over Time", "plot3", uiOutput("age_pop_time_ui")),
    create_tab("Projected TFR", "plot4"),
    create_tab("Rate of Population Growth", "plot5"),
    create_tab("Death and Birth Projections", "plot6", shiny.semantic::multiple_radio("radio_death_births", "Type of plot", choices = c("Birth counts", "Birth rates", "Death counts", "Death rates"), type = "inline")),
    create_tab("YADR and OADR", "plot7", shiny.semantic::multiple_radio("radio_yadr_oadr", "Type of plot", choices = c("YADR", "OADR"), type = "inline")),
    create_tab("Population size and Aging", "plot8")
  )

  div(tabset(tabs = tabs))
}

#' UI component for step one
#'
#' @return A div containing UI elements for step one
#' @importFrom shiny actionButton tableOutput
#' @importFrom shiny.semantic grid
#' @importFrom plotly plotlyOutput
#' @export
step_one_ui <- function() {
  div(
    class = "ui raised very padded container segment",
    style = "display: flex; align-items: flex-start; gap: 10px; width: 85%",
    div(
      style = "flex: 3;",
      plotlyOutput("plot_pop", height = "700px")
    ),
    div(
      style = "flex: 1;",
      shiny.semantic::semantic_DTOutput("table_pop", height = "400px")
    )
  )
}

#' UI component for step two
#'
#' @return A div containing UI elements for step two
#' @importFrom shiny actionButton tableOutput
#' @importFrom shiny.semantic grid
#' @importFrom plotly plotlyOutput
#' @export
step_two_ui <- function() {
  div(
    class = "ui raised very padded container segment",
    style = "display: flex; align-items: flex-start; gap: 10px; width: 85%",
    div(
      style = "flex: 3;",
      plotlyOutput("plot_tfr", height = "550px")
    )
  )
}

#' Generate the plot modules after simulation
#'
#' @param ... A list of reactive expressions returning ggplot2 objects.
#' @importFrom shiny callModule observe
#' @importFrom untheme plotWithDownloadButtons
#' @export
plots_tabset <- function(...) {
  observe({
    args <- list(...)
    tab_counter <- 0

    lapply(args, function(arg_reactive) {
      tab_counter <<- tab_counter + 1
      plot_data <- arg_reactive()

      callModule(
        plotWithDownloadButtons,
        paste0("plot", tab_counter),
        ggplot_obj = plot_data
      )
    })
  })
}


#' The Application Server-Side Logic
#'
#' This function defines the server logic for the Shiny application,
#' managing data processing, UI rendering, and routing.
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#' @importFrom shiny reactive reactiveVal renderPlot renderTable observeEvent updateNumericInput renderUI
#' @importFrom shinyjs hide show
#' @importFrom untheme plotWithDownloadButtons
#' @importFrom OPPPserver get_wpp_pop get_wpp_tfr run_forecast remove_forecast
#' @importFrom plotly renderPlotly
#' @export
app_server <- function(input, output, session) {
  # TODO: remove this and import directly run_forecast. Need to fix
  # dependency issue
  library(OPPPserver)
  # Reactive expressions for population and total fertility rate data
  reactive_pop <- reactive(get_wpp_pop(input$wpp_country, input$wpp_starting_year))
  reactive_tfr <- reactive(get_wpp_tfr(input$wpp_country))

  # Render plots for population pyramid and total fertility rate
  output$plot_pop <- renderPlotly(create_pop_pyramid(reactive_pop())$plotly)
  output$plot_tfr <- renderPlotly(create_tfr_plot(reactive_tfr())$plotly)

  # Render population table
  output$table_pop <- DT::renderDataTable(prepare_pop_agegroups_table(reactive_pop()))

  # Handle navigation between steps
  handle_navigation(reactive_pop, reactive_tfr, input, output)

  # Define a reactiveVal to store simulation results
  simulation_results <- reactiveVal()

  # Begin simulation on button click
  observeEvent(input$begin, {
    hide("step3")
    show("step4")

    begin_simulation(input, simulation_results, output)
  })
}

#' Handle Navigation Between Steps
#'
#' This function manages the navigation between different steps of the application.
#'
#' @param input,output Internal parameters for `{shiny}`.
#' @importFrom shinyjs hide show
#' @importFrom shiny renderUI
#' @noRd
handle_navigation <- function(reactive_pop, reactive_tfr, input, output) {
  observeEvent(input$forward_step2, {
    hide("step1")
    show("step2")

    # Added this twice because it allows the spinner around step_one_ui to
    # register the time spent
    create_pop_pyramid(reactive_pop())
    output$step_one_ui <- renderUI(step_one_ui())
  })

  observeEvent(input$forward_step3, {
    hide("step2")
    show("step3")

    # Added this twice because it allows the spinner around step_two_ui to
    # register the time spent
    create_tfr_plot(reactive_tfr())
    output$step_two_ui <- renderUI(step_two_ui())
  })

  observeEvent(input$back_to_step1, {
    hide("step2")
    show("step1")
  })

  observeEvent(input$back_to_step2, {
    hide("step3")
    show("step2")
  })

  observeEvent(input$back_to_step3, {
    hide("step4")
    show("step3")
  })
}

#' Begin Simulation
#'
#' This function manages the simulation process and updates the UI accordingly.
#'
#' @param input Internal parameter for `{shiny}`.
#' @param simulation_results A reactive value to store simulation results.
#' @param output Internal parameter for `{shiny}`.
#' @importFrom shiny reactive renderUI req
#' @importFrom shiny.semantic selectInput
#' @importFrom shinyjs hide show
#' @noRd
begin_simulation <- function(input, simulation_results, output) {
  forecast_res <- reactive({
    # TODO: update 2021 year
    start_year <- ifelse(
      as.numeric(input$wpp_starting_year) %in% 2022:2023,
      2021,
      as.numeric(input$wpp_starting_year)
    )

    run_forecast(
      country = input$wpp_country,
      start_year = start_year,
      end_year = as.numeric(input$wpp_ending_year),
      output_dir = "/tmp/hasdaney213/"
    )
  })

  output$app_tabset <- renderUI({
    simulation_results(forecast_res()) # Update simulation_results
    app_tabset()
  })

  pop_age_sex_years <- reactive({
    req(simulation_results())
    unique(simulation_results()$population_by_age_and_sex$year)
  })

  output$pop_age_sex_years_ui <- renderUI({
    selectInput(
      inputId = "pop_age_sex_years",
      label = "Select year",
      choices = pop_age_sex_years(),
      selected = pop_age_sex_years()[1]
    )
  })

  pyramid_plot <- reactive({
    req(simulation_results(), input$pop_age_sex_years)
    create_pop_pyramid(
      simulation_results()$population_by_age_and_sex,
      input$pop_age_sex_years
    )
  })

  age_group_plot <- reactive({
    req(input$radio_population_by_broad_age_group)
    create_age_group_plot(
      simulation_results()$population_by_broad_age_group,
      input$radio_population_by_broad_age_group
    )
  })

  age_pop_time <- reactive({
    req(simulation_results())
    ages <- unique(simulation_results()$population_by_time$age)
  })

  output$age_pop_time_ui <- renderUI({
    selectInput(
      inputId = "age_pop_time",
      label = "Select age group",
      choices = age_pop_time(),
      selected = age_pop_time()[1]
    )
  })

  pop_time_plot <- reactive({
    req(input$age_pop_time)
    create_pop_time_plot(simulation_results()$population_by_time, input$age_pop_time)
  })

  tfr_projected_plot <- reactive({
    create_tfr_projected_plot(
      simulation_results()$tfr_by_time,
      as.numeric(input$wpp_ending_year)
    )
  })

  annual_growth_plot <- reactive({
    create_annual_growth_plot(
      simulation_results()$annual_growth_rate,
      as.numeric(input$wpp_ending_year)
    )
  })

  deaths_births_plot <- reactive({
    type_value <- tolower(strsplit(input$radio_death_births, " ")[[1]])
    create_deaths_births_plot(
      simulation_results()$births_counts_rates,
      simulation_results()$deaths_counts_rates,
      type_value[1],
      type_value[2],
      as.numeric(input$wpp_ending_year)
    )
  })

  yadr_oadr_plot <- reactive({
    type_value <- tolower(input$radio_yadr_oadr)
    create_yadr_oadr_plot(
      simulation_results()$oadr,
      simulation_results()$yadr,
      type_value,
      as.numeric(input$wpp_ending_year)
    )
  })

  pop_size_aging_plot <- reactive({
    create_pop_aging_pop_size_plot(
      simulation_results()$pop_aging_and_pop_size,
      as.numeric(input$wpp_ending_year)
    )
  })

  plots_tabset(
    pyramid_plot,
    age_group_plot,
    pop_time_plot,
    tfr_projected_plot,
    annual_growth_plot,
    deaths_births_plot,
    yadr_oadr_plot,
    pop_size_aging_plot
  )
}
