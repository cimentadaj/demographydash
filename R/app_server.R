#' Generate a tabset for the application
#'
#' @return A tabset UI component for the application
#' @importFrom shiny.semantic tabset
#' @importFrom shiny actionButton div
#' @noRd
app_tabset <- function() {
  div(
    tabset(
      tabs = list(
        list(
          menu = "Tab 1",
          content = plotWithDownloadButtonsUI("plot1"),
          id = "first_tab"
        ),
        list(
          menu = "Tab 2",
          content = plotWithDownloadButtonsUI("plot2", shiny.semantic::multiple_radio("scaleType", "Scale Type", choices = c("Percent", "Absolute"), type = "inline")),
          id = "second_tab"
        ),
        list(
          menu = "Tab 3",
          content = plotWithDownloadButtonsUI("plot3"),
          id = "third_tab"
        )
      )
    )
  )
}

plotWithDownloadButtonsUI <- function(id, radio_button = NULL) {
  ns <- shiny::NS(id)

  layout <-
    shiny.semantic::sidebar_layout(
      shiny.semantic::sidebar_panel(
        radio_button,
        shiny::br(),
        shiny::downloadButton(ns("downloadPlot"), "Download Plot"),
        shiny::downloadButton(ns("downloadData"), "Download Data")
      ),
      shiny.semantic::main_panel(
        shiny::plotOutput(ns("plot"), height = "600px", width = "900px"),
        width = 4
      )
    )

  layout
}

#' UI component for step one
#'
#' @return A div containing UI elements for step one
#' @importFrom shiny actionButton plotOutput tableOutput
#' @importFrom shiny.semantic grid
#' @noRd
step_one_ui <- function() {
  div(
    class = "ui raised very padded container segment",
    style = "display: flex; align-items: flex-start; gap: 10px; width: 85%",
    div(
      style = "flex: 3;",
      plotOutput("plot_pop", height = "550px")
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
#' @importFrom shiny actionButton plotOutput tableOutput
#' @importFrom shiny.semantic grid
#' @noRd
step_two_ui <- function() {
  div(
    class = "ui raised very padded container segment",
    style = "display: flex; align-items: flex-start; gap: 10px; width: 85%",
    div(
      style = "flex: 3;",
      plotOutput("plot_tfr", height = "550px")
    )
  )
}

#' Generate the plot modules after simulation
#'
#' @param sim_res List with datasets for simulation.
#' @importFrom shiny callModule
#' @importFrom untheme plotWithDownloadButtons
#' @noRd
plots_tabset <- function(sim_res_reactive, input_reactive, age_group_dt_reactive) {
  observe({
    # Access the values of reactive expressions within a reactive context
    sim_res <- sim_res_reactive()
    input <- input_reactive()
    age_group_dt <- age_group_dt_reactive()

    # Now you can use sim_res, input, and age_group_dt as normal variables within this reactive context

    callModule(
      plotWithDownloadButtons,
      "plot1",
      data = sim_res$population_by_age_and_sex,
      ggplot_obj = create_pop_pyramid(sim_res$population_by_age_and_sex)
    )

    callModule(
      plotWithDownloadButtons,
      "plot2",
      data = age_group_dt,
      ggplot_obj = create_age_group_plot(age_group_dt)
    )

    callModule(
      plotWithDownloadButtons,
      "plot3",
      data = sim_res$mtcars,
      ggplot_obj = NULL,
      update_ggplot_func = update_ggplot_func
    )
  })
}


#' Update a ggplot object's y-axis to percentage scale
#'
#' @param ggplot_obj The ggplot2 object to be updated.
#' @param scale_type The type of scale to be applied.
#' @importFrom scales percent
#' @importFrom ggplot2 scale_y_continuous
#' @return An updated ggplot2 object
#' @noRd
update_ggplot_func <- function(ggplot_obj, scale_type) {
  if (scale_type == "Percentage") {
    ggplot_obj + scale_y_continuous(labels = percent)
  } else {
    ggplot_obj
  }
}

#' The application server-side logic
#'
#' This function defines the server logic for the Shiny application, managing data processing, UI rendering, and routing.
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#' @importFrom shinyjs hide show
#' @importFrom shiny reactive reactiveVal renderPlot renderTable observeEvent updateNumericInput renderUI
#' @importFrom untheme plotWithDownloadButtons
#' @noRd
app_server <- function(input, output, session) {
  reactive_pop <- reactive(OPPPserver::get_wpp_pop(input$wpp_country, input$wpp_starting_year))
  reactive_tfr <- reactive(OPPPserver::get_wpp_tfr(input$wpp_country))
  output$plot_pop <- renderPlot(create_pop_pyramid(reactive_pop()))
  output$plot_tfr <- renderPlot(create_tfr_plot(reactive_tfr()))
  output$table_pop <- DT::renderDataTable(prepare_pop_agegroups_table(reactive_pop()))

  observeEvent(input$forward_step2, {
    output$step_one_ui <- renderUI(step_one_ui())
    hide("step1")
    show("step2")
  })

  observeEvent(input$forward_step3, {
    output$step_two_ui <- renderUI(step_two_ui())
    hide("step2")
    show("step3")
  })

  observeEvent(input$back_to_step1, {
    hide("step2")
    show("step1")
  })

  observeEvent(input$back_to_step2, {
    hide("step3")
    show("step2")
  })

  # Define a reactiveVal to store simulation results
  simulation_results <- reactiveVal()

  observeEvent(input$begin, {
    output$app_tabset <- renderUI({
      library(OPPPserver)

      start_year <- ifelse(
        as.numeric(input$wpp_starting_year) %in% 2022:2023,
        2021,
        as.numeric(input$wpp_starting_year)
      )

      print(input$wpp_country)
      print(start_year)
      print(input$wpp_ending_year)

      forecast <<-
        OPPPserver::run_forecast(
          country = input$wpp_country,
          start_year = start_year,
          end_year = as.numeric(input$wpp_ending_year)
        )

      simulation_results(forecast) # Update simulation_results
      app_tabset()
    })

    hide("step3")
    show("step4")

    print(names(input))
    print(input[["scaleType"]])

    age_group_dt <- reactive({
      req(input$scaleType)
      res <- prepare_age_group(
        simulation_results()$population_by_broad_age_group,
        input$scaleType
      )
      res
    })

    plots_tabset(simulation_results, reactive(input), age_group_dt)

  })

  observeEvent(input$back_to_step3, {
    hide("step4")
    show("step3")

    OPPPserver::remove_forecast(forecast)
  })
}
