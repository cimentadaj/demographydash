#' Generate a tabset for the application
#'
#' @return A tabset UI component for the application
#' @importFrom shiny.semantic tabset
#' @importFrom untheme plotWithDownloadButtonsUI
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
          content = plotWithDownloadButtonsUI("plot2"),
          id = "second_tab"
        ),
        list(
          menu = "Tab 3",
          content = plotWithDownloadButtonsUI("plot3", radio_choices = c("Absolute", "Percentage")),
          id = "third_tab"
        )
      )
    )
  )
}

#' Generate the plot modules after simulation
#'
#' @param sim_res List with datasets for simulation.
#' @importFrom shiny callModule
#' @importFrom untheme plotWithDownloadButtons
#' @noRd
plots_tabset <- function(sim_res) {
  callModule(
    plotWithDownloadButtons,
    "plot1",
    data = sim_res()$mtcars,
    ggplot_obj = create_scatter_plot(sim_res()$mtcars, "mpg", "wt")
  )

  callModule(
    plotWithDownloadButtons,
    "plot2",
    data = sim_res()$iris,
    ggplot_obj = create_scatter_plot(sim_res()$iris, "Sepal.Length", "Sepal.Width")
  )

  callModule(
    plotWithDownloadButtons,
    "plot3",
    data = sim_res()$mtcars,
    ggplot_obj = create_scatter_plot(sim_res()$mtcars, "mpg", "wt"),
    update_ggplot_func = update_ggplot_func
  )
}

#' Create a basic scatter plot
#'
#' @param data The data frame to be used for plotting.
#' @param x_var The name of the column to be used for the x-axis.
#' @param y_var The name of the column to be used for the y-axis.
#' @import ggplot2
#' @return A ggplot2 object
#' @noRd
create_scatter_plot <- function(data, x_var, y_var) {
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point()
}

#' Update a ggplot object's y-axis to percentage scale
#'
#' @param ggplot_obj The ggplot2 object to be updated.
#' @param scale_type The type of scale to be applied.
#' @importFrom scales percent
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
  reactive_mtcars <- reactive({
    datasets::mtcars[datasets::mtcars$cyl == 4, ]
  })

  output$plot0 <- renderPlot({
    data1 <- reactive_mtcars()
    create_scatter_plot(data1, "mpg", "wt")
  })

  output$myTable <- renderTable(reactive_mtcars()[1:5, 1:3])

  observeEvent(input$forward, {
    hide("step1")
    show("step2")
    updateNumericInput(session, "step", value = 2)
  })

  observeEvent(input$back_to_step1, {
    hide("step2")
    show("step1")
    updateNumericInput(session, "step", value = 1)
  })

  # Define a reactiveVal to store simulation results
  simulation_results <- reactiveVal()

  observeEvent(input$begin, {
    output$app_tabset <- renderUI({
      simulation_results(run_simulation(reactive_mtcars())) # Update simulation_results
      app_tabset()
    })

    hide("step2")
    show("step3")

    plots_tabset(simulation_results)
    updateNumericInput(session, "step", value = 3)
  })

  observeEvent(input$back_to_step2, {
    hide("step3")
    show("step2")
    updateNumericInput(session, "step", value = 2)
  })
}
