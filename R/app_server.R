#' Generate the plot modules after simulation
#'
#' @param sim_res List with datasets for simulation.
#' @importFrom untheme plotWithDownloadButtons
#' @noRd
call_modules <- function(sim_res) {
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
    plotWithDownloadButtons, "plot3",
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
#' @importFrom shiny renderPlot observeEvent callModule renderTable reactive reactiveVal updateNumericInput
#' @importFrom untheme plotWithDownloadButtons
#' @noRd
app_server <- function(input, output, session) {
  reactive_mtcars <- reactive({
    datasets::mtcars[datasets::mtcars$cyl == input$cylinders, ]
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

    simulation_results(run_simulation(reactive_mtcars())) # Update simulation_results

    hide("step2")
    show("step3")

    call_modules(simulation_results)
    updateNumericInput(session, "step", value = 3)
  })

  observeEvent(input$back_to_step2, {
    hide("step3")
    show("step2")
    updateNumericInput(session, "step", value = 2)
  })
}

