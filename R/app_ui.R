#' Define a simple grid layout for the application
#'
#' @return A grid template for shiny.semantic layout
#' @importFrom shiny.semantic grid_template
#' @noRd
mySimpleGrid <- function() {
  grid_template(
    default = list(
      areas = rbind(
        c("center", "right")
      ),
      rows_height = c("auto"),
      cols_width = c("2fr", "1fr")
    )
  )
}

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

# Define the run_simulation function
run_simulation <- function(filtered_data) {
  message("Running simulation")
  Sys.sleep(10)
  # For simplicity, let's assume the simulation returns the same dataset
  # In reality, you'd have your simulation logic here
  list(mtcars = filtered_data, iris = datasets::iris)
}

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @return A shiny semantic UI for the application.
#' @importFrom shiny div selectInput actionButton numericInput
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom shiny.semantic main_panel
#' @importFrom untheme fluidUnTheme
#' @noRd
# The application User-Interface
app_ui <- function(request) {
  fluidUnTheme(
    useShinyjs(),
    main_panel(
      div(
        id = "step1",
        selectInput("cylinders", "Select Number of Cylinders:",
          choices = unique(datasets::mtcars$cyl),
          selected = 6
        ),
        actionButton("forward", "Forward")
      ),
      hidden(
        div(
          id = "step2",
          actionButton("back_to_step1", "Back"),
          step_one_ui(),
          actionButton("begin", "Begin"),
        )
      ),
      hidden(
        div(
          id = "step3",
          actionButton("back_to_step2", "Back"),
          app_tabset()
        )
      ),
      hidden(
        numericInput("step", label = NULL, value = 1)
      )
    )
  )
}

#' UI component for step one
#'
#' @return A div containing UI elements for step one
#' @importFrom shiny actionButton plotOutput tableOutput
#' @importFrom shiny.semantic grid
#' @noRd
step_one_ui <- function() {
  div(
    id = "step1",
    grid(
      mySimpleGrid(),
      container_style = "border: 3px solid #4D4D4D",
      area_styles = list(
        center = "background: #4D4D4D",
        right = "border-left: 3px solid #4D4D4D"
      ),
      center = plotOutput("plot0"), # Replace with your plot
      right = tableOutput("myTable") # Replace with your table
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom shiny tags
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "demographydash"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
