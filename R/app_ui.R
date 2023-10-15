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

# Define the run_simulation function
run_simulation <- function(filtered_data) {
  message("Running simulation")
  Sys.sleep(10)
  # For simplicity, let's assume the simulation returns the same dataset
  # In reality, you'd have your simulation logic here
  list(mtcars = filtered_data, iris = datasets::iris)
}

#' Create a field set with a given icon, label, and selectInput
#'
#' @param icon_name Name of the icon.
#' @param label_text Text for the label.
#' @param input_id ID for the select input.
#' @param input_choices Choices for the select input.
#' @param input_selected Selected choice for the select input.
#' @importFrom shiny.semantic icon label
#' @importFrom shiny selectInput
#' @noRd
create_field_set <- function(icon_name, label_text, input_id, input_choices, input_selected) {
  div(
    class = "field",
    icon(icon_name),
    label(
      class = "main label",
      label_text
    ),
    selectInput(
      input_id,
      NULL,
      choices = input_choices,
      selected = input_selected
    )
  )
}

#' The application User-Interface for input page
#'
#' @return A div containing the input page UI elements.
#' @noRd
input_page <- function() {
  div(
    create_field_set("earth-asia", "Select a country", "wpp_country", c("Burundi", "Comoros", "Djibouti"), 1),
    div(
      class = "two fields",
      create_field_set("calendar", "Starting Year", "wpp_starting_year", 2023:2099, 2023),
      create_field_set("calendar", "Ending Year", "wpp_ending_year", 2024:2100, 2024)
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

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @return A shiny semantic UI for the application.
#' @importFrom shiny div selectInput actionButton numericInput uiOutput
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom shiny.semantic main_panel action_button
#' @importFrom untheme fluidUnTheme
#' @importFrom shinycssloaders withSpinner
#' @noRd
app_ui <- function(request) {
  fluidUnTheme(
    useShinyjs(),
    main_panel(
      div(
        id = "step1",
        class = "ui raised very padded text container segment",
        div(
          class = "ui form",
          input_page(),
          action_button("forward", "Next", icon = icon("arrow-right"), class = "ui blue button")
        )
      ),
      hidden(
        div(
          id = "step2",
          actionButton("back_to_step1", "Back"),
          step_one_ui(),
          actionButton("begin", "Begin")
        )
      ),
      hidden(
        div(
          id = "step3",
          actionButton("back_to_step2", "Back"),
          withSpinner(uiOutput("app_tabset"))
        )
      ),
      hidden(
        numericInput("step", label = NULL, value = 1)
      )
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
