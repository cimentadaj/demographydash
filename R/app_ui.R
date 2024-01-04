#' @param request Internal parameter for `{shiny}`.
#' @return A shiny semantic UI for the application.
#' @importFrom shiny div actionButton numericInput uiOutput br
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom shiny.semantic main_panel action_button selectInput icon
#' @importFrom untheme fluidUnTheme
#' @importFrom shinycssloaders withSpinner
#' @noRd
app_ui <- function(request) {
  fluidUnTheme(
    tags$head(
      tags$style(HTML("
        @media (max-width: 1400px) {
          .responsive-container {
            flex-direction: column;
            width: 100% !important;
          }
          .responsive-container > div {
            flex: 1 1 auto;
            width: 100% !important;
          }
        }
      "))
    ),
    useShinyjs(),
    main_panel(
      div(
        id = "input_page",
        class = "ui raised very padded text container segment responsive-container",
        div(
          class = "ui form",
          show_input_ui(),
          uiOutput("next_pop_page")
        )
      ),
      hidden(
        div(
          id = "pop_page",
          div(
            style = "display: flex; gap: 10px;", # 20px gap between buttons
            action_button("back_to_input_page", "Back", class = "ui grey button"),
            action_button("forward_tfr_page", "Forward", class = "ui blue button"),
            div(
              style = "margin-left: auto;",
              action_button("customize_pop", "Customize", icon = icon("refresh"), class = "ui blue button")
            )
          ),
          uiOutput("popup_pop"),
          uiOutput("pass_tfr"),
          br(),
          withSpinner(uiOutput("show_pop_results_ui"))
        )
      ),
      hidden(
        div(
          id = "tfr_page",
          div(
            style = "display: flex; gap: 10px;", # 20px gap between buttons
            action_button("back_to_pop_page", "Back", class = "ui grey button"),
            action_button("begin", "Calculate", class = "ui blue button"),
            div(
              style = "margin-left: auto;",
              action_button("customize_tfr", "Customize", icon = icon("refresh"), class = "ui blue button")
            )
          ),
          uiOutput("popup_tfr"),
          br(),
          withSpinner(uiOutput("show_tfr_results_ui")),
        )
      ),
      hidden(
        div(
          id = "forecast_page",
          div(
            style = "display: flex; gap: 20px;", # 20px gap between buttons
            action_button("back_to_tfr_page", "Back", class = "ui grey button"),
            div(
              style = "margin-left: auto;",
              uiOutput("main_analysis_hover")
            )
          ),
          br(),
          selectInput("select_id", "Choose a plot:", choices = TAB_NAMES, selected = TAB_NAMES[1]),
          br(),
          withSpinner(uiOutput("show_forecast_results_ui"))
        )
      ),
      hidden(
        numericInput("step", label = NULL, value = 1)
      ),
      width = NULL
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
