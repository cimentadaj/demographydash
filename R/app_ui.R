#' The application User-Interface for input page
#'
#' @return A div containing the input page UI elements.
#' @importFrom untheme create_field_set
#' @noRd
input_page <- function() {
  div(
    create_field_set(
      "globe",
      "Select a country",
      "wpp_country",
      OPPPserver::get_wpp_countries(),
      NULL
    ),
    div(
      class = "two fields",
      create_field_set("calendar", "Starting Year", "wpp_starting_year", 2023:2099, 2023),
      create_field_set("calendar", "Ending Year", "wpp_ending_year", 2024:2100, 2100)
    )
  )
}

#' The application User-Interface
#'
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
    useShinyjs(),
    main_panel(
      div(
        id = "step1",
        class = "ui raised very padded text container segment",
        div(
          class = "ui form",
          input_page(),
          action_button("forward_step2", "Next", class = "ui blue button")
        )
      ),
      hidden(
        div(
          id = "step2",
          div(
            style = "display: flex; gap: 10px;", # 20px gap between buttons
            action_button("back_to_step1", "Back", class = "ui grey button"),
            action_button("forward_step3", "Forward", class = "ui blue button"),
            div(
              style = "margin-left: auto;",
              action_button("customize_pop", "Customize", icon = icon("refresh"), class = "ui blue button")
            )
          ),
          uiOutput("popup_pop"),
          uiOutput("pass_tfr"),
          br(),
          withSpinner(uiOutput("step_one_ui")),
        )
      ),
      hidden(
        div(
          id = "step3",
          div(
            style = "display: flex; gap: 10px;", # 20px gap between buttons
            action_button("back_to_step2", "Back", class = "ui grey button"),
            action_button("begin", "Calculate", class = "ui blue button"),
            div(
              style = "margin-left: auto;",
              action_button("customize_tfr", "Customize", icon = icon("refresh"), class = "ui blue button")
            )
          ),
          uiOutput("popup_tfr"),
          br(),
          withSpinner(uiOutput("step_two_ui")),
        )
      ),
      hidden(
        div(
          id = "step4",
          div(
            style = "display: flex; gap: 20px;", # 20px gap between buttons
            action_button("back_to_step3", "Back", class = "ui grey button"),
            div(
              style = "margin-left: auto;",
              uiOutput("hover")
            )
          ),
          br(),
          withSpinner(uiOutput("app_tabset"))
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
