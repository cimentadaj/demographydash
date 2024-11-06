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
        /* Custom CSS */
        .footer-container {
            display: flex;
            gap: 10px;
            justify-content: center;
        }

        .file-input-container, .button-container {
            flex: 0;
        }

        .button-container > div {
            display: flex;
            gap: 5px;
            justify-content: center;
        }

       /* Media query for screens smaller than 1400px */
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

        @media (max-width: 780px) {
          .ui.raised.very.padded.container.segment {
              /* Set all padding and margins to 0 to neutralize 'padded' and potentially 'very', 'raised' */
              padding: 0 !important;
              margin: 0 !important;

              /* Reset other properties that may be set by 'ui', 'container', 'segment' classes */
              box-shadow: none !important; /* Assuming 'raised' might add a shadow */
              border: none !important; /* In case any border is added */
            }
          .footer-container {
              flex-direction: column;
              justify-content: center !important;
          }

          .file-input-container, .button-container {
              width: 100%;
          }

          .footer-container > .file-input-container {
              margin-bottom: 5px; /* Decrease space between file input and buttons */
          }

          .footer-container > .button-container {
              margin-bottom: 20px; /* Add some inches of space below the buttons */
          }
        }


      .landing-page {
        max-width: 1200px;
        margin: 0 auto;
      }

      .hero-section {
        text-align: center;
        padding: 4rem 2rem;
        background: linear-gradient(to right, #f8f9fa, #e9ecef);
        border-radius: 10px;
      }

      .hero-title {
        font-size: 3.5rem;
        color: #2c3e50;
        margin-bottom: 1rem;
      }

      .hero-subtitle {
        font-size: 1.5rem;
        color: #6c757d;
        max-width: 800px;
        margin: 0 auto;
      }

      .feature-card {
        transition: transform 0.2s;
        height: 100%;
      }

      .feature-card:hover {
        transform: translateY(-5px);
      }

      .feature-content {
        text-align: center;
        padding: 1rem;
      }

      .feature-icon {
        font-size: 2.5rem;
        color: #2185d0;
        margin-bottom: 1rem;
      }

      .feature-title {
        font-size: 1.5rem;
        color: #2c3e50;
        margin-bottom: 1rem;
      }

      .feature-content ul {
        text-align: left;
        margin-top: 1rem;
        padding-left: 1.5rem;
      }

      .feature-content li {
        margin-bottom: 0.5rem;
        color: #6c757d;
      }

      .cta-section {
        text-align: center;
        padding: 2rem 0;
      }

      .ui.huge.primary.button {
        font-size: 1.25rem;
        padding: 1.25rem 2.5rem;
        background-color: #2185d0;
        color: white;
        border-radius: 30px;
        box-shadow: 0 4px 6px rgba(50, 50, 93, 0.11), 0 1px 3px rgba(0, 0, 0, 0.08);
        transition: all 0.2s;
      }

      .ui.huge.primary.button:hover {
        transform: translateY(-1px);
        box-shadow: 0 7px 14px rgba(50, 50, 93, 0.1), 0 3px 6px rgba(0, 0, 0, 0.08);
        background-color: #1678c2;
      }
      "))
    ),
    tags$script(JS_CODE_SCREEN_SIZE),
    useShinyjs(),
    main_panel(
      # Add the landing page as the first page
      div(id = "landing_page", create_landing_page()),

      # Hide the input page initially
      shinyjs::hidden(
        div(
          id = "input_page",
          class = "ui raised very padded text container segment responsive-container",
          div(
            class = "ui form",
            show_input_ui(),
            uiOutput("next_pop_page")
          )
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
            action_button("forward_e0_page", "Forward", class = "ui blue button"),
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
          id = "e0_page",
          div(
            style = "display: flex; gap: 10px;", # 20px gap between buttons
            action_button("back_to_tfr_page", "Back", class = "ui grey button"),
            action_button("forward_mig_page", "Forward", class = "ui blue button"),
            div(
              style = "margin-left: auto;",
              action_button("customize_e0", "Customize", icon = icon("refresh"), class = "ui blue button")
            )
          ),
          uiOutput("popup_e0"),
          br(),
          withSpinner(uiOutput("show_e0_results_ui")),
        )
      ),
      hidden(
        div(
          id = "mig_page",
          div(
            style = "display: flex; gap: 10px;", # 20px gap between buttons
            action_button("back_to_e0_page", "Back", class = "ui grey button"),
            action_button("begin", "Run Projection", class = "ui blue button"),
            div(
              style = "margin-left: auto;",
              action_button("customize_mig", "Customize", icon = icon("refresh"), class = "ui blue button")
            )
          ),
          uiOutput("popup_mig"),
          br(),
          withSpinner(uiOutput("show_mig_results_ui")),
        )
      ),
      hidden(
        div(
          id = "forecast_page",
          div(
            style = "display: flex; gap: 20px;", # 20px gap between buttons
            action_button("back_to_mig_page", "Back", class = "ui grey button"),
            div(
              style = "margin-left: auto;",
              uiOutput("main_analysis_hover")
            )
          ),
          br(),
          selectInput("select_id", "Results", choices = TAB_NAMES, selected = TAB_NAMES[1]),
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
