#' @param request Internal parameter for `{shiny}`.
#' @return A shiny semantic UI for the application.
#' @importFrom shiny div actionButton numericInput uiOutput br
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom shiny.semantic main_panel action_button selectInput icon
#' @importFrom untheme fluidUnTheme
#' @importFrom shinycssloaders withSpinner
#' @importFrom shiny.i18n usei18n
#' @importFrom rintrojs introjsUI
#' @noRd
app_ui <- function(request) {
  i18n <- usei18n_local()

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
      
      /* Override untheme .content width constraint for modals */
      .ui.modal > .content {
        max-width: 100% !important;
        min-width: 100% !important;
      }
      
      /* Also override for accordion content within modals */
      .ui.modal .ui.accordion .content {
        max-width: 100% !important;
        min-width: 100% !important;
      }
      "))
    ),
    tags$script(JS_CODE_SCREEN_SIZE),
    shiny.i18n::usei18n(i18n),
    useShinyjs(),
    introjsUI(),
    main_panel(
      # Add the landing page as the first page
      div(id = "landing_page", create_landing_page(i18n)),

      # Hide the input page initially
      shinyjs::hidden(
        div(
          id = "input_page",
          class = "ui raised very padded text container segment responsive-container",
          div(
            class = "ui form",
            show_input_ui(i18n),
            uiOutput("next_pop_page")
          )
        )
      ),

      hidden(
        div(
          id = "pop_page",
          div(
            style = "display: flex; gap: 10px;", # 20px gap between buttons
            action_button("back_to_input_page", i18n$translate("< Previous"), class = "ui grey button"),
            action_button("forward_tfr_page", i18n$translate("Next >"), class = "ui blue button"),
            action_button("pop_help", i18n$translate("Instructions"), class = "ui blue button"),
            div(
              style = "margin-left: auto;",
              action_button("customize_pop", i18n$translate("Customize"), icon = icon("refresh"), class = "ui blue button")
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
            action_button("back_to_pop_page", i18n$translate("< Previous"), class = "ui grey button"),
            action_button("forward_e0_page", i18n$translate("Next >"), class = "ui blue button"),
            div(
              style = "margin-left: auto;",
              action_button("customize_tfr", i18n$translate("Customize"), icon = icon("refresh"), class = "ui blue button")
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
            action_button("back_to_tfr_page", i18n$translate("< Previous"), class = "ui grey button"),
            action_button("forward_mig_page", i18n$translate("Next >"), class = "ui blue button"),
            div(
              style = "margin-left: auto;",
              action_button("customize_e0", i18n$translate("Customize"), icon = icon("refresh"), class = "ui blue button")
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
            action_button("back_to_e0_page", i18n$translate("< Previous"), class = "ui grey button"),
            action_button("begin", i18n$translate("Run Projection"), class = "ui blue button"),
            div(
              style = "margin-left: auto;",
              action_button("customize_mig", i18n$translate("Customize"), icon = icon("refresh"), class = "ui blue button")
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
            div(
              style = "display: flex; gap: 5px;",
              action_button("back_to_mig_page", i18n$translate("< Previous"), class = "ui grey button"),
              uiOutput("forecast_help_ui")
            ),
            div(
              style = "margin-left: auto; display: flex; gap: 10px; align-items: center;",
              downloadButton("download_report", i18n$translate("Download Report"), class = "ui blue button"),
              uiOutput("main_analysis_hover")
            )
          ),
          br(),
          uiOutput("select_plot_tab"),
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
