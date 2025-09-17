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
        :root { --app-top-offset: 0px; }

        /* App root below host header */
        .dd-app { padding-top: var(--app-top-offset); }

        /* Left menu layout */
        .dd-layout { display: flex; min-height: 100vh; padding-left: 12px; }
        #left_menu { width: 255px; background: transparent; border: none; position: fixed; left: 12px; top: calc(var(--app-top-offset) + (100vh - var(--app-top-offset)) * 0.18); z-index: 2; }
        #left_menu .menu-inner {
          position: sticky;
          top: var(--app-top-offset);
          height: calc((100vh - var(--app-top-offset)) * 0.47);
          overflow: auto;
          padding: 8px;
          background: #ffffff;
          border: 2px solid rgba(0,0,0,0.3);
          border-radius: 10px;
          -webkit-overflow-scrolling: touch;
        }
        #main_content { flex: 1; padding: 0; }

        /* Mobile: stack menu above content */
        @media (max-width: 1200px) {
          .dd-layout { flex-direction: column; padding-left: 0; }
          #left_menu { position: relative; left: auto; top: auto; width: 100%; border-right: none; border-bottom: none; z-index: auto; }
          #left_menu .menu-inner { position: relative; top: 0; height: auto; max-height: none; padding: 8px; }
        }

        /* On wider screens, disable inner sticky so content starts at box top */
        @media (min-width: 1201px) {
          #left_menu .menu-inner { position: static; top: auto; }
        }


        /* Navigation items: keep icon and text on one line */
        #left_menu .ui.list .item { display: flex; align-items: center; gap: 6px; }
        #left_menu .ui.list { margin-left: 0; }
        #left_menu .ui.list .item { padding: 6px 8px; }
        #left_menu .ui.list .item .icon { margin: 0 6px 0 0; }
        /* Make nav list items look clickable */
        #left_menu .ui.list .item.nav-link {
          cursor: pointer;
          display: flex;
          align-items: center;
          gap: 6px;
          border-radius: 6px;
          transition: background-color 0.2s ease;
        }
        #left_menu .ui.list .item.nav-link span {
          color: #1b1c1d;
          text-decoration: none;
          transition: color 0.2s ease;
        }
        #left_menu .ui.list .item.nav-link:hover {
          background-color: rgba(27, 110, 194, 0.08);
        }
        #left_menu .ui.list .item.nav-link:hover span {
          color: #1b6ec2;
        }
        #left_menu .ui.list .item.nav-link.active {
          background-color: rgba(27, 110, 194, 0.12);
        }
        #left_menu .ui.list .item.nav-link.active span {
          color: #1b6ec2;
          font-weight: 600;
        }
        #left_menu .nav-status-icon {
          font-size: 0.7rem;
          color: #b6c0cc;
          transition: color 0.2s ease, transform 0.2s ease;
        }
        #left_menu .nav-status-icon.active {
          color: #1b6ec2;
          transform: scale(1.1);
        }

        /* Space below simulations header */
        #left_menu .sim-header { margin-bottom: 8px; }

        /* Add button: full width, no wrap, spacing below select */
        #left_menu .menu-actions { margin: 8px 0 0 0; }
        #left_menu .menu-actions .ui.button { width: 100%; white-space: nowrap; display: inline-flex; align-items: center; justify-content: center; gap: 6px; padding: 6px 10px; margin: 0; }
        #left_menu .menu-actions .ui.button .icon { margin: 0 6px 0 0; }
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

      /* Simulation pill styles */
      .sim-pill-container {
        display: flex;
        flex-wrap: wrap;
        gap: 6px;
        margin-bottom: 6px;
      }

      .sim-pill {
        display: inline-flex;
        align-items: center;
        border-radius: 999px;
        border: 1px solid transparent;
        padding: 4px 10px;
        background-color: #eef1f6;
        color: #1b1c1d;
        font-size: 0.9em;
        cursor: pointer;
        transition: background-color 0.15s ease, color 0.15s ease, border-color 0.15s ease;
      }

      .sim-pill:hover {
        background-color: #dce6f5;
      }

      .sim-pill.active {
        background-color: #1b6ec2;
        color: #ffffff;
      }

      .sim-pill-remove {
        margin-left: 8px;
        font-size: 0.85em;
        font-weight: 600;
        color: inherit;
        opacity: 0.7;
      }

      .sim-pill-remove:hover {
        opacity: 1;
        color: #f2711c;
      }

      .sim-pill.active .sim-pill-remove:hover {
        color: #ffd7ba;
      }

      .hidden-sim-select {
        display: none !important;
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
    # Full-width app container and left menu
    div(
      class = "dd-app",
      div(
        class = "dd-layout",
        # Left menu (initially hidden; shown from server after leaving landing page)
        shinyjs::hidden(
          div(
            id = "left_menu",
            div(class = "menu-inner",
              uiOutput("sim_header"),
              uiOutput("new_sim_inline"),
              uiOutput("sim_switcher_ui"),
              uiOutput("sim_add_controls"),
              tags$div(class = "ui small header", i18n$translate("Navigation")),
              uiOutput("nav_list_ui")
              ,
              tags$div(class = "ui divider"),
              div(class = "menu-actions",
                  shiny.semantic::action_button("nav_run_projection", i18n$t("Run Projection"), class = "ui primary button")
              )
            )
          )
        ),
        
        # Main content
        div(
          id = "main_content",
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
            style = "display: flex; gap: 10px; align-items: center;",
            action_button("pop_help", i18n$translate("Instructions"), class = "ui blue button"),
            div(
              style = "margin-left: auto;",
              action_button("customize_pop", i18n$translate("Customize"), icon = icon("refresh"), class = "ui blue button")
            )
          ),
          uiOutput("popup_pop"),
          br(),
          withSpinner(uiOutput("show_pop_results_ui"))
        )
      ),
      hidden(
        div(
          id = "tfr_page",
          div(
            style = "display: flex; gap: 10px; justify-content: flex-end;",
            action_button("customize_tfr", i18n$translate("Customize"), icon = icon("refresh"), class = "ui blue button")
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
            style = "display: flex; gap: 10px; justify-content: flex-end;",
            action_button("customize_e0", i18n$translate("Customize"), icon = icon("refresh"), class = "ui blue button")
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
            style = "display: flex; gap: 10px; align-items: center;",
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
            div(uiOutput("forecast_help_ui")),
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
    )),
    # JS to compute host header height and apply offset; sync URL for sim
    tags$script(HTML('
      (function(){
        function computeTopOffset(){
          var selectors = ["header", ".un-header", ".untheme-header", ".ui.top.fixed.menu", ".ui.top.menu"];
          for (var i=0;i<selectors.length;i++){
            var el = document.querySelector(selectors[i]);
            if (el && el.offsetHeight){ return el.offsetHeight; }
          }
          return 0;
        }
        function setTopOffset(){
          var h = computeTopOffset();
          document.documentElement.style.setProperty("--app-top-offset", h+"px");
        }
        function getSimFromSearch(){
          var p = new URLSearchParams(window.location.search);
          return p.get("sim");
        }
        window.addEventListener("resize", setTopOffset);
        document.addEventListener("DOMContentLoaded", setTopOffset);
        $(document).on("shiny:connected", function(){
          setTopOffset();
          var sim = getSimFromSearch();
          if (sim){ Shiny.setInputValue("sim_from_url", sim, {priority: "event"}); }
          window.addEventListener("popstate", function(){
            var sim = getSimFromSearch();
            Shiny.setInputValue("sim_from_url", sim || null, {priority: "event"});
          });
        });
      })();
    '))
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
