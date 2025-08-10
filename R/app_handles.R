#' Create Landing Page UI
#'
#' @description
#' Creates a modern, informative landing page that introduces users to the app's functionality
#'
#' @param i18n The internationalization object for translations.
#' @importFrom shiny div h1 h2 p HTML
#' @importFrom shiny.semantic grid grid_template segment
#' @importFrom shiny.semantic icon
#' @export
create_landing_page <- function(i18n) {
  # Define the main grid template
  main_grid <- grid_template(
    default = list(
      areas = rbind(
        c("hero", "hero", "hero"),
        c("feature1", "feature2", "feature3"),
        c("cta", "cta", "cta")
      ),
      rows_height = c("auto", "auto", "auto"),
      cols_width = c("1fr", "1fr", "1fr")
    )
  )

  div(
    class = "landing-page",
    grid(
      main_grid,
      container_style = "gap: 2rem; padding: 2rem;",

      # Hero Section
      hero = div(
        class = "hero-section",
        h1(i18n$translate("Population Projection Tool"), class = "hero-title"),
        p(i18n$translate("Create detailed population projections using United Nations World Population Prospects data or your own custom inputs"),
          class = "hero-subtitle"
        )
      ),

      # Feature 1: Population Data
      feature1 = segment(
        class = "ui raised segment feature-card",
        div(
          class = "feature-content",
          div(class = "feature-icon", icon("users")),
          h2(class = "feature-title", i18n$translate("Population Data")),
          p(i18n$translate("Upload or use UN WPP data for:")),
          HTML(paste0("
            <ul>
              <li>", i18n$translate("Base population by age and sex"), "</li>
              <li>", i18n$translate("Total fertility rates"), "</li>
              <li>", i18n$translate("Life expectancy"), "</li>
              <li>", i18n$translate("Migration patterns"), "</li>
            </ul>
          "))
        )
      ),

      # Feature 2: Interactive Visualization
      feature2 = segment(
        class = "ui raised segment feature-card",
        div(
          class = "feature-content",
          div(class = "feature-icon", icon("chart bar")),
          h2(class = "feature-title", i18n$translate("Interactive Visualization")),
          p(i18n$translate("Explore your data through:")),
          HTML(paste0("
            <ul>
              <li>", i18n$translate("Population pyramids"), "</li>
              <li>", i18n$translate("Time series charts"), "</li>
              <li>", i18n$translate("Customizable projections"), "</li>
              <li>", i18n$translate("Downloadable results"), "</li>
            </ul>
          "))
        )
      ),

      # Feature 3: Data Requirements
      feature3 = segment(
        class = "ui raised segment feature-card",
        div(
          class = "feature-content",
          div(class = "feature-icon", icon("exchange")),
          h2(class = "feature-title", i18n$translate("Scenario Comparison")),
          p(i18n$translate("Compare your projections with UN WPP:")),
          HTML(paste0("
      <ul>
        <li>", i18n$translate("Side-by-side visualization"), "</li>
        <li>", i18n$translate("Compare with UN median variant"), "</li>
        <li>", i18n$translate("Validate your assumptions"), "</li>
        <li>", i18n$translate("Export comparison results"), "</li>
      </ul>
    "))
        )
      ),

      # CTA Section
      cta = div(
        class = "cta-section",
        action_button("start_analysis", i18n$translate("Start Your Analysis"),
          class = "ui huge primary button"
        )
      )
    )
  )
}

#' Render Plots Before Analysis
#'
#' @param reactive_pop A reactive expression returning the population data.
#' @param reactive_tfr A reactive expression returning the TFR (Total Fertility Rate) data.
#' @param reactive_e0 A reactive expression returning the e0 data.
#' @param reactive_mig A reactive expression returning the mig data.
#' @param wpp_starting_year A reactive expression returning the starting year.
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param input,output Internal parameters for `\{shiny\}`.
#' @param i18n The internationalization object.
#'
#' @importFrom DT renderDT
#' @importFrom plotly ggplotly renderPlotly
#' @export
#'
handle_before_analysis_plots <- function(reactive_pop, reactive_tfr, reactive_e0, reactive_mig, wpp_starting_year, wpp_ending_year, input, output, i18n = NULL) {
  # Render plots for population pyramid and total fertility rate
  output$plot_pop <- renderPlotly(
    create_pop_pyramid_plot(
      reactive_pop(),
      country = input$wpp_country,
      input_year = wpp_starting_year(),
      i18n = i18n
    )$plotly
  )

  output$plot_tfr_custom <- renderPlotly(
    create_tfr_plot(
      reactive_tfr(),
      end_year = wpp_ending_year(),
      country = input$wpp_country,
      i18n
    )$plotly
  )

  output$plot_e0_custom <- renderPlotly(
    create_e0_plot(
      reactive_e0(),
      end_year = wpp_ending_year(),
      country = input$wpp_country,
      i18n
    )$plotly
  )

  output$plot_mig_custom <- renderPlotly(
    create_mig_plot(
      reactive_mig(),
      end_year = wpp_ending_year(),
      country = input$wpp_country,
      i18n
    )$plotly
  )


  # Render population table
  output$table_pop <- renderDT(prepare_pop_agegroups_table(reactive_pop(), i18n))
}

#' Create a Custom Modal UI for Shiny
#'
#' @description
#' Generates a modal UI element for customizing the TFR and Population data.
#' This function provides a flexible way to create modals with various elements like
#' file inputs, download buttons, and custom headers.
#'
#' @param modal_id A string specifying the ID of the modal.
#' @param header_title A string containing the title text for the modal header.
#' @param output_id A string specifying the ID of the DT output to be displayed in the modal.
#' @param file_input_id A string specifying the ID for the fileInput element.
#' @param download_button_id A string specifying the ID for the downloadButton element.
#' @param hide_button_id A string specifying the ID for the actionButton used to close the modal.
#' @param additional_header (Optional) An additional UI element to be included in the header.
#'        Default is NULL.
#' @param i18n The internationalization object.
#'
#' @return A modal UI element.
#'
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shiny.semantic fileInput modal
#' @export
#'
create_modal_ui <- function(modal_id, header_title, output_id, file_input_id, download_button_id, hide_button_id, additional_header = NULL, i18n = NULL) {
  modal(
    id = modal_id,
    header = div(
      div(
        style = "display: flex; justify-content: space-between;",
        header_title,
        action_button("customize_help", if (!is.null(i18n)) i18n$t("Instructions") else "Instructions", class = "ui red button")
      ),
      additional_header,
    ),
    rHandsontableOutput(output_id),
    footer = div(
      div(
        class = "footer-container",
        div(
          class = "button-container",
          div(
            style = "display: flex; gap: 5px",
            shiny::downloadButton(download_button_id, if (!is.null(i18n)) i18n$t("Download") else "Download", class = "ui blue button"),
            actionButton(paste0(modal_id, "_ok_btn"), if (!is.null(i18n)) i18n$t("Apply") else "Apply", class = "ui positive button")
          )
        ),
      ),
      br(),
      div(i18n$t("Edit the table or paste your own data form an Excel sheet. Click on the 'Apply' button to save your changes."), style = "color: #8B0000; font-weight: bold; font-size: 12px;")
    ),
    class = "small"
  )
}

#' Create Enhanced Population Modal UI
#'
#' @description
#' Creates an enhanced modal specifically for population data that supports both
#' UN WPP data and custom data input with transformation capabilities.
#'
#' @param modal_id A string specifying the ID of the modal.
#' @param header_title A string containing the title text for the modal header.
#' @param output_id A string specifying the ID of the DT output to be displayed in the modal.
#' @param file_input_id A string specifying the ID for the fileInput element.
#' @param download_button_id A string specifying the ID for the downloadButton element.
#' @param hide_button_id A string specifying the ID for the actionButton used to close the modal.
#' @param ref_year The reference year to use as default for the date input.
#' @param additional_header (Optional) An additional UI element to be included in the header.
#' @param i18n The internationalization object.
#'
#' @return A modal UI element with enhanced functionality.
#'
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shiny.semantic fileInput modal icon selectInput
#' @importFrom shiny tags div HTML conditionalPanel numericInput dateInput h5
#' @importFrom shinyjs useShinyjs
#' @export
#'
create_enhanced_population_modal_ui <- function(modal_id, header_title, output_id, file_input_id, download_button_id, hide_button_id, ref_year, additional_header = NULL, i18n = NULL) {
  modal(
    id = modal_id,
    header = div(
      div(
        style = "display: flex; justify-content: space-between;",
        header_title,
        action_button("customize_help", if (!is.null(i18n)) i18n$t("Instructions") else "Instructions", class = "ui red button")
      ),
      # Phase 1: Add data source toggle - using Semantic UI
      div(
        class = "ui two item menu",
        style = "margin-top: 15px;",
        tags$a(
          id = paste0(modal_id, "_source_un"),
          class = "item active",
          onclick = paste0("$('#", modal_id, "_source_un').addClass('active'); $('#", modal_id, "_source_custom').removeClass('active'); Shiny.setInputValue('", modal_id, "_source', 'UN Data');"),
          icon("database"),
          if (!is.null(i18n)) i18n$t("UN Data") else "UN Data"
        ),
        tags$a(
          id = paste0(modal_id, "_source_custom"),
          class = "item",
          onclick = paste0("$('#", modal_id, "_source_custom').addClass('active'); $('#", modal_id, "_source_un').removeClass('active'); Shiny.setInputValue('", modal_id, "_source', 'Custom Data');"),
          icon("upload"),
          if (!is.null(i18n)) i18n$t("Custom Data") else "Custom Data"
        )
      ),
      # Unified Data Configuration Accordion
      div(
        id = paste0(modal_id, "_data_config_accordion"),
        class = "ui styled accordion",
        style = "margin-top: 15px; width: 100%;",
        div(
          class = "title",
          style = "background-color: #f8f8f9; padding: 12px;",
          icon("caret right"),
          tags$span(style = "font-weight: 600; margin-left: 8px;", if (!is.null(i18n)) i18n$t("Data Configuration") else "Data Configuration")
        ),
        div(
          class = "content",
          style = "padding: 15px;",
          # Dynamic content based on data source
          conditionalPanel(
            condition = paste0("input.", modal_id, "_source == null || input.", modal_id, "_source == 'UN Data'"),
            div(
              class = "ui form",
              div(
                class = "field",
                style = "width: 200px;",
                tags$label(style = "font-size: 0.9em;", if (!is.null(i18n)) i18n$t("Age Type") else "Age Type"),
                selectInput(
                  paste0(modal_id, "_un_age_type"),
                  NULL,
                  choices = c("Single Ages", "5-Year Groups"),
                  selected = "Single Ages",
                  width = "100%"
                )
              )
            )
          ),
          conditionalPanel(
            condition = paste0("input.", modal_id, "_source == 'Custom Data'"),
            div(
              class = "custom-modal-two-col",
              style = "display: grid; grid-template-columns: 1fr 1fr; gap: 24px; align-items: start; width: 100%;",
              # Left column - Data Structure
              div(
                class = "ui form",
                h5(class = "ui header", style = "margin-top: 0;", if (!is.null(i18n)) i18n$t("Data Structure") else "Data Structure"),
                div(
                  style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px;",
                  div(
                    class = "field",
                    style = "margin-bottom: 0;",
                    tags$label(style = "font-size: 0.9em;", if (!is.null(i18n)) i18n$t("Age Type") else "Age Type"),
                    selectInput(
                      paste0(modal_id, "_age_type"),
                      NULL,
                      choices = c("Single Ages", "5-Year Groups"),
                      selected = "Single Ages",
                      width = "100%"
                    )
                  ),
                  div(
                    class = "field",
                    style = "margin-bottom: 0;",
                    tags$label(
                      style = "font-size: 0.9em;", 
                      if (!is.null(i18n)) i18n$t("Open Age Group") else "Open Age Group",
                      tags$i(
                        class = "info circle icon",
                        style = "margin-left: 5px; color: #999; cursor: help;",
                        `data-content` = if (!is.null(i18n)) i18n$t("The oldest age group that will aggregate all ages above it (e.g., 85+ means all people 85 and older)") else "The oldest age group that will aggregate all ages above it (e.g., 85+ means all people 85 and older)",
                        `data-variation` = "tiny"
                      ),
                      tags$span(
                        id = paste0(modal_id, "_oag_status"),
                        style = "margin-left: 10px; font-size: 0.8em; color: #e74c3c; display: none;",
                        "\u226535"
                      )
                    ),
                    numericInput(
                      paste0(modal_id, "_oag"),
                      NULL,
                      value = 100,
                      min = 35,
                      max = 120,
                      step = 5,
                      width = "100%"
                    )
                  )
                ),
                # Always show date below the grid
                div(
                  class = "ui form",
                  style = "margin-top: 12px;",
                  div(
                    class = "field",
                    tags$label(
                      style = "font-size: 0.9em;", 
                      if (!is.null(i18n)) i18n$t("Reference Date") else "Reference Date",
                      tags$i(
                        class = "info circle icon",
                        style = "margin-left: 5px; color: #999; cursor: help;",
                        `data-content` = if (!is.null(i18n)) i18n$t("The date your population data represents. Used to align projections to specific time points") else "The date your population data represents. Used to align projections to specific time points",
                        `data-variation` = "tiny"
                      )
                    ),
                    dateInput(
                      paste0(modal_id, "_ref_date"),
                      NULL,
                      value = paste0(ref_year, "-12-31"),
                      width = "100%"
                    )
                  )
                )
              ),
              # Right column - Processing Methods
              div(
                class = "ui form",
                h5(class = "ui header", style = "margin-top: 0;", if (!is.null(i18n)) i18n$t("Processing Methods") else "Processing Methods"),
                # Show interpolation options only for 5-Year Groups
                conditionalPanel(
                  condition = paste0("input.", modal_id, "_age_type == '5-Year Groups'"),
                  div(
                    class = "field",
                    style = "margin-bottom: 10px;",
                    tags$label(
                      style = "font-size: 0.9em;", 
                      if (!is.null(i18n)) i18n$t("Interpolation") else "Interpolation",
                      tags$i(
                        class = "info circle icon",
                        style = "margin-left: 5px; color: #999; cursor: help;",
                        `data-content` = if (!is.null(i18n)) i18n$t("Method to convert 5-year age groups into single ages. Beers Ordinary is recommended for demographic data") else "Method to convert 5-year age groups into single ages. Beers Ordinary is recommended for demographic data",
                        `data-variation` = "tiny"
                      )
                    ),
                    selectInput(
                      paste0(modal_id, "_interp_method"),
                      NULL,
                      choices = c(
                        "UN" = "un",
                        "Sprague" = "sprague",
                        "Beers Ordinary" = "beers(ord)",
                        "Beers Modified" = "beers(mod)",
                        "Grabill" = "grabill",
                        "Monotonic Spline" = "mono",
                        "Uniform" = "uniform",
                        "PCLM" = "pclm"
                      ),
                      selected = "un",
                      width = "100%"
                    )
                  )
                ),
                # Show note when extrapolation will be applied
                conditionalPanel(
                  condition = paste0("(input.", modal_id, "_age_type == 'Single Ages' && input.", modal_id, "_oag != 100) || (input.", modal_id, "_age_type == '5-Year Groups' && input.", modal_id, "_oag != 100)"),
                  div(
                    style = "color: #666; font-size: 0.9em; padding: 10px; background-color: #fef9e7; border-radius: 4px; margin-top: 15px;",
                    icon("info circle"),
                    if (!is.null(i18n)) i18n$t("OAG will be automatically extended to 100+") else "OAG will be automatically extended to 100+"
                  )
                ),
                # Show message when no extrapolation needed
                conditionalPanel(
                  condition = paste0("input.", modal_id, "_age_type == '5-Year Groups' && input.", modal_id, "_oag == 100"),
                  div(
                    style = "color: #666; font-size: 0.9em; padding: 10px; background-color: #f8f8f9; border-radius: 4px; margin-top: 15px;",
                    icon("info circle"),
                    if (!is.null(i18n)) i18n$t("No extrapolation needed for OAG 100+") else "No extrapolation needed for OAG 100+"
                  )
                ),
                # Show message when no processing needed at all
                conditionalPanel(
                  condition = paste0("input.", modal_id, "_age_type == 'Single Ages' && input.", modal_id, "_oag == 100"),
                  div(
                    style = "color: #666; font-size: 0.9em; padding: 10px; background-color: #f8f8f9; border-radius: 4px;",
                    icon("info circle"),
                    if (!is.null(i18n)) i18n$t("No processing required for single ages with OAG 100+") else "No processing required for single ages with OAG 100+"
                  )
                )
              )
            )
          )  # End of conditionalPanel for Custom Data
        )  # End of accordion content
      )  # End of unified accordion
    ),
    # World-class UI/UX notice about data format
    div(
      class = "ui message",
      style = "margin: 0 0 10px 0; background-color: #f3f4f6; border-left: 4px solid #3b82f6;",
      div(
        style = "display: flex; align-items: flex-start; gap: 12px;",
        icon("info circle", style = "color: #3b82f6; font-size: 20px; margin-top: 2px;"),
        div(
          div(
            style = "font-weight: 600; color: #1f2937; margin-bottom: 4px;",
            if (!is.null(i18n)) i18n$t("Important: Population values are in thousands") else "Important: Population values are in thousands"
          ),
          div(
            style = "color: #6b7280; font-size: 14px; line-height: 1.5;",
            if (!is.null(i18n)) {
              i18n$t("Values in thousands \u2022 Decimals allowed: 450.2251 = 450,225 people \u2022 Commas OK: 3,551.917 = 3.55 million people")
            } else {
              "Values in thousands \u2022 Decimals allowed: 450.2251 = 450,225 people \u2022 Commas OK: 3,551.917 = 3.55 million people"
            }
          )
        )
      )
    ),
    rHandsontableOutput(output_id),
    footer = div(
      div(
        class = "footer-container",
        div(
          class = "button-container",
          div(
            style = "display: flex; gap: 5px",
            shiny::downloadButton(download_button_id, if (!is.null(i18n)) i18n$t("Download") else "Download", class = "ui blue button"),
            actionButton(paste0(modal_id, "_ok_btn"), if (!is.null(i18n)) i18n$t("Apply") else "Apply", class = "ui positive button")
          )
        ),
      ),
      br(),
      div(i18n$t("Edit the table or paste your own data form an Excel sheet. Click on the 'Apply' button to save your changes."), style = "color: #8B0000; font-weight: bold; font-size: 12px;")
    ),
    class = "small scrolling"
  )
}

#' Create Header Content for Modals in Shiny
#'
#' @description
#' Constructs a header content `div` for use with create_modal_ui, allowing for
#' additional text and custom styling.
#'
#' @param text A string for the primary text content of the header.
#' @param additional_text (Optional) Additional text to be included in the header. Default is NULL.
#' @param additional_style (Optional) A string specifying CSS styles to be applied to the header. Default is an empty string.
#'
#' @return A `div` element representing the header content.
#'
#' @importFrom shiny tags
#' @export
#'
create_header_content <- function(text, additional_text = NULL, additional_style = "") {
  div(
    tags$em(text),
    style = additional_style
  )
}

#' Handles Customization of Data in the App
#'
#' This function sets up the UI elements to display and customize population and total fertility rate (TFR) data. It renders data tables for population and TFR data and sets up modals for data customization.
#'
#' @param current_pop_reactive A reactive expression that returns the population data to be displayed.
#' @param current_tfr_reactive A reactive expression that returns the TFR data to be displayed.
#' @param current_e0_reactive A reactive expression that returns the e0 data to be displayed.
#' @param current_mig_reactive A reactive expression that returns the migration data to be displayed.
#' @param pop_to_commit_rv A reactive value to commit the population data.
#' @param tfr_to_commit_rv A reactive value to commit the TFR data.
#' @param e0_to_commit_rv A reactive value to commit the e0 data.
#' @param mig_to_commit_rv A reactive value to commit the migration data.
#' @param tfr_starting_year A reactive expression returning the TFR starting year.
#' @param wpp_starting_year A reactive expression returning the starting year.
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param current_tab A reactive value for the current tab.
#' @param input,output Internal parameters for `\{shiny\}`.
#' @param i18n The internationalization object.
#'
#' @importFrom shiny renderUI div br showNotification
#' @importFrom shiny.semantic fileInput action_button modal
#' @importFrom rhandsontable renderRHandsontable rhandsontable rHandsontableOutput hot_col
#' @importFrom OPPPserver get_wpp_pop
#'
#' @return None
#' @export
#'
handle_customize_data <- function(
    current_pop_reactive, current_tfr_reactive, current_e0_reactive, current_mig_reactive,
    pop_to_commit_rv, tfr_to_commit_rv, e0_to_commit_rv, mig_to_commit_rv,
    pop_data_source, tfr_starting_year, wpp_starting_year, wpp_ending_year, current_tab, input, output, i18n = NULL
) {
  output$location_selector <- renderUI(location_selector_ui(input, i18n))

  observeEvent(input$customize_pop, {
    show_modal("modal_population")
    current_tab("modal_pop")
  })

  observeEvent(input$customize_tfr, {
    show_modal("modal_tfr")
    current_tab("modal_tfr")
  })

  observeEvent(input$customize_e0, {
    show_modal("modal_e0")
    current_tab("modal_e0")
  })

  observeEvent(input$customize_mig, {
    show_modal("modal_mig")
    current_tab("modal_mig")
  })

  # Add reactive values for enhanced population modal
  un_data_5yr <- reactiveVal(NULL)  # Cache for 5-year grouped UN data
  custom_data <- reactiveVal(NULL)  # Store custom data separately
  accordion_state <- reactiveVal(list(data_config = FALSE))  # Track accordion state
  
  # Clear 5-year cache when reference date changes
  observeEvent(input$modal_population_ref_date, {
    un_data_5yr(NULL)  # Reset cache so it fetches fresh data
  })
  
  # Update OAG display and validation
  observeEvent(input$modal_population_oag, {
    oag <- input$modal_population_oag
    if (!is.null(oag)) {
      # Check if OAG is valid
      if (is.na(oag) || oag < 35) {
        shinyjs::show("modal_population_oag_status")
      } else {
        shinyjs::hide("modal_population_oag_status")
      }
    }
  })
  
  # Track accordion state changes
  observeEvent(input$accordion_data_config_open, {
    current <- accordion_state()
    current$data_config <- input$accordion_data_config_open
    accordion_state(current)
  })
  
  # Initialize accordion when modal opens
  observeEvent(input$customize_pop, {
    shinyjs::delay(200, {  # Wait for modal to render
      # Get saved state
      state <- accordion_state()
      
      # Use the extracted function for accordion initialization
      js_code <- initialize_population_modal_accordion("modal_population", state$data_config)
      shinyjs::runjs(js_code)
    })
  })
  
  # Re-initialize popups when switching between data sources
  observeEvent(input$modal_population_source, {
    # Re-initialize popups after content change
    shinyjs::delay(50, {
      shinyjs::runjs("
        // Re-initialize popups for new content
        $('.info.circle.icon').popup({
          hoverable: true,
          position: 'top center',
          delay: {
            show: 300,
            hide: 0
          }
        });
      ")
    })
  })

  output$tmp_pop_dt <- renderRHandsontable({
    # Enhanced table rendering for population modal
    data_source <- input$modal_population_source
    
    if (is.null(data_source) || data_source == "UN Data") {
      # Show UN data based on age type selection
      un_age_type <- input$modal_population_un_age_type %||% "Single Ages"
      
      if (un_age_type == "Single Ages") {
        # Use the pre-loaded single age data
        res <- current_pop_reactive()
      } else {
        # 5-Year Groups - check cache first
        if (is.null(un_data_5yr())) {
          # Only fetch if not cached
          ref_date <- input$modal_population_ref_date
          ref_year <- if (!is.null(ref_date)) {
            if (is.character(ref_date)) as.numeric(format(as.Date(ref_date), "%Y"))
            else if (inherits(ref_date, "Date")) as.numeric(format(ref_date, "%Y"))
            else wpp_starting_year()
          } else wpp_starting_year()
          
          # Fetch and cache the 5-year grouped data
          print("ref_year")
          print(ref_year)
          data_5yr <- get_wpp_pop(input$wpp_country, year = ref_year, n = 5)
          data_5yr$age <- round(data_5yr$age)  # Clean up decimal ages
          # Format ages to match custom data pattern
          data_5yr$age <- format_5year_age_labels(data_5yr$age)
          un_data_5yr(data_5yr)
        }
        res <- un_data_5yr()
      }
      
      # Rename columns based on their actual names, not position
      col_names <- names(res)
      if ("age" %in% col_names) names(res)[names(res) == "age"] <- i18n$t("Age")
      if ("popM" %in% col_names) names(res)[names(res) == "popM"] <- i18n$t("Male (in thousands)")
      if ("popF" %in% col_names) names(res)[names(res) == "popF"] <- i18n$t("Female (in thousands)")
      # Ensure correct column order - Males first
      res <- res[, c(i18n$t("Age"), i18n$t("Male (in thousands)"), i18n$t("Female (in thousands)"))]
    } else {
      # Custom data mode
      age_type <- input$modal_population_age_type
      oag <- input$modal_population_oag
      
      # Use defaults if not yet set
      if (is.null(age_type)) age_type <- "Single Ages"
      if (is.null(oag) || is.na(oag) || oag < 35) {
        # Return empty table with error message if OAG is invalid
        return(rhandsontable(
          data.frame(
            Age = i18n$t("Invalid OAG value"),
            Female = NA,
            Male = NA,
            stringsAsFactors = FALSE
          ),
          rowHeaders = NULL,
          readOnly = TRUE,
          stretchH = "all",
          height = 100
        ))
      }
      
      # Check if we have saved custom data with matching configuration
      saved_custom <- custom_data()
      if (!is.null(saved_custom) && 
          nrow(saved_custom) == length(generate_age_labels(age_type, oag))) {
        # Use saved custom data
        res <- saved_custom
        # Rename columns based on their actual names, not position
        col_names <- names(res)
        if ("age" %in% col_names) names(res)[names(res) == "age"] <- i18n$t("Age")
        if ("popM" %in% col_names) names(res)[names(res) == "popM"] <- i18n$t("Male (in thousands)")
        if ("popF" %in% col_names) names(res)[names(res) == "popF"] <- i18n$t("Female (in thousands)")
        # Ensure correct column order - Males first
        res <- res[, c(i18n$t("Age"), i18n$t("Male (in thousands)"), i18n$t("Female (in thousands)"))]
      } else {
        # Generate empty template
        age_labels <- generate_age_labels(age_type, oag)
        res <- data.frame(
          Age = age_labels,
          Male = NA_real_,
          Female = NA_real_,
          stringsAsFactors = FALSE
        )
        names(res) <- c(
          i18n$t("Age"), 
          i18n$t("Male (in thousands)"),
          i18n$t("Female (in thousands)")
        )
      }
    }
    
    rhandsontable(
      res,
      rowHeaders = NULL,
      useTypes = TRUE,
      stretchH = "all",
      height = 400,
      minSpareRows = 0,
      maxRows = nrow(res)
    ) %>%
      hot_col(i18n$t("Age"), readOnly = TRUE) %>%
      hot_col(c(i18n$t("Female (in thousands)"), i18n$t("Male (in thousands)")), type = "numeric", format = "0,0")
  })

  output$tmp_tfr_dt <- renderRHandsontable({
    res <- current_tfr_reactive()
    names(res) <- c(
      i18n$t("Year"), 
      i18n$t("TFR")
    )
    rhandsontable(
      res,
      useTypes = TRUE,
      stretchH = "all",
      height = 400
    )
  })

  output$tmp_e0_dt <- renderRHandsontable({
    res <- current_e0_reactive()
    names(res) <- c(
      i18n$t("Year"), 
      i18n$t("Males"), 
      i18n$t("Females")
    )
    rhandsontable(
      res,
      useTypes = TRUE,
      stretchH = "all",
      height = 400
    )
  })

  output$tmp_mig_dt <- renderRHandsontable({
    res <- current_mig_reactive()
    names(res) <- c(
      i18n$t("Year"), 
      i18n$t("Migration")
    )
    rhandsontable(
      res,
      useTypes = TRUE,
      stretchH = "all",
      height = 400
    )
  })


  output$popup_pop <- renderUI({
    # Get reference year from wpp_starting_year
    ref_year <- wpp_starting_year()
    
    create_enhanced_population_modal_ui(
      modal_id = "modal_population",
      header_title = paste0(i18n$t("Population data for"), " ", input$wpp_country, " ", i18n$t("in base year"), " ", ref_year),
      output_id = "tmp_pop_dt",
      file_input_id = "upload_pop",
      download_button_id = "download_pop",
      hide_button_id = "hide_pop",
      ref_year = ref_year,
      additional_header = NULL,  # The enhanced modal already includes the thousands notice
      i18n = i18n
    )
  })


  output$popup_tfr <- renderUI({
    create_modal_ui(
      modal_id = "modal_tfr",
      header_title = paste0(i18n$t("Total Fertility Rate for"), " ", input$wpp_country, " ", i18n$t("in analysis period.")),
      output_id = "tmp_tfr_dt",
      file_input_id = "upload_tfr",
      download_button_id = "download_tfr",
      hide_button_id = "hide_tfr",
      i18n = i18n
    )
  })


  output$popup_e0 <- renderUI({
    create_modal_ui(
      modal_id = "modal_e0",
      header_title = paste0(i18n$t("Life Expectancy for"), " ", input$wpp_country, " ", i18n$t("in analysis period.")),
      output_id = "tmp_e0_dt",
      file_input_id = "upload_e0",
      download_button_id = "download_e0",
      hide_button_id = "hide_e0",
      i18n = i18n
    )
  })

  output$popup_mig <- renderUI({
    create_modal_ui(
      modal_id = "modal_mig",
      header_title = paste0(i18n$t("Migration data for"), " ", input$wpp_country, " ", i18n$t("in analysis period.")),
      output_id = "tmp_mig_dt",
      file_input_id = "upload_mig",
      download_button_id = "download_mig",
      hide_button_id = "hide_mig",
      i18n = i18n
    )
  })

  # ADD observeEvent blocks for the new "Ok" buttons
  observeEvent(input$modal_population_ok_btn, {
    req(input$tmp_pop_dt)
    
    tryCatch({
      # Get data from table
      data <- rhandsontable::hot_to_r(input$tmp_pop_dt)
      
      # Apply transformations if using custom data OR if using UN 5-year data
      data_source <- input$modal_population_source
      needs_transformation <- FALSE
      
      if (!is.null(data_source) && data_source == "Custom Data") {
        # Custom data always needs transformation check
        needs_transformation <- TRUE
        # Get transformation parameters
        age_type <- input$modal_population_age_type %||% "Single Ages"
        oag_current <- input$modal_population_oag %||% 100
        interp_method <- input$modal_population_interp_method %||% "un"
        
        # Extract year from reference date
        ref_year <- NULL
        ref_date <- input$modal_population_ref_date
        if (!is.null(ref_date)) {
          if (is.character(ref_date)) {
            ref_year <- as.numeric(format(as.Date(ref_date), "%Y"))
          } else if (inherits(ref_date, "Date")) {
            ref_year <- as.numeric(format(ref_date, "%Y"))
          }
        }
        
        # Save the raw custom data (before transformation) for later use
        raw_data <- data
        # Map column names properly - data comes from table with translated headers
        col_mapping <- list()
        col_mapping[[i18n$t("Age")]] <- "age"
        col_mapping[[i18n$t("Male (in thousands)")]] <- "popM"
        col_mapping[[i18n$t("Female (in thousands)")]] <- "popF"
        
        for (old_name in names(col_mapping)) {
          if (old_name %in% names(raw_data)) {
            names(raw_data)[names(raw_data) == old_name] <- col_mapping[[old_name]]
          }
        }
        # Ensure we have the right columns in standard order (age, popF, popM for backend)
        raw_data <- raw_data[, c("age", "popF", "popM")]
        custom_data(raw_data)
        
      } else {
        # UN Data mode - check if 5-year groups selected
        un_age_type <- input$modal_population_un_age_type %||% "Single Ages"
        if (un_age_type == "5-Year Groups") {
          needs_transformation <- TRUE
          age_type <- "5-Year Groups"
          oag_current <- 100  # WPP data always has OAG 100+
          interp_method <- "un"  # Default method
          ref_year <- wpp_starting_year()
        }
      }
      
      # Apply transformation if needed
      if (needs_transformation) {
        # Standardize column names for transformation - map properly by name
        col_mapping <- list()
        col_mapping[[i18n$t("Age")]] <- "age"
        col_mapping[[i18n$t("Male (in thousands)")]] <- "popM"
        col_mapping[[i18n$t("Female (in thousands)")]] <- "popF"
        
        for (old_name in names(col_mapping)) {
          if (old_name %in% names(data)) {
            names(data)[names(data) == old_name] <- col_mapping[[old_name]]
          }
        }
        # Ensure we have the right columns in standard order (age, popF, popM for backend)
        data <- data[, c("age", "popF", "popM")]
        
        # Apply transformation pipeline
        data <- transform_population_data(
          data, 
          age_type = age_type,
          oag_current = oag_current,
          oag_target = 100,  # Always target 100 for consistency
          interp_method = interp_method,
          country = input$wpp_country,
          ref_year = ref_year
        )
      } else {
        # Single age UN data - standardize column names properly
        col_mapping <- list()
        col_mapping[[i18n$t("Age")]] <- "age"
        col_mapping[[i18n$t("Male (in thousands)")]] <- "popM"
        col_mapping[[i18n$t("Female (in thousands)")]] <- "popF"
        
        for (old_name in names(col_mapping)) {
          if (old_name %in% names(data)) {
            names(data)[names(data) == old_name] <- col_mapping[[old_name]]
          }
        }
        # Ensure we have the right columns in standard order (age, popF, popM for backend)
        data <- data[, c("age", "popF", "popM")]
      }
      
      # Update reactive value (this is the final data used by the app)
      pop_to_commit_rv(data)
      
      # Update data source tracker
      if (!is.null(data_source) && data_source == "Custom Data") {
        pop_data_source("Custom Data")
      } else {
        pop_data_source("UN Data")
      }
      
      # Close modal
      shiny.semantic::hide_modal("modal_population")
      
      # Show success message
      showNotification(i18n$t("Population data updated successfully"), type = "message", duration = 3)
      
    }, error = function(e) {
      # Show error message to user
      showNotification(
        paste(i18n$t("Error:"), e$message), 
        type = "error", 
        duration = 10
      )
    })
  })

  observeEvent(input$modal_tfr_ok_btn, {
    req(input$tmp_tfr_dt)
    current_data_from_table <- rhandsontable::hot_to_r(input$tmp_tfr_dt)
    tfr_to_commit_rv(current_data_from_table)
    shiny.semantic::hide_modal("modal_tfr")
  })

  observeEvent(input$modal_e0_ok_btn, {
    req(input$tmp_e0_dt)
    current_data_from_table <- rhandsontable::hot_to_r(input$tmp_e0_dt)
    e0_to_commit_rv(current_data_from_table)
    shiny.semantic::hide_modal("modal_e0")
  })

  observeEvent(input$modal_mig_ok_btn, {
    req(input$tmp_mig_dt)
    current_data_from_table <- rhandsontable::hot_to_r(input$tmp_mig_dt)
    mig_to_commit_rv(current_data_from_table)
    shiny.semantic::hide_modal("modal_mig")
  })

  cnt_years <-
    reactive({
      paste0(
        tolower(gsub(" ", "", input$wpp_country)),
        "_",
        wpp_starting_year()
      )
    })

  tfr_years <-
    reactive({
      paste0(
        tolower(gsub(" ", "", input$wpp_country)),
        "_",
        tfr_starting_year(),
        "_",
        wpp_ending_year()
      )
    })

  output$download_pop <- shiny::downloadHandler(
    filename = function() paste0("population_", cnt_years(), ".csv"),
    content = function(file) {
      # Download exactly what's visible in the table
      req(input$tmp_pop_dt)
      data <- rhandsontable::hot_to_r(input$tmp_pop_dt)
      write.csv(data, file, row.names = FALSE)
    }
  )

  output$download_tfr <- shiny::downloadHandler(
    filename = function() paste0("tfr_", tfr_years(), ".csv"),
    content = function(file) write.csv(current_tfr_reactive(), file, row.names = FALSE)
  )

  output$download_e0 <- shiny::downloadHandler(
    filename = function() paste0("e0_", tfr_years(), ".csv"),
    content = function(file) write.csv(current_e0_reactive(), file, row.names = FALSE)
  )

  output$download_mig <- shiny::downloadHandler(
    filename = function() paste0("mig_", tfr_years(), ".csv"),
    content = function(file) write.csv(current_mig_reactive(), file, row.names = FALSE)
  )
}

#' Handle Navigation Between Steps
#'
#' This function manages the navigation between different pages of the application.
#'
#' @param simulation_results A reactive expression returning the simulation results.
#' @param reactive_pop A reactive expression that returns the population data to be displayed.
#' @param reactive_tfr A reactive expression that returns the TFR data to be displayed.
#' @param reactive_e0 A reactive expression that returns the e0 data to be displayed.
#' @param reactive_mig A reactive expression that returns the migration data to be displayed.
#' @param wpp_starting_year A reactive expression returning the starting year.
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param current_tab A reactive value for the current tab.
#' @param input,output Internal parameters for `\{shiny\}`.
#' @param i18n The internationalization object.
#'
#' @importFrom shinyjs hide show
#' @importFrom shiny renderUI HTML
#' @importFrom shiny.semantic show_modal hide_modal
#' @importFrom shinyalert shinyalert
#' @importFrom shiny showNotification
#' @importFrom utils write.csv
#' @export
#'
handle_navigation <- function(simulation_results, reactive_pop, reactive_tfr, reactive_e0, reactive_mig, pop_data_source, wpp_starting_year, wpp_ending_year, current_tab, input, output, i18n = NULL) {

  processing <- reactiveVal(TRUE)
  show_tfr_modal <- reactiveVal(TRUE)

  observeEvent(input$start_analysis, {
    hide("landing_page")
    show("input_page")
    current_tab("input_page")
  })

  # In handle_navigation function, add this new observer:
  observeEvent(input$back_to_landing, {
    hide("input_page")
    show("landing_page")
    current_tab("landing_page")
  })

  observeEvent(input$back_to_input_page, {
    hide("pop_page")
    show("input_page")
    current_tab("input_page")
  })

  observeEvent(input$back_to_pop_page, {
    hide("tfr_page")
    show("pop_page")
    current_tab("pop_page")
  })

  observeEvent(input$back_to_tfr_page, {
    hide("e0_page")
    show("tfr_page")
    current_tab("tfr_page")
  })

  observeEvent(input$back_to_e0_page, {
    hide("mig_page")
    show("e0_page")
    current_tab("e0_page")
  })

  observeEvent(input$back_to_mig_page, {
    hide("forecast_page")
    show("mig_page")
    current_tab("mig_page")
  })

  observeEvent(input$forward_pop_page, {
    hide("input_page")
    show("pop_page")
    current_tab("pop_page")

    output$show_pop_results_ui <- renderUI({
      create_pop_pyramid_plot(
        reactive_pop(),
        country = input$wpp_country,
        input_year = wpp_starting_year(),
        i18n = i18n
      )

      res <- show_pop_results_ui(data_source = pop_data_source(), i18n = i18n)
      processing(FALSE)
      res
    })
  })

  # After the processing is finished, show the shinyalert modal
  observeEvent(processing(),
    {
      if (!processing()) {
        shinyalert(
          title = i18n$t("Explore UN Estimates \xF0\x9F\x8C\x90"),
          text = tags$div(
            style = "text-align: left;",
            HTML(
              i18n$t("\xF0\x9F\x94\xA2 The data shown here are estimates from the United Nations<br/>
              \xF0\x9F\x94\x84 Click 'Customize' to enter your own data<br/>
              \xF0\x9F\xA7\xAE Upload: starting population by sex and single year of age with an open age interval of 100+")
            )
          ),
          type = "info",
          html = TRUE,
          closeOnEsc = TRUE,
          showConfirmButton = TRUE,
          confirmButtonText = i18n$t("Got it!")
        )
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(input$forward_tfr_page, {
    if (show_tfr_modal()) {
      show_modal("modal_passtfr")
    } else {
      show_tfr(reactive_tfr, wpp_ending_year, input, output, i18n)
    }

    show_tfr_modal(FALSE)
  })

  observeEvent(input$change_source_btn, {
    show_tfr(reactive_tfr, wpp_ending_year, input, output, i18n)
  })

  observeEvent(input$forward_e0_page, {
    hide_modal("modal_passtfr")
    hide("tfr_page")
    show("e0_page")
    show_e0(reactive_e0, wpp_ending_year, input, output, i18n)
  })

  observeEvent(input$forward_mig_page, {
    hide("e0_page")
    show("mig_page")
    show_mig(reactive_mig, wpp_ending_year, input, output, i18n)
  })


  observeEvent(input$pass_source_btn, {
    hide_modal("modal_passtfr")
    hide("pop_page")
    show("forecast_page")
    current_tab("forecast_page")

    # Compute in the background because sincer we're
    # jumping the actual TFR (jumping the traditional TFR page
    # which calculates the TFR), we need to compute the TFR
    # before the simulation.
    compute_tfr(reactive_tfr, wpp_ending_year, input, output, i18n)
    compute_e0(reactive_e0, wpp_ending_year, input, output, i18n)
    compute_mig(reactive_mig, wpp_ending_year, input, output, i18n)

    begin_forecast(
      reactive_pop,
      reactive_tfr,
      reactive_e0,
      reactive_mig,
      wpp_starting_year,
      wpp_ending_year,
      input,
      output,
      simulation_results,
      i18n
    )
  })

  output$pass_tfr <- renderUI({
    modal(
      id = "modal_passtfr",
      content = list(
        style = "font-size: 18px; font-weight: bold; padding: 10px; text-align: center; display: flex; justify-content: center; align-items: center;",
        `data-custom` = "value",
        i18n$t("Select TFR, Life Expectancy and Migration Source")
      ),
      footer = div(
        style = "display: flex; gap: 2px; justify-content: center;",
        div(
          style = "display: flex; gap: 10px;", # 20px gap between buttons
          action_button("change_source_btn", i18n$t("Enter Custom Values"), class = "ui grey button"),
          action_button("pass_source_btn", i18n$t("Use WPP 2024 Median Values"), class = "ui blue button")
        )
      ),
      class = "small"
    )
  })

}

#' Handle all validity checks from the app
#'
#' This function defines all validity checks on inputs to make
#' sure they work correctly
#'
#' @param wpp_starting_year A reactive expression returning the starting year.
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param output Internal parameter for `\{shiny\}`.
#' @param i18n The internationalization object.
#'
#' @importFrom shiny HTML wellPanel renderUI
#' @importFrom shiny.semantic action_button
#' @export
#'
handle_validity_checks <- function(wpp_starting_year, wpp_ending_year, output, i18n = NULL) {
  output$next_pop_page <- renderUI({
    if (wpp_ending_year() < wpp_starting_year()) {
      wellPanel(
        class = "danger",
        i18n$t("\u274C Ending year should be higher than starting year")
      )
    } else {
      action_button("forward_pop_page", i18n$t("Next"), class = "ui blue button")
    }
  })
}

#' Handle all miscellaneous code that doesn't fit in other handle_* steps
#'
#' @param wpp_starting_year A reactive expression returning the starting year.
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param input,output Internal parameter for `\{shiny\}`.
#' @param i18n The internationalization object.
#'
#' @importFrom shiny renderUI
#' @importFrom shiny.fluent TooltipHost Image
#' @export
#'
handle_misc <- function(wpp_starting_year, wpp_ending_year, input, output, i18n = NULL) {
  output$main_analysis_hover <- renderUI({
    TooltipHost(
      content = paste0(
        i18n$t("Analysis for"),
        " ",
        input$wpp_country,
        " ",
        i18n$t("between"),
        " ",
        wpp_starting_year(),
        " ",
        i18n$t("and"),
        " ",
        wpp_ending_year()
      ),
      delay = 0,
      Image(
        src = "www/info.png",
        width = "35px",
        shouldStartVisible = TRUE
      )
    )
  })
}

#' Handle Report Download
#'
#' This function handles the download of a report generated by the application.
#' It provides a placeholder for actual report generation and download functionality.
#'
#' @param simulation_results A reactive expression returning the simulation results.
#' @param wpp_starting_year A reactive expression returning the starting year.
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param input,output Internal parameters for `\{shiny\}`.
#' @param i18n The internationalization object.
#'
#' @importFrom shiny showNotification removeNotification req observeEvent
#' @export
#'
handle_report_download <- function(simulation_results, wpp_starting_year, wpp_ending_year, input, output, i18n = NULL) {
  observeEvent(input$download_report, {
    showNotification(i18n$t("Generating report... Please wait."), type = "message", duration = NULL, id = "report-gen-notification")
    
    req(simulation_results()) # Ensure simulation results are available
    results <- simulation_results()
    
    if (is.null(results)) {
      removeNotification("report-gen-notification")
      showNotification(i18n$t("Please run the projection first to generate a report."), type = "error", duration = 7)
      return()
    }
    
    # Determine current selections for plot generation
    current_pop_year <- if (!is.null(input$pop_age_sex_years)) input$pop_age_sex_years else (wpp_starting_year() + 1)
    current_age_group <- if (!is.null(input$age_pop_time) && length(unique(results$population_by_time$age)) > 0) input$age_pop_time else unique(results$population_by_time$age)[1]
    current_sex <- if (!is.null(input$sex_e0_time)) input$sex_e0_time else "Total"
    current_pop_display <- if (!is.null(input$radio_population_by_broad_age_group)) input$radio_population_by_broad_age_group else "Absolute"
    current_death_birth <- if (!is.null(input$radio_death_births)) input$radio_death_births else "Birth Counts"
    current_dependency <- if (!is.null(input$radio_yadr_oadr)) input$radio_yadr_oadr else "oadr"

    # Generate all plots non-reactively
    plots <- tryCatch({
      create_all_report_plots(
        simulation_results = results,
        country = input$wpp_country,
        start_year = wpp_starting_year(),
        end_year = wpp_ending_year(),
        pop_year = current_pop_year,
        age_group = current_age_group,
        i18n = i18n
      )
    }, error = function(e) {
      removeNotification("report-gen-notification")
      showNotification(paste(i18n$t("Error generating plots for report:"), e$message), type = "error", duration = 10)
      return(NULL)
    })

    if (is.null(plots)) return()

    # Generate the PDF report
    pdf_file_path <- tryCatch({
      generate_demography_report(
        plot_list = plots,
        country = input$wpp_country,
        start_year = wpp_starting_year(),
        end_year = wpp_ending_year(),
        i18n = i18n
      )
    }, error = function(e) {
      removeNotification("report-gen-notification")
      showNotification(paste(i18n$t("Error creating PDF report:"), e$message), type = "error", duration = 10)
      return(NULL)
    })

    removeNotification("report-gen-notification")
    if (!is.null(pdf_file_path) && file.exists(pdf_file_path)) {
      showNotification(i18n$t("Report generated successfully!"), type = "message", duration = 5)
      utils::browseURL(pdf_file_path) # Open the PDF in browser
      print(paste("Report saved to:", pdf_file_path)) # Also print path to console
    } else {
      if(!is.null(pdf_file_path)) { # only show if pdf_file_path was set but file does not exist
          showNotification(i18n$t("Report generation failed. PDF file not found."), type = "error", duration = 7)
      }
    }
  })
}

#' Initialize Population Modal Accordion
#'
#' @description
#' Initializes the accordion component in the population modal with proper
#' event handling and state management.
#'
#' @param modal_id The ID of the modal containing the accordion
#' @param initial_state Whether the accordion should be open initially (default FALSE)
#'
#' @return JavaScript code string to be executed with shinyjs::runjs
#' @export
#'
initialize_population_modal_accordion <- function(modal_id, initial_state = FALSE) {
  js_code <- paste0("
    // Initialize the single accordion
    $('#", modal_id, "_data_config_accordion').accordion({
      exclusive: false,
      animateChildren: false,
      duration: 200,
      active: ", if(initial_state) "0" else "false", ",
      onOpen: function() {
        // Update state and refresh modal
        Shiny.setInputValue('accordion_data_config_open', true);
        $('#", modal_id, "').modal('refresh');
      },
      onClose: function() {
        // Update state and refresh modal
        Shiny.setInputValue('accordion_data_config_open', false);
        $('#", modal_id, "').modal('refresh');
      }
    });
    
    // Initialize popups for info icons
    $('.info.circle.icon').popup({
      hoverable: true,
      position: 'top center',
      delay: {
        show: 300,
        hide: 0
      }
    });
  ")
  
  return(js_code)
}

#' Generate Age Labels
#'
#' @description
#' Generates age labels for either single year ages or 5-year age groups
#' with appropriate open age group formatting.
#'
#' @param age_type Either "Single Ages" or "5-Year Groups"
#' @param oag The open age group value (e.g., 100, 95, 90, etc.)
#'
#' @return Character vector of age labels
#' @export
#'
generate_age_labels <- function(age_type, oag) {
  if (age_type == "Single Ages") {
    # Single year ages: 0, 1, 2, ..., oag+
    ages <- c(0:(oag-1), paste0(oag, "+"))
  } else {
    # 5-year age groups: 0-4, 5-9, ..., oag+
    age_starts <- seq(0, oag-1, by = 5)
    age_labels <- c()
    
    for (start in age_starts) {
      if (start + 4 < oag) {
        age_labels <- c(age_labels, paste0(start, "-", start + 4))
      } else if (start < oag) {
        # Last group before OAG
        age_labels <- c(age_labels, paste0(start, "-", oag - 1))
      }
    }
    ages <- c(age_labels, paste0(oag, "+"))
  }
  return(ages)
}

#' Format 5-Year Age Labels
#'
#' @description
#' Converts numeric ages from WPP data (0, 5, 10...) to standard
#' 5-year age group labels ("0-4", "5-9"...).
#'
#' @param ages Numeric vector of age starts
#'
#' @return Character vector of formatted age labels
#' @export
#'
format_5year_age_labels <- function(ages) {
  # Convert numeric ages (0, 5, 10...) to labels ("0-4", "5-9"...)
  labels <- character(length(ages))
  
  for (i in seq_along(ages)) {
    age <- ages[i]
    if (i == length(ages)) {
      # Last age is OAG
      labels[i] <- paste0(age, "+")
    } else {
      # Regular 5-year group
      labels[i] <- paste0(age, "-", age + 4)
    }
  }
  
  return(labels)
}
