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
handle_before_analysis_plots <- function(reactive_pop, reactive_tfr, reactive_e0, reactive_mig, wpp_starting_year, wpp_ending_year, input, output, i18n = NULL, sim_base_dir = NULL) {
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
              ),
              div(
                style = "margin-top: 10px;",
                actionButton(
                  paste0(modal_id, "_reset_un_btn"),
                  if (!is.null(i18n)) i18n$t("Reset") else "Reset",
                  class = "ui small button",
                  icon = icon("undo")
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
                      selected = "beers(ord)",
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
#' @param input,output,session Internal parameters for `\{shiny\}`.
#' @param i18n The internationalization object.
#'
#' @importFrom shiny renderUI div br showNotification updateSelectInput updateNumericInput
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
    pop_data_source, tfr_starting_year, wpp_starting_year, wpp_ending_year, current_tab, input, output, session, i18n = NULL,
    save_population_files = NULL,
    save_tfr_files = NULL,
    save_e0_files = NULL,
    save_mig_files = NULL,
    modal_raw_pop_data = NULL,
    modal_pop_params = NULL,
    process_population_data = NULL,
    modal_reset_trigger = NULL,
    custom_data_configs = NULL,
    last_active_modal_tab = NULL,
    resetting_simulation = NULL,
    just_restored_data = NULL,
    opening_modal = NULL,
    restored_location = NULL,
    restored_aggregation = NULL,
    restoring_inputs = NULL,
    sim_base_dir = NULL
) {
  output$location_selector <- renderUI({
    # Add explicit dependency on toggle_region to force re-render
    req(input$toggle_region)

    sel <- NULL
    force_mode <- NULL

    # Only use restoration values when actively restoring
    if (!is.null(restoring_inputs) && isTRUE(restoring_inputs())) {
      if (!is.null(restored_location)) {
        sel <- tryCatch({ restored_location() }, error = function(e) NULL)
      }
      if (!is.null(restored_aggregation)) {
        force_mode <- tryCatch({ restored_aggregation() }, error = function(e) NULL)
      }
    }

    location_selector_ui(input, i18n, selected_value = sel, force_mode = force_mode)
  })

  observeEvent(input$customize_pop, {
    show_modal("modal_population")
    current_tab("modal_pop")
    
    # Set tab based on current simulation's data source
    current_data_source <- pop_data_source() %||% "UN Data"
    cat("[MODAL_OPEN_DEBUG] Data source:", current_data_source, ", Setting modal tab to:", current_data_source, "\n")
    
    # Use JavaScript to actually switch the tab after modal opens
    if (current_data_source == "Custom Data") {
      opening_modal(TRUE)  # Set flag before automatic tab switch
      shinyjs::runjs("setTimeout(function() { $('#modal_population_source_custom').click(); }, 100);")
      shinyjs::runjs("setTimeout(function() { Shiny.setInputValue('opening_modal_complete', Math.random()); }, 200);")
      cat("[MODAL_OPEN_DEBUG] Opening Custom Data tab - preserving existing configs\n")
      # Restore Custom widgets from saved params if available; else defaults
      shinyjs::delay(150, {
        params <- tryCatch({ modal_pop_params() }, error = function(e) NULL)
        if (!is.null(params) && identical(params$data_source, "Custom Data")) {
          if (!is.null(params$age_type)) updateSelectInput(session, "modal_population_age_type", selected = params$age_type)
          if (!is.null(params$open_age) && !is.list(params$open_age)) updateNumericInput(session, "modal_population_oag", value = params$open_age)
          if (!is.null(params$interp_method) && !is.list(params$interp_method)) updateSelectInput(session, "modal_population_interp_method", selected = params$interp_method)
          cat("[WIDGET_RESTORE] (Custom) age_type=", params$age_type, ", OAG=", params$open_age, ", method=", params$interp_method, "\n", sep = "")
        } else {
          updateSelectInput(session, "modal_population_age_type", selected = "Single Ages")
          updateNumericInput(session, "modal_population_oag", value = 100)
          updateSelectInput(session, "modal_population_interp_method", selected = "beers(ord)")
          cat("[WIDGET_RESET] (Custom) Defaults applied: Single Ages, OAG 100, beers(ord)\n")
        }
        # Force table re-render after adjusting widgets
        try({ un_data_reset_trigger(un_data_reset_trigger() + 1) }, silent = TRUE)
      })
    } else {
      opening_modal(TRUE)  # Set flag before automatic tab switch
      shinyjs::runjs("setTimeout(function() { $('#modal_population_source_un').click(); }, 100);")
      shinyjs::runjs("setTimeout(function() { Shiny.setInputValue('opening_modal_complete', Math.random()); }, 200);")
      # Clear custom configs aggressively for UN Data simulations
      custom_data_configs(list())
      # Reset Custom Data widgets to defaults
      updateSelectInput(session, "modal_population_age_type", selected = "Single Ages")
      updateNumericInput(session, "modal_population_oag", value = 100)
      updateSelectInput(session, "modal_population_interp_method", selected = "beers(ord)")
      cat("[CONFIG_TRACE] MODAL_OPEN: Cleared all configs for UN Data simulation\n")
      cat("[MODAL_OPEN_DEBUG] Aggressively cleared custom configs and reset widgets for UN Data simulation\n")
    }
    
    previous_modal_tab(current_data_source)
    
    # Fetch both UN data types upfront if not already cached
    ref_year <- extract_reference_year(input$modal_population_ref_date, wpp_starting_year())
    
    # Fetch single age data if not cached (this is now our base for all UN data)
    if (is.null(un_data_single_cache())) {
      data_single <- safe_get_wpp_pop(input$wpp_country, ref_year, 1)
      
      # Ensure standard column order using utility function
      data_single <- ensure_standard_columns(data_single)
      
      un_data_single_cache(data_single)
    }
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

  # Add reactive values for enhanced population modal with separate tab states
  # UN Data tab caches - independent for each age type
  un_data_single_cache <- reactiveVal(NULL)  # Cache for single age UN data edits
  un_data_5yr_cache <- reactiveVal(NULL)     # Cache for 5-year group UN data edits
  
  # Custom Data tab state - stores independent configurations
  # Each configuration is stored by its key: "age_type_oag" (e.g., "Single Ages_100", "5-Year Groups_85")
  # Note: custom_data_configs is now passed as parameter from main server scope
  
  # UI state
  accordion_state <- reactiveVal(list(data_config = FALSE))  # Track accordion state
  previous_modal_tab <- reactiveVal(NULL)  # Track previous tab for auto-saving
  # Note: last_active_modal_tab is now passed as parameter from main server scope
  un_data_reset_trigger <- reactiveVal(0)  # Trigger to force table re-render on reset
  
  # On simulation reset, clear only UN caches and force re-render.
  # app_server handles cross-sim clearing of custom_data_configs to avoid races.
  observeEvent(modal_reset_trigger(), {
    if (!is.null(modal_reset_trigger())) {
      current_configs <- custom_data_configs()
      cat("[MODAL_RESET] (handles) Pre-reset — UN caches will be cleared; configs count:", length(current_configs), "keys:", paste(names(current_configs), collapse=", "), "\n")
      un_data_single_cache(NULL)   # Clear UN data caches
      un_data_5yr_cache(NULL)
      # Force the data table to re-render on next open for both UN and Custom
      un_data_reset_trigger(un_data_reset_trigger() + 1)
      cat("[MODAL_RESET] (handles) UN caches cleared; table re-render scheduled\n")
    }
  }, ignoreInit = TRUE)
  
  
  # State tracking for Custom Data tab (simplified)
  
  # Reset UN data to original when Reset button is clicked
  observeEvent(input$modal_population_reset_un_btn, {
    
    # Extract reference year using utility function
    ref_year <- extract_reference_year(input$modal_population_ref_date, wpp_starting_year())
    
    # Re-fetch original UN data for both age types
    data_single <- safe_get_wpp_pop(input$wpp_country, ref_year, 1)
    data_5yr <- safe_get_wpp_pop(input$wpp_country, ref_year, 5)
    
    # Ensure standard column order using utility function
    data_single <- ensure_standard_columns(data_single)
    data_5yr <- ensure_standard_columns(data_5yr)
    
    # Update both caches with fresh data
    un_data_single_cache(data_single)
    un_data_5yr_cache(data_5yr)
    
    # Trigger table re-render
    un_data_reset_trigger(un_data_reset_trigger() + 1)
    
    # Show notification
    showNotification(i18n$t("UN Data reset to original"), type = "message", duration = 2)
  })
  
  # Clear and re-fetch UN cache when reference date changes (only when modal is open)
  observeEvent(input$modal_population_ref_date, {
    if (!identical(current_tab(), "modal_pop")) return()
    # Extract reference year using utility function
    ref_year <- extract_reference_year(input$modal_population_ref_date, wpp_starting_year())
    
    # Fetch and cache both age types (independent caches)
    data_single <- safe_get_wpp_pop(input$wpp_country, ref_year, 1)
    data_5yr <- safe_get_wpp_pop(input$wpp_country, ref_year, 5)
    
    # Ensure standard column order using utility function
    data_single <- ensure_standard_columns(data_single)
    data_5yr <- ensure_standard_columns(data_5yr)
    
    un_data_single_cache(data_single)
    un_data_5yr_cache(data_5yr)
  })
  
  # Save custom tab configuration changes
  # Track previous UN age type to detect actual changes
  previous_un_age_type <- reactiveVal(NULL)
  
  # Observer for UN Data age type changes - save edits to appropriate cache (NO transformation)
  observeEvent(input$modal_population_un_age_type, {
    # Check if we're on UN Data tab (either explicitly or by default)
    current_source <- input$modal_population_source %||% "UN Data"
    
    # Only process if this is an actual change (not initial load) and we have table data
    if (!is.null(previous_un_age_type()) && 
        previous_un_age_type() != input$modal_population_un_age_type &&
        current_source == "UN Data" && 
        !is.null(input$tmp_pop_dt)) {
      
      # Get current table data directly from rhandsontable
      tryCatch({
        current_data <- rhandsontable::hot_to_r(input$tmp_pop_dt)
        
        # Map column names back to standard using utility function
        current_data <- map_column_names(current_data, i18n)
        
        # Save to the appropriate cache based on PREVIOUS type (what the data currently is)
        previous_type <- previous_un_age_type()
        
        # Save directly to appropriate cache - NO transformation
        if (previous_type == "Single Ages") {
          un_data_single_cache(current_data[, c("age", "popM", "popF")])
        } else {
          un_data_5yr_cache(current_data[, c("age", "popM", "popF")])
        }
      }, error = function(e) {
        # Silently handle errors
      })
    }
    
    # Update the previous value for next comparison
    previous_un_age_type(input$modal_population_un_age_type)
  })

  # Keep table data when changing interpolation method in Custom tab only
  observeEvent(input$modal_population_interp_method, {
    current_source <- input$modal_population_source %||% "UN Data"
    if (current_source != "Custom Data") return()
    if (isTRUE(resetting_simulation()) || isTRUE(just_restored_data())) return()
    if (is.null(input$tmp_pop_dt)) return()

    # Save current grid to the current Custom config (same age type + OAG)
    tryCatch({
      current_data <- rhandsontable::hot_to_r(input$tmp_pop_dt)
      current_data <- map_column_names(current_data, i18n)
      # Ensure expected columns and order
      current_data <- ensure_standard_columns(current_data)

      age_type <- input$modal_population_age_type %||% "Single Ages"
      oag_val  <- input$modal_population_oag %||% 100
      config_key <- get_config_key(age_type, oag_val)

      all_configs <- custom_data_configs()
      all_configs[[config_key]] <- list(
        data = current_data[, c("age", "popM", "popF")],
        interp_method = input$modal_population_interp_method %||% "beers(ord)"
      )
      custom_data_configs(all_configs)
      cat("[CONFIG_TRACE] INTERP_CHANGED: Preserved current grid under key", config_key, "rows:", nrow(current_data), "\n")
    }, error = function(e){
      # No-op on errors; do not disrupt user typing
    })
  })
  
  # Helper function to create configuration key
  get_config_key <- function(age_type, oag) {
    paste0(age_type %||% "Single Ages", "_", oag %||% 100)
  }
  
  # Observer to save custom data when leaving a configuration (manual save only)
  # Note: Auto-save removed to prevent interference with view switching
  # Data is now saved only on explicit actions (Apply button, tab switches)
  
  # Create debounced OAG reactive to avoid intermediate typing errors
  oag_debounced <- reactive({
    input$modal_population_oag
  }) %>% debounce(500)  # Wait 500ms after user stops typing
  
  # OAG validation observer
  observeEvent(oag_debounced(), {
    oag <- oag_debounced()
    
    # Validate OAG is between 35 and 100
    if (!is.null(oag) && !is.na(oag)) {
      if (oag < 35 || oag > 100) {
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
  
  # Re-initialize popups and restore config when switching between data sources
  observeEvent(input$modal_population_source, {
    # Skip if this is an automatic tab switch during modal opening
    if (opening_modal()) {
      cat("[TAB_SWITCH_DEBUG] Skipping automatic tab switch during modal opening\n")
      return()
    }
    
    # Clear the restoration flag when user manually changes tabs
    just_restored_data(FALSE)
    
    # First, save current table data from the tab we're leaving
    if (!is.null(previous_modal_tab()) && !is.null(input$tmp_pop_dt)) {
      tryCatch({
        # Get current table data
        current_data <- rhandsontable::hot_to_r(input$tmp_pop_dt)
        
        # Map translated column names back to standard names using utility function
        current_data <- map_column_names(current_data, i18n)
        
        # Ensure we have the expected columns in the right order
        if (!all(c("age", "popM", "popF") %in% names(current_data))) {
        }
        
        # Save to appropriate cache based on which tab we're leaving
        if (previous_modal_tab() == "UN Data") {
          # Check which UN Data type was selected
          un_age_type <- input$modal_population_un_age_type %||% "Single Ages"
          
          # Transform back to single ages if needed before saving
          if (un_age_type == "5-Year Groups") {
            # Save to 5-year cache
            un_data_5yr_cache(current_data[, c("age", "popM", "popF")])
          } else {
            # Save to single age cache
            un_data_single_cache(current_data[, c("age", "popM", "popF")])
          }
        } else if (previous_modal_tab() == "Custom Data") {
          # Get current configuration parameters
          current_age_type <- input$modal_population_age_type %||% "Single Ages"
          current_oag <- input$modal_population_oag %||% 100
          current_method <- input$modal_population_interp_method %||% "beers(ord)"
          
          # Create configuration key and save directly (no transformation)
          config_key <- get_config_key(current_age_type, current_oag)
          
          # Update configurations with current data (skip during reset and just after restoration)
          if (!isTRUE(resetting_simulation()) && !isTRUE(just_restored_data())) {
            all_configs <- custom_data_configs()
            all_configs[[config_key]] <- list(
              data = current_data[, c("age", "popM", "popF")],
              interp_method = current_method
            )
            custom_data_configs(all_configs)
            cat("[CONFIG_TRACE] TAB_SWITCH: Saved config", config_key, "with", nrow(current_data), "rows\n")
          } else {
            cat("[CONFIG_TRACE] TAB_SWITCH: Skipped saving (resetting:", isTRUE(resetting_simulation()), ", just_restored:", isTRUE(just_restored_data()), ")\n")
          }
        }
      }, error = function(e) {
        # Silently handle any errors during auto-save
        # This prevents disrupting the user experience
      })
    }
    
    # Now handle the tab we're switching TO
    if (!is.null(input$modal_population_source) && 
        input$modal_population_source == "Custom Data") {
      # Custom Data tab now handles multiple configurations
      # The renderRHandsontable will automatically show the right config
      # based on current age_type and OAG inputs
    }
    
    # Update previous tab tracker
    previous_modal_tab(input$modal_population_source)
    
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
    # Add dependency on reset trigger to force re-render
    un_data_reset_trigger()
    
    data_source <- input$modal_population_source %||% "UN Data"
    
    # Extract reference year using utility function
    ref_year <- extract_reference_year(input$modal_population_ref_date, wpp_starting_year())
    
    # Prepare table based on data source
    if (data_source == "Custom Data") {
      
      # Get current configuration parameters
      age_type_input <- input$modal_population_age_type %||% "Single Ages"
      oag_input <- input$modal_population_oag %||% 100
      interp_input <- input$modal_population_interp_method %||% "beers(ord)"
      
      # Validate OAG
      display_oag <- if (!is.null(oag_input) && !is.na(oag_input) && 
                        oag_input >= 35 && oag_input <= 100) {
        oag_input
      } else {
        100  # Default to 100 if invalid
      }
      
      # Create configuration key
      config_key <- paste0(age_type_input, "_", display_oag)
      
      # Get all configurations
      all_configs <- custom_data_configs()
      cat("[CONFIG_TRACE] LOOKUP: Checking configs for", config_key, "- Available:", paste(names(all_configs), collapse=", "), "\n")
      cat("[CUSTOM_DATA_LOOKUP] Looking for config_key:", config_key, ", Available configs:", paste(names(all_configs), collapse=", "), "\n")
      
      # Check if we have data for this configuration
      if (config_key %in% names(all_configs)) {
        # Use stored data for this configuration
        config_data <- all_configs[[config_key]]
        res <- config_data$data
        cat("[CUSTOM_DATA_DEBUG] WIDGETS - Age:", age_type_input, ", OAG:", display_oag, ", Method:", interp_input, "\n")
        cat("[CUSTOM_DATA_DEBUG] DATA - Using stored data, rows:", nrow(res), ", config_key:", config_key, "\n")
        cat("[CUSTOM_TABLE_DEBUG] Actual data being shown in rhandsontable:\n")
        print(head(res, 5))
      } else {
        # No data for this configuration - create empty template
        res <- generate_empty_template(age_type_input, display_oag)
        cat("[CUSTOM_DATA_DEBUG] WIDGETS - Age:", age_type_input, ", OAG:", display_oag, ", Method:", interp_input, "\n")
        cat("[CUSTOM_DATA_DEBUG] DATA - Creating empty template, rows:", nrow(res), ", config_key:", config_key, "\n")
      }
      
    } else {
      # UN Data handling - simplified approach using stored raw data
      un_display_type <- input$modal_population_un_age_type %||% "Single Ages"
      
      # Check if we have restored data that matches current settings
      raw_data <- tryCatch({ modal_raw_pop_data() }, error = function(e) NULL)
      params <- tryCatch({ modal_pop_params() }, error = function(e) NULL)
      
      if (!is.null(raw_data) && !is.null(params) && 
          params$data_source == "UN Data" &&
          params$age_type == un_display_type) {
        # Use restored raw data (it matches what user expects to see)
        res <- raw_data
        cat("[MODAL_RENDER] Using restored UN data:", nrow(raw_data), "x", ncol(raw_data), "\n")
        cat("[MODAL_RENDER] UN data head (restored):\n"); try(print(utils::head(res, 5)), silent = TRUE)
      } else {
        # Fetch fresh UN data with current age type (guard + tryCatch)
        ref_year <- extract_reference_year(input$modal_population_ref_date, wpp_starting_year())
        req(!is.null(input$wpp_country), nzchar(input$wpp_country))
        res <- tryCatch({
          if (un_display_type == "Single Ages") {
            safe_get_wpp_pop(input$wpp_country, ref_year, 1)
          } else {
            safe_get_wpp_pop(input$wpp_country, ref_year, 5)
          }
        }, error = function(e) NULL)
        if (is.null(res)) {
          return(rhandsontable(
            data.frame(
              Age = character(0),
              `Male (in thousands)` = numeric(0),
              `Female (in thousands)` = numeric(0),
              stringsAsFactors = FALSE
            ),
            rowHeaders = NULL,
            readOnly = FALSE,
            stretchH = "all",
            height = 400
          ))
        }
        res <- ensure_standard_columns(res)
        cat("[MODAL_RENDER] Fetched fresh UN data:", nrow(res), "x", ncol(res), "for", un_display_type, "\n")
        cat("[MODAL_RENDER] UN data head (fresh):\n"); try(print(utils::head(res, 5)), silent = TRUE)
      }
    }
    
    # Check if we have valid result data
    if (is.null(res) || nrow(res) == 0) {
      # Return empty table
      return(rhandsontable(
        data.frame(
          Age = character(0),
          Male = numeric(0),
          Female = numeric(0),
          stringsAsFactors = FALSE
        ),
        rowHeaders = NULL,
        readOnly = FALSE,
        stretchH = "all",
        height = 400
      ))
    }
    
    
    # Map standard column names to display names using utility function
    res <- map_column_names_to_display(res, i18n)
    
    # Reorder columns - Age, Male, Female (ensures correct order)
    age_col <- i18n$t("Age")
    male_col <- i18n$t("Male (in thousands)")
    female_col <- i18n$t("Female (in thousands)")
    
    # Only include columns that exist
    col_order <- c()
    if (age_col %in% names(res)) col_order <- c(col_order, age_col)
    if (male_col %in% names(res)) col_order <- c(col_order, male_col)
    if (female_col %in% names(res)) col_order <- c(col_order, female_col)
    
    # Handle both data.frame and data.table
    if (inherits(res, "data.table")) {
      res <- res[, ..col_order]
    } else {
      res <- res[, col_order]
    }
    
    # Create handsontable
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
      # Display population values with one decimal place (e.g., 3,551.9)
      hot_col(c(i18n$t("Female (in thousands)"), i18n$t("Male (in thousands)")), type = "numeric", format = "0,0.0")
  })

  output$tmp_tfr_dt <- renderRHandsontable({
    res <- current_tfr_reactive()
    try({
      cat("[TFR_DEBUG] Customize RAW head (as shown in table):\n");
      print(utils::head(as.data.frame(res), 5))
    }, silent = TRUE)
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
    try({
      cat("[E0_DEBUG] Customize RAW head (as shown in table):\n");
      print(utils::head(as.data.frame(res), 5))
    }, silent = TRUE)
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
    try({
      cat("[MIG_DEBUG] Customize RAW head (as shown in table):\n");
      print(utils::head(as.data.frame(res), 5))
    }, silent = TRUE)
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
  # Observer to clear opening_modal flag after modal opening is complete
  observeEvent(input$opening_modal_complete, {
    opening_modal(FALSE)
    cat("[MODAL_OPEN_DEBUG] Modal opening complete - clearing opening_modal flag\n")
  })

  observeEvent(input$modal_population_ok_btn, {
    # Clear the restoration flag when user clicks Apply
    just_restored_data(FALSE)
    
    req(input$tmp_pop_dt)
    
    
    tryCatch({
      # Get data from table
      data <- rhandsontable::hot_to_r(input$tmp_pop_dt)
      
      # Standardize column names by mapping display names back to standard names
      data <- map_column_names(data, i18n)
      
      
      # Verify columns and ensure correct order using utility function
      data <- ensure_standard_columns(data)
      
      # Extract reference year using utility function
      ref_year <- extract_reference_year(input$modal_population_ref_date, wpp_starting_year())
      
      data_source <- input$modal_population_source %||% "UN Data"
      
      # Create params object to match the reusable transformation function
      params <- list(
        data_source = data_source,
        age_type = if (data_source == "UN Data") {
          input$modal_population_un_age_type %||% "Single Ages"
        } else {
          input$modal_population_age_type %||% "Single Ages"
        },
        open_age = if (data_source == "Custom Data") {
          input$modal_population_oag %||% 100
        } else {
          NULL
        },
        interp_method = if (data_source == "Custom Data") {
          input$modal_population_interp_method %||% "beers(ord)"
        } else {
          NULL
        }
      )
      
      if (data_source == "UN Data") {
        # UN Data mode - save to appropriate cache, then transform for downstream use
        un_age_type <- params$age_type
        
        # Save the current data to the appropriate cache
        if (un_age_type == "Single Ages") {
          un_data_single_cache(data)
        } else {
          un_data_5yr_cache(data)
        }
      } else {
        # Custom Data mode - save current config
        current_age_type <- params$age_type
        current_oag <- params$open_age
        selected_method <- params$interp_method
        
        # Save current data to the specific configuration (skip during reset)
        if (!isTRUE(resetting_simulation())) {
          config_key <- get_config_key(current_age_type, current_oag)
          all_configs <- custom_data_configs()
          all_configs[[config_key]] <- list(
            data = data,
            interp_method = selected_method
          )
          custom_data_configs(all_configs)
          cat("[CONFIG_TRACE] APPLY_BUTTON: Saved config", config_key, "with", nrow(data), "rows\n")
        }
      }
      
      # Persist current parameter state for this simulation so modal reopens with same widgets
      try({ modal_pop_params(params) }, silent = TRUE)
      # Persist raw data for UN display path; harmless for Custom
      try({ modal_raw_pop_data(data) }, silent = TRUE)

      # Use the reusable transformation function
      final_data <- process_population_data(
        data,
        params,
        country = input$wpp_country,
        ref_year = ref_year
      )
      
      # Update reactive value (this is the final data used by the app)
      pop_to_commit_rv(final_data)
      
      # Update data source tracker
      if (!is.null(data_source) && data_source == "Custom Data") {
        pop_data_source("Custom Data")
      } else {
        pop_data_source("UN Data")
      }
      
      # SAVE RAW INPUT DATA with correct parameter context
      tryCatch({
        save_population_files(trigger = "modal_population_ok", raw_data_override = data)
      }, error = function(e) {
        cat("[PHASE4] Error saving raw data:", conditionMessage(e), "\n")
      })
      
      # Save the current tab state before closing
      last_active_modal_tab(input$modal_population_source)
      
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
    
    tryCatch({
      # Get raw data from table
      data <- rhandsontable::hot_to_r(input$tmp_tfr_dt)
      
      # SAVE RAW INPUT DATA with correct parameter context
      tryCatch({
        save_tfr_files(trigger = "modal_tfr_ok", raw_data_override = data)
      }, error = function(e) {
        cat("[PHASE5] Error saving raw TFR data:", conditionMessage(e), "\n")
      })
      
      # Update reactive value (this is the data used by the app)
      tfr_to_commit_rv(data)
      
      # Close modal
      shiny.semantic::hide_modal("modal_tfr")
      
      # Show success message
      showNotification(i18n$t("TFR data updated successfully"), type = "message", duration = 3)
      
    }, error = function(e) {
      # Handle any errors in the modal processing
      showNotification(paste("Error updating TFR data:", conditionMessage(e)), type = "error", duration = 5)
    })
  })

  observeEvent(input$modal_e0_ok_btn, {
    req(input$tmp_e0_dt)
    
    tryCatch({
      # Get raw data from table
      data <- rhandsontable::hot_to_r(input$tmp_e0_dt)
      
      # SAVE RAW INPUT DATA with correct parameter context
      tryCatch({
        save_e0_files(trigger = "modal_e0_ok", raw_data_override = data)
      }, error = function(e) {
        cat("[PHASE6] Error saving raw e0 data:", conditionMessage(e), "\n")
      })
      
      # Update reactive value (this is the data used by the app)
      e0_to_commit_rv(data)
      
      # Close modal
      shiny.semantic::hide_modal("modal_e0")
      
      # Show success message
      showNotification(i18n$t("Life expectancy data updated successfully"), type = "message", duration = 3)
      
    }, error = function(e) {
      # Handle any errors in the modal processing
      showNotification(paste("Error updating e0 data:", conditionMessage(e)), type = "error", duration = 5)
    })
  })

  observeEvent(input$modal_mig_ok_btn, {
    req(input$tmp_mig_dt)
    
    tryCatch({
      # Get raw data from table
      data <- rhandsontable::hot_to_r(input$tmp_mig_dt)
      
      # SAVE RAW INPUT DATA with correct parameter context
      tryCatch({
        save_mig_files(trigger = "modal_mig_ok", raw_data_override = data)
      }, error = function(e) {
        cat("[PHASE6] Error saving raw migration data:", conditionMessage(e), "\n")
      })
      
      # Update reactive value (this is the data used by the app)
      mig_to_commit_rv(data)
      
      # Close modal
      shiny.semantic::hide_modal("modal_mig")
      
      # Show success message
      showNotification(i18n$t("Migration data updated successfully"), type = "message", duration = 3)
      
    }, error = function(e) {
      # Handle any errors in the modal processing
      showNotification(paste("Error updating migration data:", conditionMessage(e)), type = "error", duration = 5)
    })
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
handle_navigation <- function(
  simulation_results,
  reactive_pop,
  reactive_tfr,
  reactive_e0,
  reactive_mig,
  pop_data_source,
  wpp_starting_year,
  wpp_ending_year,
  current_tab,
  input_next_completed,
  input,
  output,
  i18n = NULL,
  # Optional hooks to rehydrate population transforms on sidebar nav
  pop_to_commit_rv = NULL,
  modal_raw_pop_data = NULL,
  modal_pop_params = NULL,
  process_population_data = NULL,
  # Optional hooks to rehydrate other indicators
  tfr_to_commit_rv = NULL,
  e0_to_commit_rv = NULL,
  mig_to_commit_rv = NULL,
  restoring_inputs = NULL,
  # Save functions
  save_population_files = NULL
) {

  processing <- reactiveVal(TRUE)
  show_tfr_modal <- reactiveVal(TRUE)

  observeEvent(input$start_analysis, {
    hide("landing_page")
    show("input_page")
    show("left_menu")
    current_tab("input_page")
  })

  # Sidebar navigation
  observeEvent(input$nav_input, {
    hide("pop_page"); hide("tfr_page"); hide("e0_page"); hide("mig_page"); hide("forecast_page")
    show("input_page")
    current_tab("input_page")
    # Log gating state when arriving via sidebar nav
    cat("[GATE_DEBUG] nav_input | sim:", input$sim_switcher,
        "| next_clicked:", isTRUE(input_next_completed()), "\n")
  })

  observeEvent(input$nav_pop, {
    cat("[GATE_DEBUG] nav_pop | sim:", input$sim_switcher,
        "| next_clicked:", isTRUE(input_next_completed()), "\n")
    if (!isTRUE(input_next_completed())) {
      showNotification(i18n$translate("Please first select an input and click Next"), type = "warning", duration = 4)
      return()
    }
    hide("input_page"); hide("tfr_page"); hide("e0_page"); hide("mig_page"); hide("forecast_page")
    show("pop_page")
    current_tab("pop_page")
    # Log pre-transform (Customize/raw) and post-transform (Plot) data heads
    try({
      raw_dt <- tryCatch({ modal_raw_pop_data() }, error = function(e) NULL)
      if (!is.null(raw_dt)) {
        cat("[PYRAMID_DEBUG] [Sidebar] Customize RAW data head (as saved/displayed):\n");
        try(print(utils::head(as.data.frame(raw_dt), 5)), silent = TRUE)
      } else {
        cat("[PYRAMID_DEBUG] [Sidebar] Customize RAW data is NULL\n")
      }
      plot_dt <- tryCatch({ reactive_pop() }, error = function(e) NULL)
      if (!is.null(plot_dt)) {
        cat("[PYRAMID_DEBUG] [Sidebar] PLOT data head (after transform):\n");
        try(print(utils::head(as.data.frame(plot_dt), 5)), silent = TRUE)
      } else {
        cat("[PYRAMID_DEBUG] [Sidebar] PLOT data is NULL\n")
      }
    }, silent = TRUE)
    # If the committed pop data was cleared during sim switching, re-derive it
    try({
      if (!is.null(pop_to_commit_rv) && is.null(pop_to_commit_rv()) &&
          !is.null(modal_raw_pop_data) && !is.null(modal_raw_pop_data()) &&
          !is.null(modal_pop_params) && !is.null(modal_pop_params()) &&
          !is.null(process_population_data)) {
        params <- modal_pop_params()
        data <- modal_raw_pop_data()
        final_data <- process_population_data(
          raw_data = data,
          params = params,
          country = input$wpp_country,
          ref_year = wpp_starting_year()
        )
        pop_to_commit_rv(final_data)
        # Keep data source indicator consistent
        if (!is.null(params$data_source)) pop_data_source(params$data_source)
      }
    }, silent = TRUE)
    # Render same content as the Next flow to ensure fresh, per-sim results
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

  observeEvent(input$nav_tfr, {
    cat("[GATE_DEBUG] nav_tfr | sim:", input$sim_switcher,
        "| next_clicked:", isTRUE(input_next_completed()), "\n")
    if (!isTRUE(input_next_completed())) {
      showNotification(i18n$translate("Please first select an input and click Next"), type = "warning", duration = 4)
      return()
    }
    hide("input_page"); hide("pop_page"); hide("e0_page"); hide("mig_page"); hide("forecast_page")
    show("tfr_page")
    current_tab("tfr_page")
    
    # Rehydrate TFR if missing: load from disk for this sim
    try({
      if (!is.null(tfr_to_commit_rv) && is.null(tfr_to_commit_rv())) {
        sim_dir <- file.path(sim_base_dir, input$sim_switcher, "inputs")
        tfr_path <- file.path(sim_dir, "tfr.csv")
        if (file.exists(tfr_path)) {
          dt <- data.table::fread(tfr_path)
          if (nrow(dt) > 0) {
            names(dt) <- c("year", "tfr")
            suppressWarnings({ dt$year <- as.numeric(dt$year) })
            suppressWarnings({ dt$tfr  <- as.numeric(dt$tfr) })
            tfr_to_commit_rv(as.data.frame(dt))
          }
        }
      }
    }, silent = TRUE)
    if (isTRUE(show_tfr_modal())) {
      show_modal("modal_passtfr")
    } else {
      show_tfr(reactive_tfr, wpp_ending_year, input, output, i18n)
    }
    show_tfr_modal(FALSE)
  })

  observeEvent(input$change_source_btn, {
    show_tfr(reactive_tfr, wpp_ending_year, input, output, i18n)
  })

  observeEvent(input$nav_e0, {
    cat("[GATE_DEBUG] nav_e0 | sim:", input$sim_switcher,
        "| next_clicked:", isTRUE(input_next_completed()), "\n")
    if (!isTRUE(input_next_completed())) {
      showNotification(i18n$translate("Please first select an input and click Next"), type = "warning", duration = 4)
      return()
    }
    hide("input_page"); hide("pop_page"); hide("tfr_page"); hide("mig_page"); hide("forecast_page")
    show("e0_page")
    current_tab("e0_page")
    hide_modal("modal_passtfr")
    # Rehydrate e0 if missing
    try({
      if (!is.null(e0_to_commit_rv) && is.null(e0_to_commit_rv())) {
        sim_dir <- file.path(sim_base_dir, input$sim_switcher, "inputs")
        e0_path <- file.path(sim_dir, "e0.csv")
        if (file.exists(e0_path)) {
          dt <- data.table::fread(e0_path)
          if (nrow(dt) > 0) {
            names(dt) <- c("year", "e0M", "e0F")
            suppressWarnings({ dt$year <- as.numeric(dt$year) })
            suppressWarnings({ dt$e0M <- as.numeric(dt$e0M) })
            suppressWarnings({ dt$e0F <- as.numeric(dt$e0F) })
            e0_to_commit_rv(as.data.frame(dt))
          }
        }
      }
    }, silent = TRUE)
    show_e0(reactive_e0, wpp_ending_year, input, output, i18n)
  })

  observeEvent(input$nav_mig, {
    cat("[GATE_DEBUG] nav_mig | sim:", input$sim_switcher,
        "| next_clicked:", isTRUE(input_next_completed()), "\n")
    if (!isTRUE(input_next_completed())) {
      showNotification(i18n$translate("Please first select an input and click Next"), type = "warning", duration = 4)
      return()
    }
    hide("input_page"); hide("pop_page"); hide("tfr_page"); hide("e0_page"); hide("forecast_page")
    show("mig_page")
    current_tab("mig_page")
    if (!is.null(restoring_inputs) && isTRUE(restoring_inputs())) {
      cat("[RESTORE_GUARD] Delaying migration compute until restore completes\n")
      local_obs <- NULL
      local_obs <- observeEvent(restoring_inputs(), {
        if (!isTRUE(restoring_inputs())) {
          try({
            if (!is.null(mig_to_commit_rv) && is.null(mig_to_commit_rv())) {
              sim_dir <- file.path(sim_base_dir, input$sim_switcher, "inputs")
              mig_path <- file.path(sim_dir, "mig.csv")
              if (file.exists(mig_path)) { dt <- data.table::fread(mig_path); if (nrow(dt)>0){ names(dt)<-c("year","mig"); suppressWarnings({dt$year<-as.numeric(dt$year)}); suppressWarnings({dt$mig<-as.numeric(dt$mig)}); mig_to_commit_rv(as.data.frame(dt)) } }
            }
          }, silent = TRUE)
          show_mig(reactive_mig, wpp_ending_year, input, output, i18n)
          local_obs$destroy()
        }
      }, ignoreInit = FALSE)
      return()
    }
    # Rehydrate migration if missing
    try({
      if (!is.null(mig_to_commit_rv) && is.null(mig_to_commit_rv())) {
        sim_dir <- file.path(sim_base_dir, input$sim_switcher, "inputs")
        mig_path <- file.path(sim_dir, "mig.csv")
        if (file.exists(mig_path)) {
          dt <- data.table::fread(mig_path)
          if (nrow(dt) > 0) {
            names(dt) <- c("year", "mig")
            suppressWarnings({ dt$year <- as.numeric(dt$year) })
            suppressWarnings({ dt$mig  <- as.numeric(dt$mig) })
            mig_to_commit_rv(as.data.frame(dt))
          }
        }
      }
    }, silent = TRUE)
    curr_sig <- paste0(input$toggle_region, "|", input$wpp_country, "|", input$wpp_starting_year, "|", input$wpp_ending_year)
    cat("[PAGE_COMPUTE_DEBUG] page=mig sim=", input$sim_switcher, " curr_sig=", curr_sig, "\n", sep = "")
    show_mig(reactive_mig, wpp_ending_year, input, output, i18n)
  })

  observeEvent(input$forward_pop_page, {
    hide("input_page")
    show("pop_page")
    current_tab("pop_page")

    # If restore is in progress, delay rendering until complete
    if (!is.null(restoring_inputs) && isTRUE(restoring_inputs())) {
      cat("[RESTORE_GUARD] Delaying Population compute (Next) until restore completes\n")
      local_obs <- NULL
      local_obs <- observeEvent(restoring_inputs(), {
        if (!isTRUE(restoring_inputs())) {
          try({
            raw_dt <- tryCatch({ modal_raw_pop_data() }, error = function(e) NULL)
            if (!is.null(raw_dt)) { cat("[PYRAMID_DEBUG] [Next] Customize RAW data head (as saved/displayed):\n"); try(print(utils::head(as.data.frame(raw_dt), 5)), silent = TRUE) }
            plot_dt <- tryCatch({ reactive_pop() }, error = function(e) NULL)
            if (!is.null(plot_dt)) { cat("[PYRAMID_DEBUG] [Next] PLOT data head (after transform):\n"); try(print(utils::head(as.data.frame(plot_dt), 5)), silent = TRUE) }
          }, silent = TRUE)
          output$show_pop_results_ui <- renderUI({
            create_pop_pyramid_plot(reactive_pop(), country = input$wpp_country, input_year = wpp_starting_year(), i18n = i18n)
            # Save files now that data is loaded
            if (!is.null(save_population_files)) {
              try({ save_population_files(trigger = "pop_page_loaded") }, silent = TRUE)
            }
            res <- show_pop_results_ui(data_source = pop_data_source(), i18n = i18n)
            processing(FALSE); res
          })
          local_obs$destroy()
        }
      }, ignoreInit = FALSE)
      return()
    }

    # Log pre-transform (Customize/raw) and post-transform (Plot) data heads
    try({
      raw_dt <- tryCatch({ modal_raw_pop_data() }, error = function(e) NULL)
      if (!is.null(raw_dt)) {
        cat("[PYRAMID_DEBUG] [Next] Customize RAW data head (as saved/displayed):\n");
        try(print(utils::head(as.data.frame(raw_dt), 5)), silent = TRUE)
      } else {
        cat("[PYRAMID_DEBUG] [Next] Customize RAW data is NULL\n")
      }
      plot_dt <- tryCatch({ reactive_pop() }, error = function(e) NULL)
      if (!is.null(plot_dt)) {
        cat("[PYRAMID_DEBUG] [Next] PLOT data head (after transform):\n");
        try(print(utils::head(as.data.frame(plot_dt), 5)), silent = TRUE)
      } else {
        cat("[PYRAMID_DEBUG] [Next] PLOT data is NULL\n")
      }
    }, silent = TRUE)

    output$show_pop_results_ui <- renderUI({
      curr_sig <- paste0(input$toggle_region, "|", input$wpp_country, "|", input$wpp_starting_year, "|", input$wpp_ending_year)
      cat("[PAGE_COMPUTE_DEBUG] page=population (Next) sim=", input$sim_switcher, " curr_sig=", curr_sig, "\n", sep = "")
      eff_country <- tryCatch({ if (!is.null(restoring_inputs) && isTRUE(restoring_inputs())) restored_location() else input$wpp_country }, error=function(e) input$wpp_country)
      create_pop_pyramid_plot(
        reactive_pop(),
        country = eff_country,
        input_year = wpp_starting_year(),
        i18n = i18n
      )

      # Save population files now that data is loaded
      if (!is.null(save_population_files)) {
        try({
          save_population_files(trigger = "pop_page_loaded")
        }, silent = TRUE)
      }

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

    my_sim <- simulations$current
    cat("[PASS_SOURCE_BTN] User clicked WPP shortcut for sim:", my_sim, "\n")

    # Use begin_forecast directly with compute mode
    # Since this handler might not have access to setup_forecast_ui
    begin_forecast(
      reactive_pop = reactive_pop,
      reactive_tfr = reactive_tfr,
      reactive_e0 = reactive_e0,
      reactive_mig = reactive_mig,
      wpp_starting_year = wpp_starting_year,
      wpp_ending_year = wpp_ending_year,
      input = input,
      output = output,
      simulation_results = simulation_results,
      i18n = i18n,
      results_dir = file.path(sim_base_dir, my_sim, "results"),
      force = TRUE,  # Force compute
      is_active = reactive({ current_tab() == "forecast_page" }),
      sim_name = my_sim,
      is_current_sim = reactive({ simulations$current == my_sim }),
      prefer_saved = FALSE,
      allow_compute = reactive(TRUE)
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
handle_validity_checks <- function(wpp_starting_year, wpp_ending_year, input, output, i18n = NULL, has_simulation = NULL) {
  output$next_pop_page <- renderUI({
    # Year guard
    if (wpp_ending_year() < wpp_starting_year()) {
      return(wellPanel(
        class = "danger",
        i18n$t("Ending year should be higher than starting year")
      ))
    }

    # Simulation guard (Phase 2+)
    if (!is.null(has_simulation)) {
      has_sim <- tryCatch({ has_simulation() }, error = function(e) FALSE)
      if (!isTRUE(has_sim)) {
        return(wellPanel(
          class = "danger",
          i18n$t("Please add and select a simulation first (use '+ Add a new simulation')")
        ))
      }
    }

    # OK to proceed
    action_button("forward_pop_page", i18n$t("Next"), class = "ui blue button")
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
      # Report saved to console
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
