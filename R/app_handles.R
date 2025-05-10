#' Create Landing Page UI
#'
#' @description
#' Creates a modern, informative landing page that introduces users to the app's functionality
#'
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
#' @param input,output Internal parameters for `{shiny}.
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
#' @param input,output Internal parameters for `{shiny}`.
#' @param i18n The internationalization object.
#'
#' @importFrom shiny renderUI div br
#' @importFrom shiny.semantic fileInput action_button modal
#' @importFrom rhandsontable renderRHandsontable rhandsontable rHandsontableOutput
#'
#' @return None
#' @export
#'
handle_customize_data <- function(
    current_pop_reactive, current_tfr_reactive, current_e0_reactive, current_mig_reactive,
    pop_to_commit_rv, tfr_to_commit_rv, e0_to_commit_rv, mig_to_commit_rv,
    tfr_starting_year, wpp_starting_year, wpp_ending_year, current_tab, input, output, i18n = NULL
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

  output$tmp_pop_dt <- renderRHandsontable({
    res <- current_pop_reactive()
    names(res) <- c(
      i18n$t("Age"), 
      i18n$t("Female (in thousands)"), 
      i18n$t("Male (in thousands)")
    )
    rhandsontable(
      res,
      useTypes = TRUE,
      stretchH = "all",
      height = 400
    )
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
    create_modal_ui(
      modal_id = "modal_population",
      header_title = paste0(i18n$t("Population data for"), " ", input$wpp_country, " ", i18n$t("in base year")),
      output_id = "tmp_pop_dt",
      file_input_id = "upload_pop",
      download_button_id = "download_pop",
      hide_button_id = "hide_pop",
      additional_header = create_header_content(i18n$t("Population units are in thousands. Any edits need to be added in thousands"), additional_style = "font-weight: normal; font-size: smaller;"),
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
    current_data_from_table <- rhandsontable::hot_to_r(input$tmp_pop_dt)
    pop_to_commit_rv(current_data_from_table)
    shiny.semantic::hide_modal("modal_population")
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
    content = function(file) write.csv(current_pop_reactive(), file, row.names = FALSE)
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
#' @param input,output Internal parameters for `{shiny}`.
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
handle_navigation <- function(simulation_results, reactive_pop, reactive_tfr, reactive_e0, reactive_mig, wpp_starting_year, wpp_ending_year, current_tab, input, output, i18n = NULL) {

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

      res <- show_pop_results_ui()
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
#' @param output Internal parameter for `{shiny}`.
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
#' @param input,output Internal parameter for `{shiny}`.
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
#' @param input,output Internal parameters for `{shiny}`.
#' @param i18n The internationalization object.
#'
#' @importFrom shiny showNotification removeNotification req observeEvent
#' @export
#'
handle_report_download <- function(simulation_results, wpp_starting_year, wpp_ending_year, input, output, i18n = NULL) {
  # Get the forecast results and ensure they exist
  req(simulation_results())
  results <- simulation_results()
  
  # Try to determine available age groups and selected values
  # Get available age groups
  age_groups <- unique(results$population_by_time$age)
  selected_age <- if(!is.null(input$age_pop_time)) input$age_pop_time else age_groups[1]

  # Create all plots using the non-reactive function
  plots <- create_all_report_plots(
    simulation_results = results,
    country = input$wpp_country,
    start_year = wpp_starting_year(),
    end_year = wpp_ending_year(),
    pop_year = if(!is.null(input$pop_age_sex_years)) input$pop_age_sex_years else wpp_starting_year() + 1,
    age_group = selected_age,
    sex = if(!is.null(input$sex_e0_time)) input$sex_e0_time else "Total",
    pop_display_type = if(!is.null(input$radio_population_by_broad_age_group)) input$radio_population_by_broad_age_group else "Absolute",
    death_birth_type = if(!is.null(input$radio_death_births)) input$radio_death_births else "Birth Counts",
    dependency_type = if(!is.null(input$radio_yadr_oadr)) input$radio_yadr_oadr else "oadr",
    i18n = i18n
  )

  browser()
}
