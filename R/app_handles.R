#' Create Landing Page UI
#'
#' @description
#' Creates a modern, informative landing page that introduces users to the app's functionality
#'
#' @importFrom shiny div h1 h2 p HTML
#' @importFrom shiny.semantic grid grid_template segment
#' @importFrom shiny.semantic icon
#' @export
create_landing_page <- function() {
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
        h1("Population Projection Tool", class = "hero-title"),
        p("Create detailed population projections using United Nations World Population Prospects data or your own custom inputs",
          class = "hero-subtitle"
        )
      ),

      # Feature 1: Population Data
      feature1 = segment(
        class = "ui raised segment feature-card",
        div(
          class = "feature-content",
          div(class = "feature-icon", icon("users")),
          h2(class = "feature-title", "Population Data"),
          p("Upload or use UN WPP data for:"),
          HTML("
            <ul>
              <li>Base population by age and sex</li>
              <li>Total fertility rates</li>
              <li>Life expectancy</li>
              <li>Migration patterns</li>
            </ul>
          ")
        )
      ),

      # Feature 2: Interactive Visualization
      feature2 = segment(
        class = "ui raised segment feature-card",
        div(
          class = "feature-content",
          div(class = "feature-icon", icon("chart bar")),
          h2(class = "feature-title", "Interactive Visualization"),
          p("Explore your data through:"),
          HTML("
            <ul>
              <li>Population pyramids</li>
              <li>Time series charts</li>
              <li>Customizable projections</li>
              <li>Downloadable results</li>
            </ul>
          ")
        )
      ),

      # Feature 3: Data Requirements
      feature3 = segment(
        class = "ui raised segment feature-card",
        div(
          class = "feature-content",
          div(class = "feature-icon", icon("exchange")),
          h2(class = "feature-title", "Scenario Comparison"),
          p("Compare your projections with UN WPP:"),
          HTML("
      <ul>
        <li>Side-by-side visualization</li>
        <li>Compare with UN median variant</li>
        <li>Validate your assumptions</li>
        <li>Export comparison results</li>
      </ul>
    ")
        )
      ),

      # CTA Section
      cta = div(
        class = "cta-section",
        action_button("start_analysis", "Start Your Analysis",
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
#'
#' @importFrom DT renderDataTable
#' @importFrom plotly ggplotly renderPlotly
#' @export
#'
handle_before_analysis_plots <- function(reactive_pop, reactive_tfr, reactive_e0, reactive_mig, wpp_starting_year, wpp_ending_year, input, output) {
  # Render plots for population pyramid and total fertility rate
  output$plot_pop <- renderPlotly(
    create_pop_pyramid_plot(
      reactive_pop(),
      country = input$wpp_country,
      input_year = wpp_starting_year()
    )$plotly
  )

  output$plot_tfr_custom <- renderPlotly(
    create_tfr_plot(
      reactive_tfr(),
      end_year = wpp_ending_year(),
      country = input$wpp_country
    )$plotly
  )

  output$plot_e0_custom <- renderPlotly(
    create_e0_plot(
      reactive_e0(),
      end_year = wpp_ending_year(),
      country = input$wpp_country
    )$plotly
  )

  output$plot_mig_custom <- renderPlotly(
    create_mig_plot(
      reactive_mig(),
      end_year = wpp_ending_year(),
      country = input$wpp_country
    )$plotly
  )


  # Render population table
  output$table_pop <- renderDT(prepare_pop_agegroups_table(reactive_pop()))
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
#'
#' @return A modal UI element.
#'
#' @importFrom DT DTOutput
#' @importFrom shiny.semantic fileInput modal
#' @export
#'
create_modal_ui <- function(modal_id, header_title, output_id, file_input_id, download_button_id, hide_button_id, additional_header = NULL) {
  modal(
    id = modal_id,
    header = div(
      div(
        style = "display: flex; justify-content: space-between;",
        header_title,
        action_button("customize_help", "Instructions", class = "ui red button")
      ),
      additional_header,
    ),
    DTOutput(output_id),
    footer = div(
      div(
        class = "footer-container",
        div(
          class = "file-input-container",
          div(
            style = "display: flex; align-items: center; gap: 5px;",
            shiny.semantic::file_input(file_input_id, label = NULL, placeholder = "Upload CSV file", width = "100%"),
          )
        ),
        div(
          class = "button-container",
          div(
            style = "display: flex; gap: 5px",
            shiny::downloadButton(download_button_id, "Download", class = "ui blue button")
          )
        ),
      ),
      div("Uploaded data must match exactly the format, column names and ordering shown in the table above", style = "color: #8B0000; font-weight: bold; font-size: 12px;")
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
#' @param reactive_pop A reactive expression that returns the population data to be displayed.
#' @param reactive_tfr A reactive expression that returns the TFR data to be displayed.
#' @param reactive_e0 A reactive expression that returns the e0 data to be displayed.
#' @param reactive_mig A reactive expression that returns the migration data to be displayed.
#' @param tfr_starting_year A reactive expression returning the TFR starting year.
#' @param wpp_starting_year A reactive expression returning the starting year.
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param input,output Internal parameters for `{shiny}`.
#'
#' @importFrom shiny renderUI div br
#' @importFrom shiny.semantic fileInput action_button modal
#' @importFrom DT datatable renderDT DTOutput
#'
#' @return None
#' @export
#'
handle_customize_data <- function(reactive_pop, reactive_tfr, reactive_e0, reactive_mig, tfr_starting_year, wpp_starting_year, wpp_ending_year, current_tab, input, output) {
  output$location_selector <- renderUI(location_selector_ui(input))

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

  dt_options <- list(
    paging = FALSE,
    searching = FALSE,
    lengthChange = FALSE,
    scrollY = "400px", # Enable vertical scrolling, adjust "400px" as needed
    dom = "lfrtp"
  )

  output$tmp_pop_dt <- renderDT({
    res <- reactive_pop()
    names(res) <- c("Age", "Female (in thousands)", "Male (in thousands)")
    datatable(
      res,
      options = dt_options
    )
  })

  output$tmp_tfr_dt <- renderDT({
    res <- reactive_tfr()
    names(res) <- c("Year", "TFR")
    datatable(
      res,
      options = dt_options
    )
  })

  output$tmp_e0_dt <- renderDT({
    res <- reactive_e0()
    names(res) <- c("Year", "Males", "Females")
    datatable(
      res,
      options = dt_options
    )
  })

  output$tmp_mig_dt <- renderDT({
    res <- reactive_mig()
    names(res) <- c("Year", "Migration")
    datatable(
      res,
      options = dt_options
    )
  })


  output$popup_pop <- renderUI({
    create_modal_ui(
      modal_id = "modal_population",
      header_title = paste0("Population data for ", input$wpp_country, " in base year"),
      output_id = "tmp_pop_dt",
      file_input_id = "upload_pop",
      download_button_id = "download_pop",
      hide_button_id = "hide_pop",
      additional_header = create_header_content("Population units are in thousands. Any edits need to be added in thousands", additional_style = "font-weight: normal; font-size: smaller;")
    )
  })


  output$popup_tfr <- renderUI({
    create_modal_ui(
      modal_id = "modal_tfr",
      header_title = paste0("Total Fertility Rate for ", input$wpp_country, " in analysis period."),
      output_id = "tmp_tfr_dt",
      file_input_id = "upload_tfr",
      download_button_id = "download_tfr",
      hide_button_id = "hide_tfr"
    )
  })


  output$popup_e0 <- renderUI({
    create_modal_ui(
      modal_id = "modal_e0",
      header_title = paste0("Life Expectancy for ", input$wpp_country, " in analysis period."),
      output_id = "tmp_e0_dt",
      file_input_id = "upload_e0",
      download_button_id = "download_e0",
      hide_button_id = "hide_e0"
    )
  })

  output$popup_mig <- renderUI({
    create_modal_ui(
      modal_id = "modal_mig",
      header_title = paste0("Migration data for ", input$wpp_country, " in analysis period."),
      output_id = "tmp_mig_dt",
      file_input_id = "upload_mig",
      download_button_id = "download_mig",
      hide_button_id = "hide_mig"
    )
  })


  data_uploaded_pop <- reactiveVal(NULL)
  data_uploaded_tfr <- reactiveVal(NULL)
  data_uploaded_e0 <- reactiveVal(NULL)
  data_uploaded_mig <- reactiveVal(NULL)


  observeEvent(input$upload_pop, {
    req(input$upload_pop)
    data_uploaded_pop(TRUE)
    reactive_pop()
  })

  observeEvent(input$upload_tfr, {
    req(input$upload_tfr)
    data_uploaded_tfr(TRUE)
    reactive_tfr()
  })

  observeEvent(input$upload_e0, {
    req(input$upload_e0)
    data_uploaded_e0(TRUE)
    reactive_e0()
  })

  observeEvent(input$upload_mig, {
    req(input$upload_mig)
    data_uploaded_mig(TRUE)
    reactive_mig()
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
    content = function(file) write.csv(reactive_pop(), file, row.names = FALSE)
  )

  output$download_tfr <- shiny::downloadHandler(
    filename = function() paste0("tfr_", tfr_years(), ".csv"),
    content = function(file) write.csv(reactive_tfr(), file, row.names = FALSE)
  )

  output$download_e0 <- shiny::downloadHandler(
    filename = function() paste0("e0_", tfr_years(), ".csv"),
    content = function(file) write.csv(reactive_e0(), file, row.names = FALSE)
  )

  output$download_mig <- shiny::downloadHandler(
    filename = function() paste0("mig_", tfr_years(), ".csv"),
    content = function(file) write.csv(reactive_mig(), file, row.names = FALSE)
  )
}

#' Handle Navigation Between Steps
#'
#' This function manages the navigation between different pages of the application.
#'
#' @param reactive_pop A reactive expression that returns the population data to be displayed.
#' @param reactive_tfr A reactive expression that returns the TFR data to be displayed.
#' @param reactive_e0 A reactive expression that returns the e0 data to be displayed.
#' @param reactive_mig A reactive expression that returns the migration data to be displayed.
#' @param wpp_starting_year A reactive expression returning the starting year.
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param input,output Internal parameters for `{shiny}`.
#'
#' @importFrom shinyjs hide show
#' @importFrom shiny renderUI HTML
#' @importFrom shiny.semantic show_modal hide_modal
#' @importFrom shinyalert shinyalert
#' @importFrom utils write.csv
#' @export
#'
handle_navigation <- function(reactive_pop, reactive_tfr, reactive_e0, reactive_mig, wpp_starting_year, wpp_ending_year, current_tab, input, output) {
  processing <- reactiveVal(TRUE)
  show_tfr_modal <- reactiveVal(TRUE)
  simulation_results <- reactiveVal()

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
        input_year = wpp_starting_year()
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
          title = "Explore UN Estimates \xF0\x9F\x8C\x90",
          text = tags$div(
            style = "text-align: left;",
            HTML("\xF0\x9F\x94\xA2 The data shown here are estimates from the United Nations<br/>
              \xF0\x9F\x94\x84 Click 'Customize' to enter your own data<br/>
              \xF0\x9F\xA7\xAE Upload: starting population by sex and single year of age with an open age interval of 100+")
          ),
          type = "info",
          html = TRUE,
          closeOnEsc = TRUE,
          showConfirmButton = TRUE,
          confirmButtonText = "Got it!"
        )
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(input$forward_tfr_page, {
    if (show_tfr_modal()) {
      show_modal("modal_passtfr")
    } else {
      show_tfr(reactive_tfr, wpp_ending_year, input, output)
    }

    show_tfr_modal(FALSE)
  })

  observeEvent(input$change_source_btn, {
    show_tfr(reactive_tfr, wpp_ending_year, input, output)
  })

  observeEvent(input$forward_e0_page, {
    hide_modal("modal_passtfr")
    hide("tfr_page")
    show("e0_page")
    show_e0(reactive_e0, wpp_ending_year, input, output)
  })


  observeEvent(input$forward_mig_page, {
    hide("e0_page")
    show("mig_page")
    show_mig(reactive_mig, wpp_ending_year, input, output)
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
    compute_tfr(reactive_tfr, wpp_ending_year, input, output)
    compute_e0(reactive_e0, wpp_ending_year, input, output)
    compute_mig(reactive_mig, wpp_ending_year, input, output)

    begin_forecast(
      reactive_pop,
      reactive_tfr,
      reactive_e0,
      reactive_mig,
      wpp_starting_year,
      wpp_ending_year,
      input,
      output,
      simulation_results
    )
  })

  output$pass_tfr <- renderUI({
    modal(
      id = "modal_passtfr",
      content = list(
        style = "font-size: 18px; font-weight: bold; padding: 10px; text-align: center; display: flex; justify-content: center; align-items: center;",
        `data-custom` = "value",
        HTML("Select TFR, Life Expectancy and Migration Source")
      ),
      footer = div(
        style = "display: flex; gap: 2px; justify-content: center;",
        div(
          style = "display: flex; gap: 10px;", # 20px gap between buttons
          action_button("change_source_btn", "Enter Custom Values", class = "ui grey button"),
          action_button("pass_source_btn", "Use WPP 2024 Median Values", class = "ui blue button")
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
#'
#' @importFrom shiny HTML wellPanel renderUI
#' @importFrom shiny.semantic action_button
#' @export
#'
handle_validity_checks <- function(wpp_starting_year, wpp_ending_year, output) {
  output$next_pop_page <- renderUI({
    if (wpp_ending_year() < wpp_starting_year()) {
      wellPanel(
        class = "danger",
        HTML("\u274C Ending year should be higher than starting year")
      )
    } else {
      action_button("forward_pop_page", "Next", class = "ui blue button")
    }
  })
}

#' Handle all miscellaneous code that doesn't fit in other handle_* steps
#'
#' @param wpp_starting_year A reactive expression returning the starting year.
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param input,output Internal parameter for `{shiny}`.
#'
#' @importFrom shiny renderUI
#' @importFrom shiny.fluent TooltipHost Image
#' @export
#'
handle_misc <- function(wpp_starting_year, wpp_ending_year, input, output) {
  output$main_analysis_hover <- renderUI({
    TooltipHost(
      content = paste0(
        "Analysis for ",
        input$wpp_country,
        " between ",
        wpp_starting_year(),
        " and ",
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
