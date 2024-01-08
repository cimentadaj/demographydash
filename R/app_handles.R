#' Render Population and TFR Plots
#'
#' @param reactive_pop A reactive expression returning the population data.
#' @param reactive_tfr A reactive expression returning the TFR (Total Fertility Rate) data.
#' @param wpp_starting_year A reactive expression returning the starting year.
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param input,output Internal parameters for `{shiny}.
#'
#' @importFrom DT renderDataTable
#' @importFrom plotly ggplotly renderPlotly
#' @export
#'
handle_pop_tfr_plots <- function(reactive_pop, reactive_tfr, wpp_starting_year, wpp_ending_year, input, output) {
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

  # Render population table
  output$table_pop <- renderDataTable(prepare_pop_agegroups_table(reactive_pop()))
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
      div(style = "display: flex;", header_title),
      additional_header
    ),
    div(DTOutput(output_id)),
    footer = div(
      class = "footer-container",
      div(
        class = "file-input-container",
        shiny.semantic::fileInput(file_input_id, label = NULL, placeholder = "Upload CSV file", width = "100%")
      ),
      div(
        class = "button-container",
        div(
          style = "display: flex; gap: 5px",
          shiny::downloadButton(download_button_id, "Download", class = "ui blue button"),
          action_button(hide_button_id, "Close", class = "ui red button"),
        )
      )
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
handle_customize_data <- function(reactive_pop, reactive_tfr, tfr_starting_year, wpp_starting_year, wpp_ending_year, input, output) {
  observeEvent(input$customize_pop, {
    show_modal("modal_population")
  })

  observeEvent(input$hide_pop, {
    hide_modal("modal_population")
  })

  observeEvent(input$customize_tfr, {
    show_modal("modal_tfr")
  })

  observeEvent(input$hide_tfr, {
    hide_modal("modal_tfr")
  })

  output$tmp_pop_dt <- renderDT({
    res <- reactive_pop()
    names(res) <- c("Age", "Female", "Male")
    datatable(
      res,
      options = list(
        paging = TRUE,
        searching = FALSE,
        lengthChange = FALSE,
        dom = "lfrtp"
      )
    )
  })

  output$tmp_tfr_dt <- renderDT({
    res <- reactive_tfr()
    names(res) <- c("Year", "TFR")
    datatable(
      res,
      options = list(paging = TRUE, searching = FALSE, lengthChange = FALSE)
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

  data_uploaded_pop <- reactiveVal(NULL)
  data_uploaded_tfr <- reactiveVal(NULL)

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
}

#' Handle Navigation Between Steps
#'
#' This function manages the navigation between different pages of the application.
#'
#' @param reactive_pop A reactive expression that returns the population data to be displayed.
#' @param reactive_tfr A reactive expression that returns the TFR data to be displayed.
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
handle_navigation <- function(reactive_pop, reactive_tfr, wpp_starting_year, wpp_ending_year, input, output) {
  processing <- reactiveVal(TRUE)
  show_tfr_modal <- reactiveVal(TRUE)
  simulation_results <- reactiveVal()

  observeEvent(input$back_to_input_page, {
    hide("pop_page")
    show("input_page")
  })

  observeEvent(input$back_to_pop_page, {
    hide("tfr_page")
    show("pop_page")
  })

  observeEvent(input$back_to_tfr_page, {
    hide("forecast_page")
    show("tfr_page")
  })

  observeEvent(input$forward_pop_page, {
    hide("input_page")
    show("pop_page")

    # Repeated the create_pop_pyramid_plot here because it allows the spinner
    # around the page to register the time spent
    create_pop_pyramid_plot(reactive_pop())
    output$show_pop_results_ui <- renderUI({
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

  observeEvent(input$change_tfr_btn, {
    show_tfr(reactive_tfr, wpp_ending_year, input, output)
  })

  observeEvent(input$pass_tfr_btn, {
    hide_modal("modal_passtfr")
    hide("pop_page")
    show("forecast_page")

    # Compute in the background because sincer we're
    # jumping the actual TFR (jumping the traditional TFR page
    # which calculates the TFR), we need to compute the TFR
    # before the simulation.
    compute_tfr(reactive_tfr, wpp_ending_year, input, output)

    begin_forecast(
      reactive_pop,
      reactive_tfr,
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
        style = "font-size: 20px; font-weight: bold; padding: 10px; text-align: center; display: flex; justify-content: center; align-items: center;",
        `data-custom` = "value",
        HTML("Would you like to assume the Total Fertility Rate (TFR)?")
      ),
      footer = div(
        style = "display: flex; gap: 2px; justify-content: center;",
        div(
          style = "display: flex; gap: 10px;", # 20px gap between buttons
          action_button("change_tfr_btn", "Inspect TFR", class = "ui grey button"),
          action_button("pass_tfr_btn", "Assume TFR", class = "ui blue button")
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
