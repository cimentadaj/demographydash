#' Generate a tabset for the application
#'
#' @return A tabset UI component for the application
#' @importFrom shiny.semantic tabset
#' @importFrom shiny actionButton div
#' @importFrom untheme create_tab
#' @export
app_tabset <- function() {
  tabs <- list(
    create_tab("Pop Pyramid", "plot1", uiOutput("pop_age_sex_years_ui"), width = "800px"),
    create_tab("Pop by Age", "plot2", shiny.semantic::multiple_radio("radio_population_by_broad_age_group", "Scale Type", choices = c("Percent", "Absolute"), type = "inline")),
    create_tab("Pop Over Time", "plot3", uiOutput("age_pop_time_ui")),
    create_tab("TFR", "plot4"),
    create_tab("Pop Growth", "plot5"),
    create_tab("Deaths and Births", "plot6", shiny.semantic::multiple_radio("radio_death_births", "Type of plot", choices = c("Birth counts", "Birth rates", "Death counts", "Death rates"), type = "inline")),
    create_tab("YADR and OADR", "plot7", shiny.semantic::multiple_radio("radio_yadr_oadr", "Type of plot", choices = c("YADR", "OADR"), type = "inline")),
    create_tab("Pop and Aging", "plot8"),
    create_tab("Life Expectancy and CDR", "plot9"),
    create_tab("TFR by CDR", "plot10")
  )

  div(tabset(tabs = tabs))
}

#' UI component for step one
#'
#' @return A div containing UI elements for step one
#' @importFrom shiny actionButton tableOutput
#' @importFrom shiny.semantic grid
#' @importFrom plotly plotlyOutput
#' @export
step_one_ui <- function() {
  div(
    class = "ui raised very padded container segment",
    style = "display: flex; align-items: flex-start; gap: 10px; width: 70%",
    div(
      style = "flex: 2;",
      plotlyOutput("plot_pop", height = "600px", width = "85%")
    ),
    div(
      style = "flex: 1;",
      shiny.semantic::semantic_DTOutput("table_pop", height = "400px")
    )
  )
}

#' UI component for step two
#'
#' @return A div containing UI elements for step two
#' @importFrom shiny actionButton tableOutput
#' @importFrom shiny.semantic grid
#' @importFrom plotly plotlyOutput
#' @export
step_two_ui <- function() {
  div(
    class = "ui raised very padded container segment",
    style = "display: flex; align-items: flex-start; gap: 10px; width: 75%",
    div(
      style = "flex: 3;",
      plotlyOutput("plot_tfr", height = "600px", width = "85%")
    )
  )
}


#' The Application Server-Side Logic
#'
#' This function defines the server logic for the Shiny application,
#' managing data processing, UI rendering, and routing.
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#' @importFrom shiny reactive reactiveVal renderPlot renderTable observeEvent updateNumericInput renderUI observe
#' @importFrom shinyjs hide show
#' @importFrom DT renderDT DTOutput renderDataTable datatable
#' @importFrom utils read.csv
#' @importFrom shiny.semantic modal
#' @importFrom untheme plotWithDownloadButtons plots_tabset
#' @importFrom OPPPserver get_wpp_pop get_wpp_tfr run_forecast remove_forecast
#' @importFrom plotly renderPlotly
#' @export
app_server <- function(input, output, session) {
  # TODO: remove this and import directly run_forecast. Need to fix
  # dependency issue
  library(OPPPserver)

  reactive_pop <- reactive({
    if (!is.null(input$upload_pop) && nrow(input$upload_pop) > 0) {
      res <- data.table(read.csv(input$upload_pop$datapath))
    } else {
      res <- get_wpp_pop(input$wpp_country, input$wpp_starting_year)
    }
    res
  })

  reactive_tfr <- reactive({
    if (!is.null(input$upload_tfr) && nrow(input$upload_tfr) > 0) {
      res <- data.table(read.csv(input$upload_tfr$datapath))
    } else {
      res <- get_wpp_tfr(input$wpp_country)
    }
    res
  })

  # Render plots for population pyramid and total fertility rate
  output$plot_pop <- renderPlotly(
    create_pop_pyramid(
      reactive_pop(),
      country = input$wpp_country,
      input_year = as.numeric(input$wpp_starting_year)
    )$plotly
  )

  output$plot_tfr <- renderPlotly(
    create_tfr_plot(
      reactive_tfr(),
      end_year = input$wpp_ending_year,
      country = input$wpp_country
    )$plotly
  )

  # Render population table
  output$table_pop <- renderDataTable(prepare_pop_agegroups_table(reactive_pop()))

  # Handle navigation between steps
  handle_navigation(reactive_pop, reactive_tfr, input, output)

  # Handle all customize buttons
  handle_customize_data(reactive_pop, reactive_tfr, output)

  # Define a reactiveVal to store simulation results
  simulation_results <- reactiveVal()

  # Begin simulation on button click
  observeEvent(input$begin, {
    hide("step3")
    show("step4")

    begin_simulation(input, reactive_pop, simulation_results, output)
  })
}

#' Handle Customization of Data in a Shiny App
#'
#' This function sets up the UI elements to display and customize population and total fertility rate (TFR) data in a Shiny application. It renders data tables for population and TFR data and sets up modals for data customization.
#'
#' @param reactive_pop A reactive expression that returns the population data to be displayed.
#' @param reactive_tfr A reactive expression that returns the TFR data to be displayed.
#' @param output The output list from the Shiny server function where the UI elements will be rendered.
#'
#' @importFrom shiny renderUI div br
#' @importFrom shiny.semantic fileInput action_button modal
#' @importFrom DT datatable renderDT DTOutput
#'
#' @return None
#' @export
#'
handle_customize_data <- function(reactive_pop, reactive_tfr, output) {
  output$pop_dt <- renderDT({
    datatable(
      reactive_pop(),
      options = list(paging = TRUE, searching = FALSE, lengthChange = FALSE)
    )
  })

  output$tfr_dt <- renderDT({
    datatable(
      reactive_tfr(),
      options = list(paging = TRUE, searching = FALSE, lengthChange = FALSE)
    )
  })

  output$popup_pop <- renderUI({
    modal(
      div(
        DTOutput("pop_dt"),
      ),
      br(),
      id = "modal_population",
      header = "Population data",
      footer = div(
        style = "display: flex; gap: 2px; justify-content: center;",
        div(
          style = "flex: 0;", # Flexible div for spacing
          shiny.semantic::fileInput("upload_pop", label = NULL, placeholder = "Upload CSV file", width = "50%")
        ),
        div(
          style = "flex: 0;", # Flexible div for spacing
          action_button("hide_pop", "Close", class = "ui blue button")
        )
      ),
      class = "small"
    )
  })

  output$popup_tfr <- renderUI({
    modal(
      div(
        DTOutput("tfr_dt"),
      ),
      br(),
      id = "modal_tfr",
      header = "Total Fertility data",
      footer = div(
        style = "display: flex; gap: 2px; justify-content: center;",
        div(
          style = "flex: 0;", # Flexible div for spacing
          shiny.semantic::fileInput("upload_tfr", label = NULL, placeholder = "Upload CSV file", width = "50%")
        ),
        div(
          style = "flex: 0;", # Flexible div for spacing
          action_button("hide_tfr", "Close", class = "ui blue button")
        )
      ),
      class = "small"
    )
  })
}

#' Handle Navigation Between Steps
#'
#' This function manages the navigation between different steps of the application.
#'
#' @param input,output Internal parameters for `{shiny}`.
#' @importFrom shinyjs hide show
#' @importFrom shiny renderUI HTML
#' @importFrom shiny.semantic show_modal hide_modal
#' @importFrom shinyalert shinyalert
#' @noRd
handle_navigation <- function(reactive_pop, reactive_tfr, input, output) {
  processing <- reactiveVal(TRUE)

  observeEvent(input$forward_step2, {
    hide("step1")
    show("step2")

    # Added this twice because it allows the spinner around step_one_ui to
    # register the time spent
    create_pop_pyramid(reactive_pop())
    output$step_one_ui <- renderUI({
      res <- step_one_ui()
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
              \xF0\x9F\xA7\xAE Upload: single year ages with an open interval at 100+")
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

  observeEvent(input$forward_step3, {
    hide("step2")
    show("step3")

    # Added this twice because it allows the spinner around step_two_ui to
    # register the time spent
    create_tfr_plot(reactive_tfr(), end_year = input$wpp_ending_year, country = input$wpp_country)
    output$step_two_ui <- renderUI(step_two_ui())
  })

  observeEvent(input$back_to_step1, {
    hide("step2")
    show("step1")
  })

  observeEvent(input$back_to_step2, {
    hide("step3")
    show("step2")
  })

  observeEvent(input$back_to_step3, {
    hide("step4")
    show("step3")
  })

  observeEvent(input$customize_pop, {
    show_modal("modal_population")
  })

  observeEvent(input$hide_pop, {
    hide_modal("modal_population")
  })

  dataUploaded <- reactiveVal(NULL)

  # Observer for file upload
  observeEvent(input$upload_pop, {
    req(input$upload_pop)
    dataUploaded(TRUE)
    reactive_pop()
  })

  observeEvent(input$customize_tfr, {
    show_modal("modal_tfr")
  })

  observeEvent(input$hide_tfr, {
    hide_modal("modal_tfr")
  })

  dataUploaded_tfr <- reactiveVal(NULL)

  # Observer for file upload
  observeEvent(input$upload_tfr, {
    req(input$upload_tfr)
    dataUploaded_tfr(TRUE)
    reactive_tfr()
  })
}

#' Begin Simulation
#'
#' This function manages the simulation process and updates the UI accordingly.
#'
#' @param input Internal parameter for `{shiny}`.
#' @param pop_dt Population data
#' @param simulation_results A reactive value to store simulation results.
#' @param output Internal parameter for `{shiny}`.
#' @importFrom shiny reactive renderUI req
#' @importFrom shiny.semantic selectInput
#' @importFrom shinyjs hide show
#' @noRd
begin_simulation <- function(input, pop_dt, simulation_results, output) {
  # TODO: fix 2021
  forecast_res <- reactive({
    run_forecast(
      country = input$wpp_country,
      start_year = 2021,
      end_year = as.numeric(input$wpp_ending_year),
      output_dir = "/tmp/hasdaney213/",
      pop = pop_dt()
    )
  })

  output$app_tabset <- renderUI({
    simulation_results(forecast_res())
    app_tabset()
  })

  age_pop_time <- reactive({
    ages <- unique(simulation_results()$population_by_time$age)
  })

  output$age_pop_time_ui <- renderUI({
    selectInput(
      inputId = "age_pop_time",
      label = "Select age group",
      choices = age_pop_time(),
      selected = age_pop_time()[1]
    )
  })

  pop_age_sex_years <- reactive({
    unique(simulation_results()$population_by_age_and_sex$year)
  })

  output$pop_age_sex_years_ui <- renderUI({
    selectInput(
      inputId = "pop_age_sex_years",
      label = "Select year",
      choices = pop_age_sex_years(),
      selected = as.numeric(input$wpp_starting_year) + 1
    )
  })

  pyramid_plot <- reactive({
    create_pop_pyramid(
      simulation_results()$population_by_age_and_sex,
      input_year = input$pop_age_sex_years
    )
  })

  age_group_plot <- reactive({
    create_age_group_plot(
      simulation_results()$population_by_broad_age_group,
      input$radio_population_by_broad_age_group
    )
  })

  pop_time_plot <- reactive({
    if (is.null(input$age_pop_time)) {
      return(NULL)
    }

    create_pop_time_plot(
      simulation_results()$population_by_time, input$age_pop_time
    )
  })

  tfr_projected_plot <- reactive({
    create_tfr_projected_plot(
      simulation_results()$tfr_by_time,
      as.numeric(input$wpp_ending_year)
    )
  })

  annual_growth_plot <- reactive({
    create_annual_growth_plot(
      simulation_results()$annual_growth_rate,
      as.numeric(input$wpp_ending_year)
    )
  })

  deaths_births_plot <- reactive({
    type_value <- tolower(strsplit(input$radio_death_births, " ")[[1]])
    create_deaths_births_plot(
      simulation_results()$births_counts_rates,
      simulation_results()$deaths_counts_rates,
      type_value[1],
      type_value[2],
      as.numeric(input$wpp_ending_year)
    )
  })

  yadr_oadr_plot <- reactive({
    type_value <- tolower(input$radio_yadr_oadr)
    create_yadr_oadr_plot(
      simulation_results()$oadr,
      simulation_results()$yadr,
      type_value,
      as.numeric(input$wpp_ending_year)
    )
  })

  pop_size_aging_plot <- reactive({
    create_un_projection_plot(
      simulation_results()$pop_aging_and_pop_size,
      as.numeric(input$wpp_ending_year),
      c(
        "pop" = "Population",
        "percent65" = "% of population 65+",
        "title" = "Population Size by Percentage of Population Over 65+ Over Time"
      ),
      percent_x = TRUE
    )
  })

  e0_by_cdr_plot <- reactive({
    create_un_projection_plot(
      simulation_results()$cdr_by_e0,
      as.numeric(input$wpp_ending_year),
      c(
        "cdr" = "Crude Death Rate",
        "e0" = "Life Expectancy",
        "title" = "Crude Death Rate by Life Expectancy Over Time"
      )
    )
  })

  tfr_by_cdr_plot <- reactive({
    create_un_projection_plot(
      simulation_results()$cbr_by_tfr,
      as.numeric(input$wpp_ending_year),
      c(
        "cbr" = "Crude Birth Rate",
        "tfr" = "Total Fertility Rate",
        "title" = "Crude Birth Rate by Total Fertility Rate Over Time"
      )
    )
  })

  observe({
    input$radio_population_by_broad_age_group
    input$age_pop_time

    req(
      input$pop_age_sex_years
    )

    cnt <- tolower(gsub(" ", "", input$wpp_country))
    births_deaths <- tolower(strsplit(input$radio_death_births, " ")[[1]])
    yadr_oadr <- tolower(input$radio_yadr_oadr)

    plots_tabset(
      list(
        plt_reactive = pyramid_plot,
        filename = paste0("pyramid_age_sex_", cnt, "_", input$pop_age_sex_years)
      ),
      list(
        plt_reactive = age_group_plot,
        filename = paste0("pop_age_group_", tolower(input$radio_population_by_broad_age_group), "_", cnt)
      ),
      list(
        plt_reactive = pop_time_plot,
        filename = paste0("pop_over_time_agegroup_", tolower(input$age_pop_time), "_", cnt)
      ),
      list(
        plt_reactive = tfr_projected_plot,
        filename = paste0("tfr_projection_", cnt)
      ),
      list(
        plt_reactive = annual_growth_plot,
        filename = paste0("pop_growth_rate_", cnt)
      ),
      list(
        plt_reactive = deaths_births_plot,
        filename = paste0(births_deaths[1], "_", births_deaths[2], "_", cnt)
      ),
      list(
        plt_reactive = yadr_oadr_plot,
        filename = paste0(yadr_oadr, "_", cnt)
      ),
      list(
        plt_reactive = pop_size_aging_plot,
        filename = paste0("total_pop_and_aging_pop_", cnt)
      ),
      list(
        plt_reactive = e0_by_cdr_plot,
        filename = paste0("death_rate_life_exp_", cnt)
      ),
      list(
        plt_reactive = tfr_by_cdr_plot,
        filename = paste0("tfr_cdr_", cnt)
      )
    )
  })
}
