#' Generate a tabset for the application
#'
#' @return A tabset UI component for the application
#' @importFrom shiny.semantic tabset multiple_radio
#' @importFrom shiny actionButton div
#' @importFrom untheme create_tab
#' @export
app_tabset <- function() {
  tabs <- list(
    create_tab("Pop Pyramid", "plot1", uiOutput("pop_age_sex_years_ui"), width = "800px"),
    create_tab("Pop by Age", "plot2", multiple_radio("radio_population_by_broad_age_group", "Scale Type", choices = c("Percent", "Absolute"), type = "inline")),
    create_tab("Pop Over Time", "plot3", uiOutput("age_pop_time_ui")),
    create_tab("TFR", "plot4"),
    create_tab("Pop Growth", "plot5"),
    create_tab("Deaths and Births", "plot6", multiple_radio("radio_death_births", "Type of plot", choices = c("Birth counts", "Birth rates", "Death counts", "Death rates"), type = "inline")),
    create_tab("YADR and OADR", "plot7", multiple_radio("radio_yadr_oadr", "Type of plot", choices = c("YADR", "OADR"), type = "inline")),
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
show_pop_results_ui <- function() {
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
show_tfr_results_ui <- function() {
  div(
    class = "ui raised very padded container segment",
    style = "display: flex; align-items: flex-start; gap: 10px; width: 75%",
    div(
      style = "flex: 3;",
      plotlyOutput("plot_tfr", height = "600px", width = "85%")
    )
  )
}

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
handle_pop_tfr_plots <- function(reactive_pop, reactive_tfr, wpp_starting_year, wpp_ending_year, input, output) {
  # Render plots for population pyramid and total fertility rate
  output$plot_pop <- renderPlotly(
    create_pop_pyramid_plot(
      reactive_pop(),
      country = input$wpp_country,
      input_year = wpp_starting_year()
    )$plotly
  )

  output$plot_tfr <- renderPlotly(
    create_tfr_plot(
      reactive_tfr(),
      end_year = wpp_ending_year(),
      country = input$wpp_country
    )$plotly
  )

  # Render population table
  output$table_pop <- renderDataTable(prepare_pop_agegroups_table(reactive_pop()))
}


#' Handle Customization of Data in a Shiny App
#'
#' This function sets up the UI elements to display and customize population and total fertility rate (TFR) data in a Shiny application. It renders data tables for population and TFR data and sets up modals for data customization.
#'
#' @param reactive_pop A reactive expression that returns the population data to be displayed.
#' @param reactive_tfr A reactive expression that returns the TFR data to be displayed.
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
handle_customize_data <- function(reactive_pop, reactive_tfr, wpp_starting_year, wpp_ending_year, input, output) {
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
    modal(
      id = "modal_population",
      header = div(
        div(
          style = "display: flex;",
          paste0(
            "Population data for ",
            input$wpp_country,
            " between ",
            wpp_starting_year(),
            " and ",
            wpp_ending_year()
          )
        ),
        div(
          tags$em("Population units are in thousands. Any edits need to be added in thousands"),
          style = "font-weight: normal; font-size: smaller;"
        )
      ),
      div(
        DTOutput("tmp_pop_dt")
      ),
      footer = div(
        div(
          style = "display: flex; gap: 2px; justify-content: center;",
          div(
            style = "flex: 0;", # Flexible div for spacing
            shiny.semantic::fileInput("upload_pop", label = NULL, placeholder = "Upload CSV file", width = "50%")
          ),
          div(
            style = "flex: 0;", # Flexible div for spacing
            div(
              style = "display: flex; gap: 5px",
              shiny::downloadButton("download_pop", "Download", class = "ui blue button"),
              action_button("hide_pop", "Close", class = "ui red button"),
            )
          )
        ),
      ),
      class = "small"
    )
  })

  output$popup_tfr <- renderUI({
    modal(
      div(
        DTOutput("tmp_tfr_dt"),
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
          style = "flex: 0;",
          div(
            style = "display: flex; gap: 5px",
            shiny::downloadButton("download_tfr", "Download", class = "ui blue button"),
            action_button("hide_tfr", "Close", class = "ui red button")
          )
        )
      ),
      class = "small"
    )
  })
}


#' Compute the TFR page
#'
#' @param reactive_tfr A reactive function returning the TFR data frame
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param input,output Internal parameters for `{shiny}`.
#' @importFrom shiny renderUI
#' @noRd
compute_tfr <- function(reactive_tfr, wpp_ending_year, input, output) {
  # Repeated the create_tfr_plot here because it allows the spinner
  # around the page to register the time spent
  create_tfr_plot(reactive_tfr(), end_year = wpp_ending_year(), country = input$wpp_country)
  output$show_tfr_results_ui <- renderUI(show_tfr_results_ui())
}

#' Show and compute the TFR page
#'
#' @param reactive_tfr A reactive function returning the TFR data frame
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param input,output Internal parameters for `{shiny}`.
#'
#' @importFrom shinyjs hide show
#' @importFrom shiny.semantic hide_modal
#' @noRd
show_tfr <- function(reactive_tfr, wpp_ending_year, input, output) {
  hide_modal("modal_passtfr")
  hide("step2")
  show("step3")
  compute_tfr(reactive_tfr, wpp_ending_year, input, output)
}


#' Handle Navigation Between Steps
#'
#' This function manages the navigation between different steps of the application.
#'
#' @param reactive_pop A reactive expression that returns the population data to be displayed.
#' @param reactive_tfr A reactive expression that returns the TFR data to be displayed.
#' @param wpp_starting_year A reactive expression returning the starting year.
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param input,output Internal parameters for `{shiny}`.
#' @importFrom shinyjs hide show
#' @importFrom shiny renderUI HTML
#' @importFrom shiny.semantic show_modal hide_modal
#' @importFrom shinyalert shinyalert
#' @importFrom utils write.csv
#' @noRd
handle_navigation <- function(reactive_pop, reactive_tfr, wpp_starting_year, wpp_ending_year, input, output) {
  processing <- reactiveVal(TRUE)

  observeEvent(input$forward_step2, {
    hide("step1")
    show("step2")

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

  output$pass_tfr <- renderUI({
    modal(
      id = "modal_passtfr",
      ## content = "Would you like to assume the Total Fertility Rate (TFR)?",
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

  show_tfr_modal <- reactiveVal(TRUE)

  observeEvent(input$forward_step3, {
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
    hide("step2")
    show("step4")

    # compute in the background
    compute_tfr(reactive_tfr, wpp_ending_year, input, output)

    # Define a reactiveVal to store simulation results
    simulation_results <- reactiveVal()
    begin_simulation(reactive_pop, reactive_tfr, wpp_starting_year, wpp_ending_year, input, output, simulation_results)
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

  output$download_pop <- shiny::downloadHandler(
    filename = function() {
      paste0(
        "population_",
        tolower(gsub(" ", "", input$wpp_country)),
        "_",
        wpp_starting_year(),
        "_",
        wpp_ending_year()
      )
    },
    content = function(file) {
      write.csv(reactive_pop(), file, row.names = FALSE)
    }
  )

  output$download_tfr <- shiny::downloadHandler(
    filename = function() {
      paste0(
        "tfr_",
        tolower(gsub(" ", "", input$wpp_country)),
        "_",
        wpp_starting_year(),
        "_",
        wpp_ending_year()
      )
    },
    content = function(file) {
      write.csv(reactive_tfr(), file, row.names = FALSE)
    }
  )

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
#' @param reactive_pop Population data
#' @param reactive_tfr TFR data
#' @param wpp_starting_year A reactive expression returning the starting year.
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param input,output Internal parameter for `{shiny}`.
#' @param simulation_results A reactive value to store simulation results.
#' @importFrom shiny reactive renderUI req
#' @importFrom shiny.semantic selectInput
#' @importFrom shinyjs hide show
#' @noRd
begin_simulation <- function(reactive_pop, reactive_tfr, wpp_starting_year, wpp_ending_year, input, output, simulation_results) {
  # Fixed output directory to /tmp/hasdaney213/ because run_forecast removes the temporary directory
  # automatically after runs and since plotly uses the temporary directory this
  # raises error. By fixing the output directory run_forecast and plotly use different
  # temporary directories.
  forecast_res <- reactive({
    run_forecast(
      country = input$wpp_country,
      start_year = wpp_starting_year(),
      end_year = wpp_ending_year(),
      output_dir = "/tmp/hasdaney213/",
      pop = reactive_pop(),
      tfr = reactive_tfr()
    )
  })

  # Generates the tabset UI where all plots are rendered
  output$app_tabset <- renderUI({
    simulation_results(forecast_res())
    app_tabset()
  })

  ##### Reactive Widgets calculatd based on the data #####
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
      selected = wpp_starting_year() + 1
    )
  })
  ##### Reactive Widgets end #####


  ##### Reactive Plots from the analysis page #####
  pyramid_plot <- reactive({
    req(simulation_results())
    create_pop_pyramid_plot(
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
      wpp_ending_year()
    )
  })

  annual_growth_plot <- reactive({
    create_annual_growth_plot(
      simulation_results()$annual_growth_rate,
      wpp_ending_year()
    )
  })

  deaths_births_plot <- reactive({
    # type_value here is something like "Birth Counts" or "Birth Rates"
    # we split it to define the type of value and titles and use
    # each word for different labels.
    type_value <- tolower(strsplit(input$radio_death_births, " ")[[1]])
    create_deaths_births_plot(
      simulation_results()$births_counts_rates,
      simulation_results()$deaths_counts_rates,
      type_value[1],
      type_value[2],
      wpp_ending_year()
    )
  })

  yadr_oadr_plot <- reactive({
    type_value <- tolower(input$radio_yadr_oadr)
    create_yadr_oadr_plot(
      simulation_results()$oadr,
      simulation_results()$yadr,
      type_value,
      wpp_ending_year()
    )
  })

  pop_size_aging_plot <- reactive({
    create_un_projection_plot(
      simulation_results()$pop_aging_and_pop_size,
      wpp_ending_year(),
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
      wpp_ending_year(),
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
      wpp_ending_year(),
      c(
        "cbr" = "Crude Birth Rate",
        "tfr" = "Total Fertility Rate",
        "title" = "Crude Birth Rate by Total Fertility Rate Over Time"
      )
    )
  })

  ##### Reactive Plots End #####

  ##### Generate all plots and show in the tabset UI #####
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
  ##### Finish plotting in tabs #####
}

#' The Application Server-Side Logic
#'
#' This function defines the server logic for the Shiny application,
#' managing data processing, UI rendering, and routing.
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#' @importFrom shiny reactive reactiveVal renderPlot renderTable observeEvent updateNumericInput renderUI observe wellPanel
#' @importFrom shinyjs hide show
#' @importFrom DT renderDT DTOutput datatable
#' @importFrom utils read.csv
#' @importFrom shiny.semantic modal
#' @importFrom shiny.fluent TooltipHost Image
#' @importFrom untheme plotWithDownloadButtons plots_tabset
#' @importFrom OPPPserver get_wpp_pop get_wpp_tfr run_forecast remove_forecast
#' @importFrom plotly renderPlotly
#' @export
app_server <- function(input, output, session) {
  # Make the www folder available for loading images and icons
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  wpp_starting_year <- reactive(as.numeric(input$wpp_starting_year))
  wpp_ending_year <- reactive(as.numeric(input$wpp_ending_year))

  # TODO: remove this and import directly run_forecast. Need to fix
  # dependency issue
  library(OPPPserver)

  output$next_pop_page <- renderUI({
    if (wpp_ending_year() < wpp_starting_year()) {
      wellPanel(
        class = "danger",
        HTML("\u274C Ending year should be higher than starting year")
      )
    } else {
      action_button("forward_step2", "Next", class = "ui blue button")
    }
  })

  reactive_pop <- reactive({
    if (!is.null(input$upload_pop) && nrow(input$upload_pop) > 0) {
      res <- data.table(read.csv(input$upload_pop$datapath))
      names(res) <- c("age", "popF", "popM")
    } else {
      res <- get_wpp_pop(input$wpp_country, wpp_starting_year())
    }
    res
  })

  reactive_tfr <- reactive({
    if (!is.null(input$upload_tfr) && nrow(input$upload_tfr) > 0) {
      res <- data.table(read.csv(input$upload_tfr$datapath))
      names(res) <- c("year", "tfr")
    } else {
      res <- get_wpp_tfr(input$wpp_country)
    }
    res
  })

  # Handle pop/tfr plots/tables before analysis
  handle_pop_tfr_plots(reactive_pop, reactive_tfr, wpp_starting_year, wpp_ending_year, input, output)

  # Handle navigation between steps
  handle_navigation(reactive_pop, reactive_tfr, wpp_starting_year, wpp_ending_year, input, output)

  # Handle all customize buttons
  handle_customize_data(reactive_pop, reactive_tfr, wpp_starting_year, wpp_ending_year, input, output)

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

  # Define a reactiveVal to store simulation results
  simulation_results <- reactiveVal()

  # Begin simulation on button click
  observeEvent(input$begin, {
    hide("step3")
    show("step4")

    begin_simulation(reactive_pop, reactive_tfr, wpp_starting_year, wpp_ending_year, input, output, simulation_results)
  })
}
