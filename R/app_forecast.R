#' Run Forecast
#'
#' This function manages the simulation process and updates the UI accordingly.
#'
#' @param reactive_pop Population data
#' @param reactive_tfr TFR data
#' @param wpp_starting_year A reactive expression returning the starting year.
#' @param wpp_ending_year A reactive expression returning the ending year.
#' @param input,output Internal parameter for `{shiny}`.
#' @param simulation_results A reactive value to store simulation results.
#'
#' @importFrom shiny reactive renderUI req
#' @importFrom shiny.semantic selectInput
#' @importFrom shinyjs hide show
#' @importFrom untheme plots_tabset
#' @importFrom OPPPserver run_forecast
#' @export
#'
begin_forecast <- function(reactive_pop, reactive_tfr, wpp_starting_year, wpp_ending_year, input, output, simulation_results) {
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
  output$show_forecast_results_ui <- renderUI({
    simulation_results(forecast_res())
    show_forecast_results_ui()
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
