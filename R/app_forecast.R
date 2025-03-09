#' Run Forecast
#'
#' This function manages the simulation process and updates the UI accordingly.
#'
#' @param reactive_pop Population data
#' @param reactive_tfr TFR data
#' @param reactive_e0 e0 data
#' @param reactive_mig migration data
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
begin_forecast <- function(reactive_pop, reactive_tfr, reactive_e0, reactive_mig, wpp_starting_year, wpp_ending_year, input, output, simulation_results, i18n) {
  # Fixed output directory to /tmp/hasdaney213/ because run_forecast removes the temporary directory
  # automatically after runs and since plotly uses the temporary directory this
  # raises error. By fixing the output directory run_forecast and plotly use different
  # temporary directories.
  forecast_res <- reactive({

    # Here we check the each of these in case the user did NOT upload the data
    # because if they didn't, we provide a NULL a value so that Hana downloads
    # the data in a way we can't do in the front end and complies with what Patrick
    # needs
    pop <- reactive_pop()
    tfr <- check_data("tfr", reactive_tfr)
    e0 <- check_data("e0", reactive_e0)
    mig <- check_data("mig", reactive_mig)

    # Call the run_forecast function with the processed data
    res <- run_forecast(
      country = input$wpp_country,
      start_year = wpp_starting_year(),
      end_year = wpp_ending_year(),
      output_dir = "/tmp/hasdaney213/",
      pop = pop,
      tfr = tfr,
      e0 = e0,
      mig = mig
    )

    remove_last_year <- c(
      "population_by_age_and_sex",
      "population_by_broad_age_group",
      "population_by_time",
      "yadr",
      "oadr",
      "pop_aging_and_pop_size"
    )

    res[remove_last_year] <- lapply(
      res[remove_last_year],
      function(x) {
        x$year <- x$year + 1
        x[x$year != 2101]
      }
    )
    res
  })

  # Generates the tabset UI where all plots are rendered
  output$show_forecast_results_ui <- renderUI({
    simulation_results(forecast_res())
    div(
      show_forecast_results_ui(input, i18n)
    )
  })


  output$forecast_help_ui <- renderUI({
    action_button("forecast_help", i18n$t("Instructions"), class = "ui blue button")
  })

  output$all_pop_data <- shiny::downloadHandler(
    filename = function() paste0("all_data_pyramid_age_sex_", input$wpp_country, ".csv"),
    content = function(file) {
      utils::write.csv(
        simulation_results()$population_by_age_and_sex,
        file,
        row.names = FALSE
      )
    }
  )

  ##### Reactive Widgets calculatd based on the data #####
  age_pop_time <- reactive({
    ages <- unique(simulation_results()$population_by_time$age)
  })


  output$age_pop_time_ui <- renderUI({
    selectInput(
      inputId = "age_pop_time",
      label = i18n$t("Select age group"),
      choices = age_pop_time(),
      selected = age_pop_time()[1]
    )
  })


  sex_e0_time <- reactive({
    i18n$translate(c("Total", "Male", "Female"))
  })


  output$sex_e0_time_ui <- renderUI({
    selectInput(
      inputId = "sex_e0_time",
      label = i18n$translate("Select sex"),
      choices = sex_e0_time(),
      selected = sex_e0_time()[1]
    )
  })


  pop_age_sex_years <- reactive({
    unique(simulation_results()$population_by_age_and_sex$year)
  })

  output$pop_age_sex_years_ui <- renderUI({
    selectInput(
      inputId = "pop_age_sex_years",
      label = i18n$t("Select year"),
      choices = pop_age_sex_years(),
      selected = wpp_starting_year() + 1
    )
  })
  ##### Reactive Widgets end #####

  ##### Reactive Plots from the analysis page #####
  pyramid_plot <- reactive({
    req(input$pop_age_sex_years)

    create_pop_pyramid_plot(
      simulation_results()$population_by_age_and_sex,
      country = input$wpp_country,
      input_year = input$pop_age_sex_years,
      i18n
    )
  })

  age_group_plot <- reactive({
    req(input$radio_population_by_broad_age_group)

    # Find which "Percent" or "Absolute" translates to the selected value
    ops_vals <- c("Percent", "Absolute")
    chosen_val <- ops_vals[which(i18n$translate(ops_vals) == input$radio_population_by_broad_age_group)]
    # If no match found or empty result (during language change), use the input directly
    if (length(chosen_val) == 0) {
      chosen_val <- input$radio_population_by_broad_age_group
    }

    create_age_group_plot(
      simulation_results()$population_by_broad_age_group,
      chosen_val,
      input$wpp_country,
      i18n
    )
  })

  pop_time_plot <- reactive({
    if (is.null(input$age_pop_time)) {
      return(NULL)
    }

    req(input$age_pop_time)

    create_pop_time_plot(
      simulation_results()$population_by_time,
      input$age_pop_time,
      input$wpp_country,
      i18n
    )
  })

  tfr_projected_plot <- reactive({
    create_tfr_projected_plot(
      simulation_results()$tfr_by_time,
      wpp_ending_year(),
      input$wpp_country,
      i18n
    )
  })

  annual_growth_plot <- reactive({
    create_annual_growth_plot(
      simulation_results()$annual_growth_rate,
      wpp_ending_year(),
      input$wpp_country,
      i18n
    )
  })

  deaths_births_plot <- reactive({
    req(input$radio_death_births)

    # Find which "Percent" or "Absolute" translates to the selected value
    ops_vals <- c("Birth Counts", "Birth Rates", "Death Counts", "Death Rates")
    chosen_val <- ops_vals[which(i18n$translate(ops_vals) == input$radio_death_births)]
    # If no match found or empty result (during language change), use the input directly
    if (length(chosen_val) == 0) {
      chosen_val <- input$radio_death_births
    }

    # type_value here is something like "Birth Counts" or "Birth Rates"
    # we split it to define the type of value and titles and use
    # each word for different labels.
    type_value <- tolower(strsplit(chosen_val, " ")[[1]])
    create_deaths_births_plot(
      simulation_results()$births_counts_rates,
      simulation_results()$deaths_counts_rates,
      type_value[1],
      type_value[2],
      wpp_ending_year(),
      input$wpp_country,
      i18n
    )
  })

  yadr_oadr_plot <- reactive({
    req(input$radio_yadr_oadr)
    type_value <- tolower(input$radio_yadr_oadr)
    create_yadr_oadr_plot(
      simulation_results()$oadr,
      simulation_results()$yadr,
      type_value,
      wpp_ending_year(),
      input$wpp_country,
      i18n
    )
  })

  pop_size_aging_plot <- reactive({
    dt <- simulation_results()$pop_aging_and_pop_size
    max_year <- max(dt$year)
    min_year <- min(dt$year)
    plt_title <- paste0(
      "Population size and percent of population 65+",
      ": ",
      input$wpp_country,
      ", ",
      min_year,
      "-",
      max_year
    )

    create_un_projection_plot(
      dt,
      wpp_ending_year(),
      c(
        "pop" = "Population",
        "percent65" = "% of population 65+",
        "title" = plt_title
      ),
      percent_x = TRUE,
      i18n = i18n
    )
  })

  e0_by_cdr_plot <- reactive({
    dt <- simulation_results()$cdr_by_e0
    max_year <- max(dt$year)
    min_year <- min(dt$year)
    plt_title <- paste0(
      "Crude death rate and life expectancy at birth: ",
      input$wpp_country,
      ", ",
      min_year,
      "-",
      max_year
    )

    create_un_projection_plot(
      dt,
      wpp_ending_year(),
      c(
        "cdr" = "Crude Death Rate",
        "e0" = "Life Expectancy",
        "title" = plt_title
      ),
      i18n = i18n
    )
  })

  tfr_by_cdr_plot <- reactive({
    dt <- simulation_results()$cbr_by_tfr
    max_year <- max(dt$year)
    min_year <- min(dt$year)
    plt_title <- paste0(
      "Crude birth rate and total fertility rate: ",
      input$wpp_country,
      ", ",
      min_year,
      "-",
      max_year
    )

    create_un_projection_plot(
      dt,
      wpp_ending_year(),
      c(
        "cbr" = "Crude Birth Rate",
        "tfr" = "Total Fertility Rate",
        "title" = plt_title
      ),
      i18n = i18n
    )
  })


  e0_by_time_plot <- reactive({
    if (is.null(input$sex_e0_time)) {
      return(NULL)
    }

    req(input$sex_e0_time)

    # Find which "Percent" or "Absolute" translates to the selected value
    ops_vals <- c("Total", "Male", "Female")
    chosen_val <- ops_vals[which(i18n$translate(ops_vals) == input$sex_e0_time)]
    # If no match found or empty result (during language change), use the input directly
    if (length(chosen_val) == 0) {
      chosen_val <- input$sex_e0_time
    }

    create_e0_projected_plot(
      simulation_results()$e0_by_time,
      chosen_val,
      input$wpp_country,
      i18n
    )
  })


  mig_by_time_plot <- reactive({
    create_mig_projected_plot(
      simulation_results()$mig_by_time,
      wpp_ending_year(),
      input$wpp_country,
      i18n
    )
  })


  ##### Reactive Plots End #####

  cnt <- reactive({
    tolower(gsub(" ", "", input$wpp_country))
  })

  filename_deaths_births <- reactive({
    req(input$radio_death_births)
    paste0(
      tolower(strsplit(input$radio_death_births, " ")[[1]])[1],
      "_",
      tolower(strsplit(input$radio_death_births, " ")[[1]])[2],
      "_",
      cnt()
    )
  })

  filename_yadr_oadr <- reactive({
    paste0(
      tolower(input$radio_yadr_oadr),
      "_",
      cnt()
    )
  })

  filename_pop_over_time_agegroup <- reactive({
    paste0(
      "pop_over_time_agegroup_",
      tolower(input$age_pop_time),
      "_",
      cnt()
    )
  })

  filename_pop_by_age <- reactive({
    paste0(
      "pop_age_group_",
      tolower(input$radio_population_by_broad_age_group),
      "_",
      cnt()
    )
  })

  filename_pop_pyramid <- reactive({
    paste0(
      "pyramid_age_sex_",
      cnt(),
      "_",
      input$pop_age_sex_years
    )
  })

  filename_e0_over_time_sex <- reactive({
    paste0(
      "e0_sex_",
      cnt(),
      "_",
      input$sex_e0_time
    )
  })

  ##### Generate all plots and show in the tabset UI #####
  observe({
    req(input$select_id)

    # Get the current tab index by matching input$select_id against current translations
    current_index <- NULL
    for (i in seq_along(TAB_NAMES)) {
      if (input$select_id == i18n$translate(TAB_NAMES[i])) {
        current_index <- i
        break
      }
    }
    
    # If we couldn't find a match, default to first tab
    if (is.null(current_index)) {
      current_index <- 1
    }
    
    # Use the index to select the appropriate plot
    if (current_index == 1) {  # Population Pyramid
      selected_plot <- list(
        plt_reactive = pyramid_plot,
        filename = filename_pop_pyramid()
      )
    } else if (current_index == 2) {  # Population by Broad Age Groups
      selected_plot <- list(
        plt_reactive = age_group_plot,
        filename = filename_pop_by_age()
      )
    } else if (current_index == 3) {  # Population Over Time
      selected_plot <- list(
        plt_reactive = pop_time_plot,
        filename = filename_pop_over_time_agegroup()
      )
    } else if (current_index == 4) {  # Projected Total Fertility Rate
      selected_plot <- list(
        plt_reactive = tfr_projected_plot,
        filename = paste0("tfr_projection_", cnt())
      )
    } else if (current_index == 5) {  # Population Growth Rate by Age
      selected_plot <- list(
        plt_reactive = annual_growth_plot,
        filename = paste0("pop_growth_rate_", cnt())
      )
    } else if (current_index == 6) {  # Deaths and Births
      selected_plot <- list(
        plt_reactive = deaths_births_plot,
        filename = filename_deaths_births()
      )
    } else if (current_index == 7) {  # YADR and OADR
      selected_plot <- list(
        plt_reactive = yadr_oadr_plot,
        filename = filename_yadr_oadr()
      )
    } else if (current_index == 8) {  # Population Size and Aging
      selected_plot <- list(
        plt_reactive = pop_size_aging_plot,
        filename = paste0("total_pop_and_aging_pop_", cnt())
      )
    } else if (current_index == 9) {  # CDR and Life Expectancy
      selected_plot <- list(
        plt_reactive = e0_by_cdr_plot,
        filename = paste0("death_rate_life_exp_", cnt())
      )
    } else if (current_index == 10) {  # CBR and TFR
      selected_plot <- list(
        plt_reactive = tfr_by_cdr_plot,
        filename = paste0("tfr_cdr_", cnt())
      )
    } else if (current_index == 11) {  # Life Expectancy Over Time
      selected_plot <- list(
        plt_reactive = e0_by_time_plot,
        filename = filename_e0_over_time_sex()
      )
    } else if (current_index == 12) {  # Projected Net Migration
      selected_plot <- list(
        plt_reactive = mig_by_time_plot,
        filename = paste0("mig_projection_", cnt())
      )
    }

    # Use the English name for the plot ID
    selected_id <- TAB_NAMES[current_index]
    plots_tabset(input, output, selected_id, selected_plot)
  })
  ##### Finish plotting in tabs #####
}


# Helper function to check the condition
check_data <- function(indicator_name, reactive_data) {
  if (data_source[[indicator_name]] == "downloaded") {
    return(NULL)
  }

  return(reactive_data())
}
