#' Create Population Pyramid Plot
#'
#' This function takes a data table and an optional input year to create a population pyramid plot.
#'
#' @param dt Data table with population data.
#' @param input_year The input year to filter the data on, default is NULL.
#'
#' @importFrom ggplot2 aes ggplot geom_bar coord_flip labs theme_minimal theme scale_x_discrete scale_y_continuous element_blank
#' @importFrom data.table melt
#' @importFrom plotly ggplotly layout
#' @importFrom scales cut_short_scale label_number
#'
#' @return A ggplot2 object.
#' @export
create_pop_pyramid <- function(dt, input_year = NULL) {
  if (!is.null(input_year)) {
    id_vars <- c("year", "age")
  } else {
    id_vars <- c("age")
  }

  pop_dt <-
    melt(
      dt,
      id.vars = id_vars,
      measure.vars = c("popF", "popM"),
      variable.name = "gender",
      value.name = "population"
    )

  print(pop_dt)

  pop_dt$age <- as.factor(pop_dt$age)

  males <- pop_dt[["gender"]] == "popM"
  pop_dt[males, "population"] <- -pop_dt[males, "population"]

  age <- NULL
  population <- NULL
  gender <- NULL

  if (!is.null(input_year)) {
    pop_dt <- pop_dt[pop_dt$year == as.numeric(input_year), ]
  }

  plt <-
    pop_dt %>%
    ggplot(aes(x = age, y = population, fill = gender)) +
    geom_bar(alpha = 0.7, stat = "identity") +
    scale_x_discrete(
      breaks = seq(0, 100, by = 5)
    ) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    coord_flip() +
    labs(title = NULL, x = "Age", y = "Population") +
    theme_minimal(base_size = 16) + # Increase font sizes
    theme(
      legend.position = "top",
      panel.grid.major.y = element_blank(), # Remove horizontal grid lines
      panel.grid.major.x = element_blank() # Remove horizontal grid lines
    )

  list(gg = plt, plotly = ggplotly(plt))
}

#' Create Age Group Plot
#'
#' This function takes a data table and an input scale to create an age group plot.
#'
#' @param dt Data table with population data.
#' @param input_scale The type of scale to be applied.
#'
#' @importFrom ggplot2 ggplot aes_string geom_line theme_minimal theme
#' @importFrom data.table melt
#' @importFrom plotly ggplotly layout
#'
#' @return A ggplot2 object.
#' @export
create_age_group_plot <- function(dt, input_scale) {
  y_axis <- ifelse(input_scale == "Percent", "pop_percent", "pop")

  pop_dt <-
    melt(
      dt,
      id.vars = c("year", "age"),
      measure.vars = c("pop", "pop_percent"),
      variable.name = "type_value",
      value.name = "value"
    )

  pop_dt <- pop_dt[pop_dt$type_value == y_axis, ]

  plt <-
    pop_dt %>%
    ggplot(aes_string("year", "value", color = "age")) +
    geom_line() +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "bottom"
    )

  list(gg = plt, plotly = ggplotly(plt))
}


#' Create Pop Age Group Time Plot
#'
#' This function takes a data table and an input of age group to create a time age group plot.
#'
#' @param dt Data table with population data.
#' @param input_age The age group to subset
#'
#' @importFrom ggplot2 ggplot aes_string geom_line theme_minimal theme geom_ribbon
#' @importFrom data.table melt
#' @importFrom plotly ggplotly layout
#'
#' @return A ggplot2 object.
#' @export
create_pop_time_plot <- function(dt, input_age) {
  pop_dt <-
    melt(
      dt,
      id.vars = c("year", "age", "un_pop_95low", "un_pop_95high"),
      measure.vars = c("pop", "un_pop_median"),
      variable.name = "type_value",
      value.name = "value"
    )

  pop_dt <- pop_dt[pop_dt$age == input_age, ]

  year <- NULL
  value <- NULL
  type_value <- NULL
  un_pop_95low <- NULL
  un_pop_95high <- NULL

  plt <-
    pop_dt %>%
    ggplot(aes(year, value, color = type_value, group = type_value)) +
    geom_line() +
    geom_ribbon(aes(ymin = un_pop_95low, ymax = un_pop_95high), alpha = 1 / 5) +
    theme_minimal(base_size = 16)

  list(gg = plt, plotly = ggplotly(plt))
}

#' Create forecasted TFR plot
#'
#' This function takes a data table to create a projected TFR plot.
#'
#' @param dt Data table with population data.
#' @param end_year the date in YYYY-MM-DD where the projection should end.
#'
#' @importFrom ggplot2 ggplot aes_string geom_line theme_minimal theme geom_ribbon scale_y_continuous
#' @importFrom data.table melt
#' @importFrom plotly ggplotly layout
#'
#' @return A ggplot2 object.
#' @export
create_tfr_projected_plot <- function(dt, end_year) {
  tfr_dt <-
    melt(
      dt,
      id.vars = c("year", "un_tfr_95low", "un_tfr_95high"),
      measure.vars = c("tfr", "un_tfr_median"),
      variable.name = "type_value",
      value.name = "value"
    )

  tfr_dt <- tfr_dt[tfr_dt$year <= as.numeric(end_year), ]

  year <- NULL
  value <- NULL
  type_value <- NULL
  un_tfr_95low <- NULL
  un_tfr_95high <- NULL

  plt <-
    tfr_dt %>%
    ggplot(aes(year, value, group = type_value)) +
    geom_line(aes(color = type_value)) +
    geom_ribbon(aes(ymin = un_tfr_95low, ymax = un_tfr_95high), alpha = 1 / 5) +
    scale_y_continuous(name = "Projected TFR") +
    theme_minimal(base_size = 16)

  list(gg = plt, plotly = ggplotly(plt))
}

#' Create Annual Growth Rate Time Plot by Age
#'
#' This function takes a data table to create an annual growth rate time plot by age.
#'
#' @param dt Data table with annual growth data.
#' @param end_year the date in YYYY-MM-DD where the projection should end.
#'
#' @importFrom ggplot2 ggplot aes_string geom_line theme_minimal theme geom_ribbon
#' @importFrom data.table melt
#' @importFrom plotly ggplotly layout
#'
#' @return A ggplot2 object.
#' @export
create_annual_growth_plot <- function(dt, end_year) {
  dt <- dt[dt$year <= as.numeric(end_year), ]
  dt$value <- dt$growth_rate
  dt$type_value <- dt$age

  year <- NULL
  value <- NULL
  type_value <- NULL

  plt <-
    dt %>%
    ggplot(aes(year, value, color = type_value, group = type_value)) +
    geom_line() +
    scale_y_continuous(name = "Annual Rate of Population Growth") +
    theme_minimal(base_size = 16)

  list(gg = plt, plotly = ggplotly(plt))
}


#' Create Total Fertility Rate Plot
#'
#' This function takes a data table to create a total fertility rate plot.
#'
#' @param dt Data table with fertility rate data.
#'
#' @importFrom ggplot2 aes ggplot geom_line labs theme_minimal
#' @importFrom plotly ggplotly
#'
#' @return A ggplot2 object.
#' @export
create_tfr_plot <- function(dt) {
  year <- NULL
  tfr <- NULL

  plt <-
    dt %>%
    ggplot(aes(x = year, y = tfr)) +
    geom_line(size = 2, alpha = 0.7) +
    labs(
      title = NULL,
      x = "Time",
      y = "Total Fertility Rate"
    ) +
    theme_minimal(base_size = 16)

  list(gg = plt, plotly = ggplotly(plt))
}

#' Prepare Population Age Groups Table
#'
#' This function takes a data table to prepare a population age groups table.
#'
#' @param wpp_dt Data table with population data.
#'
#' @importFrom shiny.semantic semantic_DT
#'
#' @return A shiny.semantic DataTable object.
#' @export
prepare_pop_agegroups_table <- function(wpp_dt) {
  wpp_dt$population <- wpp_dt$popF + wpp_dt$popM

  # Function to categorize ages
  age_grouping <- function(age) {
    ifelse(age <= 19, "0-19",
      ifelse(age <= 39, "20-39",
        ifelse(age <= 59, "40-59", "60+")
      )
    )
  }

  # Apply the age_grouping function to the age column
  wpp_dt$age_group <- sapply(wpp_dt$age, age_grouping)

  # Summarize population by age group
  age_groups <- unique(wpp_dt$age_group)
  population_sum <- tapply(wpp_dt$population, wpp_dt$age_group, sum)

  # Create a summary table
  summary_table <- data.frame(
    age_group = age_groups,
    population = population_sum[age_groups]
  )

  # Add a row for the total
  summary_table <- rbind(
    summary_table,
    data.frame(age_group = "Total", population = sum(summary_table$population))
  )

  # Format population
  summary_table$population_formatted <- ifelse(
    summary_table$population >= 1000,
    paste0(round(summary_table$population / 1000, 1), "M"),
    paste0(summary_table$population, "K")
  )

  # Calculate percentage
  total_population <- sum(summary_table$population)
  summary_table$percentage <- round(c(summary_table$population[1:nrow(summary_table) - 1] / total_population * 100, 100), 0)

  summary_table$population <- NULL
  names(summary_table) <- c("Age groups", "Population", "Percentage")
  row.names(summary_table) <- NULL

  shiny.semantic::semantic_DT(summary_table, options = list(
    paging = FALSE, # Disable pagination
    searching = FALSE, # Disable searching
    info = FALSE # Disable info like "Showing 1 of N"
  ))
}

#' Plot births/deaths data with median and confidence intervals
#'
#' This function plots the number of births or deaths along with median and confidence intervals
#' using the ggplot2 package. Data is filtered up to a specified end year.
#'
#' @param forecast_birth A data.table of birth data.
#' @param forecast_death A data.table of death data.
#' @param data_type A character string indicating the type of data to plot ("births" or "deaths").
#' @param value_type A character string indicating the type of value to plot ("count" or "rate").
#' @param end_year An integer specifying the upper year limit for the data to be plotted.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot geom_line geom_ribbon labs theme_minimal
#' @importFrom plotly ggplotly
#' @importFrom data.table setnames .SD :=
#'
#' @export
create_deaths_births_plot <- function(forecast_birth, forecast_death, data_type, value_type, end_year) {
  # Validate input
  if (!(data_type %in% c("birth", "death")) | !(value_type %in% c("counts", "rates"))) {
    stop("Invalid data_type or value_type")
  }

  # Select appropriate data.table based on data_type
  if (data_type == "birth") {
    data <- forecast_birth
  } else {
    data <- forecast_death
  }

  # Filter data up to the specified end year
  data <- data[year <= end_year]

  # Set type based on data_type and value_type
  type <- ifelse(value_type == "counts", data_type, paste0("c", substr(data_type, 1, 1), "r"))

  # Ensure that median and CI columns are numeric
  cols_to_convert <- grep("^un_", names(data), value = TRUE)
  data[, (cols_to_convert) := lapply(.SD, as.numeric), .SDcols = cols_to_convert]

  cols <- c("year", grep(type, names(data), value = TRUE))

  data <- data[, cols, with = FALSE]

  melt_data <-
    melt(
      data,
      id.vars = c("year", grep("high|low", names(data), value = TRUE)),
      measure.vars = names(data)[2:3],
      variable.name = "type_value",
      value.name = "value"
    )

  # Rename columns that contain "high" or "low"
  setnames(melt_data, old = grep("high", names(melt_data), value = TRUE), new = "high")
  setnames(melt_data, old = grep("low", names(melt_data), value = TRUE), new = "low")

  # Plot the data
  plt <-
    ggplot(
      melt_data,
      aes(x = year, y = value, group = type_value, color = type_value)
    ) +
    geom_line() +
    geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.2) +
    labs(x = "Year", y = paste(tools::toTitleCase(data_type), value_type), color = "Type") +
    theme_minimal()

  year <- NULL
  value <- NULL
  type_value <- NULL
  low <- NULL
  high <- NULL
  data_type <- NULL

  list(gg = plt, plotly = ggplotly(plt))
}


#' Plot YADR/OADR data with median and confidence intervals
#'
#' This function plots the YADR/OADR along with the UN median and confidence intervals
#' using the ggplot2 package. Data is filtered up to a specified end year.
#'
#' @param oadr A data.table of OADR data.
#' @param yadr A data.table of YADR data.
#' @param data_type A character string indicating the type of data to plot ("yadr" or "oadr").
#' @param end_year An integer specifying the upper year limit for the data to be plotted.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot geom_line geom_ribbon labs theme_minimal
#' @importFrom plotly ggplotly
#'
#' @export
create_yadr_oadr_plot <- function(oadr, yadr, data_type, end_year) {
  # Select appropriate data.table based on data_type
  data <- if (data_type == "yadr") {
    yadr
  } else {
    oadr
  }

  names(data) <- c("year", data_type, paste0("un_", data_type, "_median"), "low", "high")

  # Filter data up to the specified end year
  data <- data[year <= end_year]

  melt_data <-
    melt(
      data,
      id.vars = c("year", grep("high|low", names(data), value = TRUE)),
      measure.vars = names(data)[2:3],
      variable.name = "type_value",
      value.name = "value"
    )

  melt_data$low <- as.numeric(melt_data$low)
  melt_data$high <- as.numeric(melt_data$high)

  # Plot the data
  plt <-
    ggplot(
      melt_data,
      aes(x = year, y = value, group = type_value, color = type_value)
    ) +
    geom_line() +
    geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.2) +
    labs(x = "Year", y = toupper(data_type), color = "Type") +
    theme_minimal()

  year <- NULL
  value <- NULL
  type_value <- NULL
  low <- NULL
  high <- NULL
  data_type <- NULL

  list(gg = plt, plotly = ggplotly(plt))
}
