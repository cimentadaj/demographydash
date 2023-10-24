#' Create Population Pyramid Plot
#'
#' @param dt Data table with population data
#' @importFrom ggplot2 aes ggplot geom_bar coord_flip labs theme_minimal theme
#' @importFrom data.table melt
#' @return A ggplot2 object
#' @export
create_pop_pyramid <- function(dt) {
  pop_dt <-
    data.table::melt(
      dt,
      id.vars = "age",
      measure.vars = c("popF", "popM"),
      variable.name = "gender",
      value.name = "population"
    )

  pop_dt$age <- as.factor(pop_dt$age)

  males <- pop_dt[["gender"]] == "popM"
  pop_dt[males, "population"] <- -pop_dt[males, "population"]

  age <- NULL
  population <- NULL
  gender <- NULL

  pop_dt %>%
    ggplot(aes(x = age, y = population, fill = gender)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      title = NULL,
      x = "Age",
      y = "Population(in millions)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom"
    )
}

#' Create Age Group Plot
#'
#' @param dt Data table with population data
#' @importFrom ggplot2 ggplot aes_string geom_line theme_minimal theme
#' @return A ggplot2 object
#' @export
create_age_group_plot <- function(dt, input_scale) {
  y_axis <- ifelse(input_scale == "Percent", "pop_percent", "pop")

  pop_dt <-
    data.table::melt(
      dt,
      id.vars = c("year", "age"),
      measure.vars = c("pop", "pop_percent"),
      variable.name = "type_value",
      value.name = "value"
    )

  pop_dt <- pop_dt[pop_dt$type_value == y_axis, ]

  pop_dt %>%
    ggplot(aes_string("year", "value", color = "age")) +
    geom_line() +
    theme_minimal() +
    theme(
      legend.position = "bottom"
    )
}


#' Create Total Fertility Rate Plot
#'
#' @param dt Data table with fertility rate data
#' @importFrom ggplot2 aes ggplot geom_line labs theme_minimal
#' @return A ggplot2 object
#' @export
create_tfr_plot <- function(dt) {
  year <- NULL
  tfr <- NULL

  dt %>%
    ggplot(aes(x = year, y = tfr)) +
    geom_line(size = 3) +
    labs(
      title = NULL,
      x = "Time",
      y = "Total Fertility Rate"
    ) +
    theme_minimal()
}
#' Prepare Population Age Groups Table
#'
#' @param wpp_dt Data table with population data
#' @importFrom shiny.semantic semantic_DT
#' @return A shiny.semantic DataTable object
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
