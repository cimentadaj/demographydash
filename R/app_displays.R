#' Create Population Pyramid Plot
#'
#' This function takes a data table and an optional input year to create a population pyramid plot.
#'
#' @param dt Data table with population data.
#' @param country The country for which the data is plotted
#' @param input_year The input year to filter the data on, default is NULL.
#'
#' @importFrom ggplot2 aes ggplot geom_bar coord_flip labs theme_minimal theme scale_x_continuous scale_x_discrete scale_y_continuous element_blank element_text
#' @importFrom data.table melt
#' @importFrom plotly ggplotly layout
#' @importFrom scales cut_short_scale label_number
#'
#' @return A ggplot2 object.
#' @export
#'
create_pop_pyramid_plot <- function(dt, country = NULL, input_year = NULL) {
  if ("year" %in% names(dt)) {
    dt <- dt[year == input_year]
  }

  id_vars <- c("age")

  pop_dt <-
    melt(
      dt,
      id.vars = id_vars,
      measure.vars = c("popF", "popM"),
      variable.name = "gender",
      value.name = "population"
    )

  pop_dt$age <- as.factor(pop_dt$age)

  males <- pop_dt[["gender"]] == "popM"
  pop_dt[males, "population"] <- -pop_dt[males, "population"]

  pop_dt$sex <- pop_dt$gender
  pop_dt$gender <- NULL

  pop_dt[sex == "popM", sex := "Males"]
  pop_dt[sex == "popF", sex := "Females"]

  names(pop_dt) <- tools::toTitleCase(names(pop_dt))
  pop_dt <- pop_dt[, c("Population", "Age", "Sex"), with = FALSE]
  pop_dt[["Population"]] <- round(pop_dt[["Population"]], 1)
  names(pop_dt)[1] <- paste(names(pop_dt)[1], "(in thousands)")

  plt_title <- paste0("Population by age and sex: ", country, ", ", input_year)

  ## mid_size <- nchar(plt_title) >= 52 & nchar(plt_title) < 55
  ## mid_big_size <- nchar(plt_title) >= 55 & nchar(plt_title) < 60
  ## big_size <- nchar(plt_title) > 60
  ## font_size <- ifelse(mid_size, 12, ifelse(mid_big_size, 11, ifelse(big_size, 10, 13)))
  font_size <- 10

  plt <-
    pop_dt %>%
    ggplot(aes_string(x = "Age", y = "`Population (in thousands)`", fill = "Sex")) +
    geom_bar(alpha = 0.7, stat = "identity") +
    scale_x_discrete(
      breaks = seq(0, 100, by = 5)
    ) +
    scale_y_continuous(
      labels = label_number(scale_cut = cut_short_scale())
    ) +
    # This second scale_y_continuous is a hack to remove the minus sign
    # from the left part of the X axis. Using scales is too complicated
    # so we just add a new scale and alter the labels formatted by the
    # labels package.
    scale_y_continuous(
      labels = function(x) abs(x)
    ) +
    coord_flip() +
    labs(title = plt_title) +
    theme_minimal(base_size = sz) + # Increase font sizes
    theme(
      plot.title = element_text(size = font_size),
      legend.position = "top",
      panel.grid.major.y = element_blank(), # Remove horizontal grid lines
      panel.grid.major.x = element_blank() # Remove horizontal grid lines
    )

  sex <- NULL
  year <- NULL

  list(
    gg = plt,
    plotly = ggplotly(plt) %>%
      layout(
        legend = list(
          x = 0.5, # Centered horizontally
          y = 1, # At the very top vertically
          xanchor = "center", # Center the legend on the x position
          yanchor = "bottom", # Anchor the legend by its bottom edge
          orientation = "h" # Horizontal orientation
        ),
        margin = list(t = 100) # Increase top margin to make space for the legend
      )
  )
}

#' Create Age Group Plot
#'
#' This function takes a data table and an input scale to create an age group plot.
#'
#' @param dt Data table with population data.
#' @param input_scale The type of scale to be applied.
#' @param country A string with the current country name for the title.
#'
#' @importFrom ggplot2 ggplot aes_string geom_line theme_minimal theme
#' @importFrom data.table melt
#' @importFrom plotly ggplotly layout
#' @importFrom rlang sym !!
#'
#' @return A ggplot2 object.
#' @export
#'
create_age_group_plot <- function(dt, input_scale, country) {
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
  axis_label <- ifelse(input_scale == "Percent", "Percent", "in thousands")

  type_pop <- paste0("Population (", axis_label, ")")
  names(pop_dt) <- c("Year", "Age", "Type", type_pop)

  if (input_scale != "Percent") {
    pop_dt[[type_pop]] <- round(pop_dt[[type_pop]], 1)
  }

  min_year <- min(pop_dt$Year)
  max_year <- max(pop_dt$Year)

  plt_title <- paste0("Population by broad age groups: ", country, ", ", min_year, "-", max_year)

  plt <-
    pop_dt %>%
    ggplot(aes(Year, !!sym(type_pop), color = Age)) +
    geom_line() +
    labs(title = plt_title, color = "Age Group") +
    theme_minimal(base_size = sz) +
    theme(
      legend.position = "bottom"
    )

  if (input_scale == "Percent") {
    plt <- plt + scale_y_continuous(labels = scales::label_percent(scale = 1))
  } else {
    plt <- plt + scale_y_continuous(labels = label_number(big.mark = ""))
  }

  Year <- NULL
  Age <- NULL

  list(gg = plt, plotly = ggplotly(plt))
}


#' Create Pop Age Group Time Plot
#'
#' This function takes a data table and an input of age group to create a time age group plot.
#'
#' @param dt Data table with population data.
#' @param input_age The age group to subset
#' @param country A string with the current country name for the title.
#'
#' @importFrom ggplot2 ggplot aes_string geom_line theme_minimal theme geom_ribbon scale_color_manual geom_rect expansion scale_fill_manual
#' @importFrom data.table melt
#' @importFrom plotly ggplotly layout
#'
#' @return A ggplot2 object.
#' @export
#'
create_pop_time_plot <- function(dt, input_age, country) {
  pop_dt <-
    melt(
      dt,
      id.vars = c("year", "age", "un_pop_95low", "un_pop_95high"),
      measure.vars = c("pop", "un_pop_median"),
      variable.name = "type_value",
      value.name = "value"
    )

  pop_dt <- pop_dt[pop_dt$age == input_age, ]
  pop_dt[type_value == "pop", type_value := "Projection"]
  pop_dt[type_value == "un_pop_median", type_value := "UN Projection"]

  names(pop_dt) <- c(
    "Year",
    "Age",
    "un_pop_95low",
    "un_pop_95high",
    "Type",
    "Population (in thousands)"
  )

  pop_dt[["Population (in thousands)"]] <- round(pop_dt[["Population (in thousands)"]], 1)

  num_cols <- names(pop_dt)[sapply(pop_dt, is.numeric)]
  num_cols <- num_cols[num_cols != "Year"]
  res <- pop_dt[, num_cols, with = FALSE]

  min_y <- min(sapply(res, min, na.rm = TRUE))
  min_y <- min_y - (min_y * 0.05)
  max_y <- max(sapply(res, max, na.rm = TRUE))
  max_y <- max_y + (max_y * 0.05)
  max_year <- max(pop_dt$Year)

  min_year <- min(pop_dt$Year)
  plt_title <- paste0("Population Age '", input_age, "': ",  country, ", ", min_year, "-", max_year)

  plt <-
    pop_dt %>%
    ggplot(aes(Year, `Population (in thousands)`, color = Type, , fill = Type, group = Type)) +
    geom_line() +
    geom_ribbon(
      data = pop_dt[Type == "UN Projection"],
      aes(ymin = un_pop_95low, ymax = un_pop_95high, color = "95% UN PI", fill = "95% UN PI"),
      alpha = 0.2
    ) +
    scale_color_manual(
      values = c("Projection" = "#F8766D", "UN Projection" = "#00BFC4", "95% UN PI" = "#00BFC4")
    ) +
    scale_fill_manual(
      values = c("Projection" = "#F8766D", "UN Projection" = "#00BFC4", "95% UN PI" = "#00BFC4")
    ) +
    scale_y_continuous(
      limits = c(min_y, max_y),
      expand = expansion(mult = 0),
      labels = label_number(big.mark = "")
    ) +
    labs(title = plt_title) +
    theme_minimal(base_size = sz) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 13)
    )

  Year <- NULL
  Type <- NULL
  Population <- NULL
  Type <- NULL
  type_value <- NULL
  un_pop_95low <- NULL
  un_pop_95high <- NULL
  `Population (in thousands)` <- NULL

  list(gg = plt, plotly = ggplotly(plt, tooltip = c("x", "y", "group")))
}

#' Create forecasted TFR plot
#'
#' This function takes a data table to create a projected TFR plot.
#'
#' @param dt Data table with population data.
#' @param end_year the date in YYYY-MM-DD where the projection should end.
#' @param country A string with the current country name for the title.
#'
#' @importFrom ggplot2 ggplot aes_string geom_line theme_minimal theme geom_ribbon scale_y_continuous scale_color_manual
#' @importFrom data.table melt
#' @importFrom plotly ggplotly layout
#'
#' @return A ggplot2 object.
#' @export
#'
create_tfr_projected_plot <- function(dt, end_year, country) {
  tfr_dt <-
    melt(
      dt,
      id.vars = c("year", "un_tfr_95low", "un_tfr_95high"),
      measure.vars = c("tfr", "un_tfr_median"),
      variable.name = "type_value",
      value.name = "value"
    )

  tfr_dt <- tfr_dt[tfr_dt$year <= as.numeric(end_year), ]
  tfr_dt[type_value == "tfr", type_value := "Projection"]
  tfr_dt[type_value == "un_tfr_median", type_value := "UN Projection"]
  names(tfr_dt) <- c("Year", "un_tfr_95low", "un_tfr_95high", "Type", "TFR")

  max_year <- max(tfr_dt$Year)
  min_year <- min(tfr_dt$Year)
  plt_title <- paste0("Total Fertility Rate: ", country, ", ", min_year, "-", max_year)

  plt <-
    ggplot(tfr_dt, aes(Year, TFR, group = Type, color = Type, fill = Type)) +
    geom_line() +
    geom_ribbon(
      data = tfr_dt[Type == "UN Projection"],
      aes(ymin = un_tfr_95low, ymax = un_tfr_95high, color = "95% UN PI", fill = "95% UN PI"), alpha = 0.2
    ) +
    scale_color_manual(
      values = c("Projection" = "#F8766D", "UN Projection" = "#00BFC4", "95% UN PI" = "#00BFC4")
    ) +
    scale_fill_manual(
      values = c("Projection" = "#F8766D", "UN Projection" = "#00BFC4", "95% UN PI" = "#00BFC4")
    ) +
    labs(title = plt_title) +
    theme_minimal(base_size = sz) +
    theme(
      legend.position = "bottom"
    )

  Year <- NULL
  TFR <- NULL
  Type <- NULL
  un_tfr_95low <- NULL
  un_tfr_95high <- NULL
  type_value <- NULL

  list(gg = plt, plotly = ggplotly(plt, tooltip = c("x", "y", "group")))
}

#' Create Annual Growth Rate Time Plot by Age
#'
#' This function takes a data table to create an annual growth rate time plot by age.
#'
#' @param dt Data table with annual growth data.
#' @param end_year the date in YYYY-MM-DD where the projection should end.
#' @param country A string with the current country name for the title.
#'
#' @importFrom ggplot2 ggplot aes_string geom_line theme_minimal theme geom_ribbon scale_color_manual
#' @importFrom data.table melt
#' @importFrom plotly ggplotly layout
#'
#' @return A ggplot2 object.
#' @export
#'
create_annual_growth_plot <- function(dt, end_year, country) {
  dt <- dt[dt$year <= as.numeric(end_year), ]
  dt$value <- dt$growth_rate
  dt$growth_rate <- NULL
  dt$type_value <- dt$age
  dt$age <- NULL

  names(dt) <- c("Year", "Population Growth Rate", "Age")

  max_year <- max(dt$Year)
  min_year <- min(dt$Year)
  plt_title <- paste0("Population growth rates for broad age groups: ", country, ", ", min_year, "-", max_year)

  plt <-
    dt %>%
    ggplot(aes(Year, `Population Growth Rate`, color = Age, group = Age)) +
    geom_line() +
    labs(title = plt_title) +
    theme_minimal(base_size = sz) +
    theme(
      legend.position = "bottom"
    )

  Year <- NULL
  `Population Growth Rate` <- NULL
  Age <- NULL

  list(gg = plt, plotly = ggplotly(plt, tooltip = c("x", "y", "color")))
}


#' Create Total Fertility Rate Plot
#'
#' This function takes a data table to create a total fertility rate plot.
#'
#' @param dt Data table with fertility rate data.
#' @param end_year the date in YYYY-MM-DD where the projection should end.
#' @param country The country for which the data is plotted
#'
#' @importFrom ggplot2 aes ggplot geom_line labs theme_minimal scale_color_manual
#' @importFrom plotly ggplotly
#'
#' @return A ggplot2 object.
#' @export
#'
create_tfr_plot <- function(dt, end_year, country) {
  Year <- NULL
  TFR <- NULL

  names(dt) <- c("Year", "TFR")

  dt <- dt[Year <= end_year]

  max_year <- max(dt$Year)
  min_year <- min(dt$Year)
  plt_title <- paste0("Total Fertility Rate: ", country, ", ", min_year, "-", max_year)

  plt <-
    dt %>%
    ggplot(aes(x = Year, y = TFR)) +
    geom_line(size = 2, alpha = 0.7) +
    labs(
      title = plt_title,
    ) +
    theme_minimal(base_size = sz)

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
#'
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
  len_pop <- length(summary_table$population)
  total_population <- sum(summary_table$population[-len_pop])
  summary_table$percentage <- round(c(summary_table$population[1:(nrow(summary_table) - 1)] / total_population * 100, 100), 0)

  summary_table$population <- NULL
  names(summary_table) <- c("Age groups", "Population", "Percentage")
  row.names(summary_table) <- NULL
  summary_table$Percentage <- paste0(summary_table$Percentage, "%")

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
#' @param country The country for which the data is plotted
#'
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot geom_line geom_ribbon labs theme_minimal scale_color_manual
#' @importFrom plotly ggplotly
#' @importFrom data.table setnames .SD :=
#'
#' @export
#'
create_deaths_births_plot <- function(forecast_birth, forecast_death, data_type, value_type, end_year, country) {
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

  # Ensure that median and PI columns are numeric
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

  melt_data$type_value <- ifelse(grepl("un_", melt_data$type_value), "UN Projection", "Projection")

  var_name <- ifelse(
    value_type == "counts",
    paste0("Number of ", tolower(data_type), "s (thousands)"),
    paste0(tools::toTitleCase(data_type), "s per 1,000 population")
  )

  names(melt_data) <- c("Year", "low", "high", "Type", var_name)

  max_year <- max(melt_data$Year)
  min_year <- min(melt_data$Year)

  # Determine the type of data (Births/Deaths)
  data_type_title <- ifelse(data_type == "birth", "Birth", "Death")

  # Construct the title based on value_type
  plt_title <- ifelse(
    value_type == "rates",
    paste0("Crude ", data_type_title, " Rate: ", country, ", ", min_year, "-", max_year),
    paste0("Annual number of ", tolower(data_type_title), "s: ", country, ", ", min_year, "-", max_year)
  )

  melt_data[[var_name]] <- round(melt_data[[var_name]], 1)

  # Plot the data
  plt <-
    ggplot(
      melt_data,
      aes(x = Year, y = !!sym(var_name), group = Type, color = Type, fill = Type)
    ) +
    geom_line() +
    geom_ribbon(
      data = melt_data[Type == "UN Projection"],
      aes(ymin = low, ymax = high, color = "95% UN PI", fill = "95% UN PI"), alpha = 0.2
    ) +
    scale_color_manual(
      values = c("Projection" = "#F8766D", "UN Projection" = "#00BFC4", "95% UN PI" = "#00BFC4")
    ) +
    scale_fill_manual(
      values = c("Projection" = "#F8766D", "UN Projection" = "#00BFC4", "95% UN PI" = "#00BFC4")
    ) +
    labs(
      title = plt_title,
    ) +
    theme_minimal(base_size = sz) +
    theme(
      legend.position = "bottom"
    )

  if (value_type == "counts") {
    plt <- plt + scale_y_continuous(labels = label_number(big.mark = ""))
  }

  Year <- NULL
  Type <- NULL
  year <- NULL
  low <- NULL
  high <- NULL

  list(
    gg = plt,
    plotly = ggplotly(plt, tooltip = c("x", "y", "color"))
  )
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
#' @param country The country for which the data is plotted
#'
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot geom_line geom_ribbon labs theme_minimal scale_color_manual
#' @importFrom plotly ggplotly
#'
#' @export
#'
create_yadr_oadr_plot <- function(oadr, yadr, data_type, end_year, country) {
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

  melt_data$type_value <- ifelse(grepl("un_", melt_data$type_value), "UN Projection", "Projection")


  var_name <- ifelse(
    data_type == "yadr",
    "Persons age <20 per 100 persons age 20-64",
    "Persons age 65+ per 100 persons age 20-64"
  )

  names(melt_data) <- c("Year", "low", "high", "Type", var_name)

  max_year <- max(melt_data$Year)
  min_year <- min(melt_data$Year)

  title_type <- ifelse(
    data_type == "yadr",
    "Young-age dependency ratio (Age <20 / Age 20-64)",
    "Old-age dependency ratio (Age 65+ / Age 20-64)"
  )

  plt_title <- paste0(title_type, ": ", country, ", ", min_year, "-", max_year)

  # Plot the data
  plt <-
    ggplot(
      melt_data,
      aes(x = Year, y = !!sym(var_name), group = Type, color = Type, fill = Type)
    ) +
    geom_line() +
    geom_ribbon(
      data = melt_data[Type == "UN Projection"],
      aes(ymin = low, ymax = high, color = "95% UN PI", fill = "95% UN PI"), alpha = 0.2
    ) +
    scale_color_manual(
      values = c("Projection" = "#F8766D", "UN Projection" = "#00BFC4", "95% UN PI" = "#00BFC4")
    ) +
    scale_fill_manual(
      values = c("Projection" = "#F8766D", "UN Projection" = "#00BFC4", "95% UN PI" = "#00BFC4")
    ) +
    labs(
      title = plt_title,
    ) +
    theme_minimal(base_size = sz) +
    theme(
      legend.position = "bottom"
    )

  year <- NULL
  Year <- NULL
  value <- NULL
  Type <- NULL
  low <- NULL
  high <- NULL
  data_type <- NULL

  list(
    gg = plt,
    plotly = ggplotly(plt, tooltip = c("x", "y", "color"))
  )
}

#' Create an Interactive Plot Based on Aging and Size Data
#'
#' This function takes a dataset and an end year to plot two sets of data up to
#' the specified year. One set is identified by the presence of "un" in the column
#' names, representing UN data, and the other set represents projection data.
#' The plot is interactive, with hover functionality to display data values.
#'
#' @param dt A data frame with a 'year' column and other numeric columns where
#'   some column names contain "un", indicating UN data. The rest are considered
#'   projection data. The function dynamically identifies these columns and does
#'   not assume specific names, making it applicable for various data types.
#' @param end_year A numeric value specifying the last year to include in the plot.
#' @param name_mappings A named vector where each name corresponds to a column in `dt` that should be renamed, and the value is the new name to be used in the plot. It should also contain a 'title' element for the plot's main title.
#' @param percent_x A boolean to whether include a percent label on the X axis
#'
#' @return A list containing a ggplot object and an interactive plotly object.
#'
#' @importFrom ggplot2 aes_string ggplot geom_point labs theme_minimal
#' @importFrom plotly ggplotly
#' @importFrom data.table data.table setnames
#' @export
#'
create_un_projection_plot <- function(dt, end_year, name_mappings, percent_x = FALSE) {
  # Extract the title from the name_mappings and separate it from column mappings
  plot_title <- name_mappings[["title"]]
  col_mappings <- name_mappings[names(name_mappings) != "title"]

  # Filter the data up to the specified end year
  data <- dt[year <= end_year]
  non_year <- setdiff(names(data), "year")
  data <- data[, (non_year) := lapply(.SD, as.numeric), .SDcols = non_year]

  # Update column names for UN and projection data after renaming
  un_colnames <- grep("un", names(data), value = TRUE)
  un_colnames <- sort(un_colnames)

  proj_colnames <- setdiff(names(data), c("year", un_colnames))
  proj_colnames <- sort(proj_colnames)

  # Prepare the data sets with type labels and combine them
  proj_data <- data[, c("year", proj_colnames), with = FALSE]
  proj_data$Type <- "Projection"

  un_data <- data[, c("year", un_colnames), with = FALSE]
  un_data$Type <- "UN Projection"
  names(un_data) <- names(proj_data) # Standardize column names for merging

  combined_data <- rbind(proj_data, un_data)

  # Rename the columns based on col_mappings
  for (old_name in names(col_mappings)) {
    setnames(combined_data, old_name, col_mappings[[old_name]])
  }

  # Add text for ggplotly to display on hover
  combined_data$text <- paste("Year:", combined_data$year)

  if (names(combined_data)[3] == "Population") {
    names(combined_data)[3] <- "Population (in thousands)"
  }

  # Create the ggplot object with the mappings
  plt <-
    ggplot(
      combined_data,
      aes(
        x = !!sym(names(combined_data)[2]),
        y = !!sym(names(combined_data)[3]),
        color = Type,
        group = Type,
        text = text
      ),
    ) +
    geom_point() +
    labs(title = plot_title) +
    theme_minimal(base_size = sz) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 13)
    )

  if (percent_x) {
    plt <- plt + scale_x_continuous(labels = scales::label_percent(scale = 1))
  }

  if (grepl("Population", names(combined_data)[3])) {
    plt <- plt + scale_y_continuous(labels = label_number(big.mark = ""))
  }

  Type <- NULL
  year <- NULL
  text <- NULL

  # Return a list containing both the ggplot and ggplotly objects
  list(gg = plt, plotly = ggplotly(plt, tooltip = c("x", "y", "color", "text")))
}
