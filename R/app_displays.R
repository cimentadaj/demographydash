color_labels <- function() {
  # Define the labels and corresponding colors
  labels_colors <- scales::hue_pal()(6)
  labels_colors <- stats::setNames(
    labels_colors,
    c("0-19", "20-39", "40-59", "60+", "65+", "Total")
  )
  labels_colors
}

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
#' @importFrom plotly ggplotly layout config
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
  pop_dt$sex <- pop_dt$gender
  pop_dt$gender <- NULL

  pop_dt[sex == "popM", sex := "Males"]
  pop_dt[sex == "popF", sex := "Females"]

  names(pop_dt) <- tools::toTitleCase(names(pop_dt))
  pop_dt <- pop_dt[, c("Population", "Age", "Sex"), with = FALSE]
  pop_dt[["Population"]] <- round(pop_dt[["Population"]], 3)
  names(pop_dt)[1] <- paste0(names(pop_dt)[1], " (000s)")

  plt_title <- paste0("Population by age and sex: ", country, ", ", input_year)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  tmp_dt <- as.data.frame(pop_dt)
  males <- tmp_dt[["Sex"]] == "Males"
  tmp_dt[males, "Population (000s)"] <- -tmp_dt[males, "Population (000s)"]

  plt <-
    ggplot(pop_dt, aes_string(x = "Age", y = "`Population (000s)`", fill = "Sex")) +
    geom_blank() +
    geom_bar(data = tmp_dt, alpha = 0.7, stat = "identity") +
    scale_x_discrete(breaks = seq(0, 100, by = 5)) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    coord_flip() +
    labs(title = plt_title) +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) + # Increase font sizes
    theme(
      plot.title = element_text(size = DOWNLOAD_PLOT_SIZE$title),
      legend.position = "top",
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank()
    )

  sex <- NULL
  year <- NULL

  # This is the visible plot so we vary the font sizes depending on screen resolution
  plt_visible <-
    plt +
    theme_minimal(base_size = PLOTLY_TEXT_SIZE$font) +
    labs(title = plt_title_adapted$title) +
    theme(
      plot.title = element_text(size = plt_title_adapted$font_size),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank()
    )

  plt_visible <- ggplotly(plt_visible) %>% layout(legend = PLOTLY_LEGEND_OPTS)

  list(
    gg = plt,
    plotly = config(
      plt_visible,
      displayModeBar = FALSE
    )
  )
}


#' Create e0 Plot
#'
#' This function takes a data table to create a total life expectancy plot.
#'
#' @param dt Data table with life expectancy data.
#' @param end_year the date in YYYY-MM-DD where the projection should end.
#' @param country The country for which the data is plotted
#'
#' @importFrom ggplot2 aes ggplot geom_line labs theme_minimal scale_color_manual
#' @importFrom plotly ggplotly
#' @importFrom data.table as.data.table
#'
#' @return A ggplot2 object.
#' @export
#'
create_e0_plot <- function(dt, end_year, country) {
  Year <- e0M <- e0F <- value <- Sex <- NULL  # To avoid R CMD check notes

  # Ensure the data is in the correct format
  dt <- as.data.table(dt)
  setnames(dt, c("year", "e0M", "e0F"), c("Year", "e0M", "e0F"))

  # Filter data up to end_year
  dt <- dt[Year <= end_year]

  # Pivot the data to long format
  dt_long <- melt(dt, id.vars = "Year",
                  measure.vars = c("e0M", "e0F"),
                  variable.name = "Sex", value.name = "LifeExpectancy")

  # Update Sex labels
  dt_long[, Sex := ifelse(Sex == "e0M", "Male", "Female")]

  # Round the life expectancy values
  dt_long[, LifeExpectancy := round(LifeExpectancy, 3)]

  # Get min and max years for the title
  max_year <- max(dt$Year)
  min_year <- min(dt$Year)

  # Create plot title
  plt_title <- paste0("Life Expectancy: ", country, ", ", min_year, "-", max_year)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  # Create ggplot
  plt <- ggplot(dt_long, aes(x = Year, y = LifeExpectancy, color = Sex)) +
    geom_line(size = 2, alpha = 0.7) +
    labs(
      title = plt_title,
      y = "Life Expectancy (years)",
      color = "Sex"
    ) +

    scale_color_manual(values = c("Male" = "#F8766D", "Female" = "#00BFC4")) +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) +
    theme(
      plot.title = element_text(size = DOWNLOAD_PLOT_SIZE$title),
      legend.position = "bottom"
    )

  # Create plotly version
  plt_visible <- plt +
    theme_minimal(base_size = PLOTLY_TEXT_SIZE$font) +
    labs(title = plt_title_adapted$title) +
    theme(
      plot.title = element_text(size = plt_title_adapted$font_size),
      legend.position = "bottom"
    )

  plt_visible <- ggplotly(plt_visible)

  # Return both ggplot and plotly versions
  list(
    gg = plt,
    plotly = config(plt_visible, displayModeBar = FALSE)
  )
}


#' Create Projected e0 Plot
#'
#' This function takes a data table to create a total life expectancy plot from
#' the model's prediction.
#'
#' @param dt Data table with projected life expectancy data.
#' @param end_year the date in YYYY-MM-DD where the projection should end.
#' @param country The country for which the data is plotted
#'
#' @importFrom ggplot2 aes ggplot geom_line labs theme_minimal scale_color_manual
#' @importFrom plotly ggplotly
#'
#' @return A ggplot2 object.
#' @export
#'
create_e0_projected_plot <- function(dt, input_sex, country) {

  # Update Sex labels
  dt[, sex := fcase(
    sex == "M", "Male",
    sex == "F", "Female",
    sex == "B", "Total",
    default = sex
  )]

  e0_dt <- melt(
    dt,
    id.vars = c("year", "sex", "un_e0_95low", "un_e0_95high"),
    measure.vars = c("e0", "un_e0_median"),
    variable.name = "type_value",
    value.name = "value"
  )

  e0_dt <- e0_dt[e0_dt$sex == input_sex, ]

  e0_dt[type_value == "e0", type_value := "Projection"]
  e0_dt[type_value == "un_e0_median", type_value := "UN Projection"]

  names(e0_dt) <- c(
    "Year",
    "Sex",
    "95% Lower bound PI",
    "95% Upper bound PI",
    "Type",
    "Life Expectancy"
  )

  columns_to_round <- c("Life Expectancy", "95% Lower bound PI", "95% Upper bound PI")

  e0_dt[, (columns_to_round) := lapply(.SD, round, 3), .SDcols = columns_to_round]

  num_cols <- names(e0_dt)[sapply(e0_dt, is.numeric)]
  num_cols <- num_cols[num_cols != "Year"]
  res <- e0_dt[, num_cols, with = FALSE]
  min_y <- min(sapply(res, min, na.rm = TRUE))
  min_y <- min_y - (min_y * 0.05)
  max_y <- max(sapply(res, max, na.rm = TRUE))
  max_y <- max_y + (max_y * 0.05)

  max_year <- max(e0_dt$Year)
  min_year <- min(e0_dt$Year)
  plt_title <- paste0("Life Expectancy '", input_sex, "': ", country, ", ", min_year, "-", max_year)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  plt <-
    e0_dt %>%
    ggplot(aes(Year, `Life Expectancy`, color = Type, fill = Type, group = Type)) +
    geom_line(aes(linetype = Type)) +
    geom_ribbon(
      data = e0_dt[Type == "UN Projection"],
      aes(
        y = NULL,
        ymin = .data[["95% Lower bound PI"]],
        ymax = .data[["95% Upper bound PI"]],
        color = "95% UN PI",
        fill = "95% UN PI",
        linetype = "95% UN PI"
      ),
      alpha = 0.2
    ) +
    scale_color_manual(
      values = c("Projection" = "#F8766D", "UN Projection" = "#00BFC4", "95% UN PI" = "#00BFC4")
    ) +
    scale_fill_manual(
      values = c("Projection" = "#F8766D", "UN Projection" = "#00BFC4", "95% UN PI" = "#00BFC4")
    ) +
    scale_linetype_manual(
      values = c("Projection" = "dashed", "UN Projection" = "solid", "95% UN PI" = "solid")
    ) +
    scale_y_continuous(
      limits = c(min_y, max_y),
      expand = expansion(mult = 0),
      labels = label_number(big.mark = "")
    ) +
    labs(title = plt_title, y = "Life Expectancy (years)") +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) +
    theme(
      plot.title = element_text(size = DOWNLOAD_PLOT_SIZE$title),
      legend.position = "bottom"
    )

  # This is the visible plot so we vary the font sizes depending on screen resolution
  plt_visible <-
    plt +
    theme_minimal(base_size = PLOTLY_TEXT_SIZE$font) +
    labs(title = plt_title_adapted$title) +
    theme(
      plot.title = element_text(size = plt_title_adapted$font_size)
    )

  plt_visible <-
    ggplotly(plt_visible, tooltip = c("x", "y", "group", "ymax", "ymin")) %>%
    layout(legend = PLOTLY_LEGEND_OPTS)

  list(
    gg = plt,
    plotly = config(plt_visible, displayModeBar = FALSE)
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
  axis_label <- ifelse(input_scale == "Percent", "(Percent)", " (000s)")

  type_pop <- paste0("Population ", axis_label)
  names(pop_dt) <- c("Year", "Age", "Type", type_pop)

  pop_dt[[type_pop]] <- round(pop_dt[[type_pop]], 3)

  min_year <- min(pop_dt$Year)
  max_year <- max(pop_dt$Year)

  plt_title <- paste0("Population by broad age groups: ", country, ", ", min_year, "-", max_year)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  plt <-
    pop_dt %>%
    ggplot(aes(Year, !!sym(type_pop), color = Age)) +
    geom_line() +
    scale_color_manual(values = color_labels()) +
    labs(
      title = plt_title,
      color = "Age Group"
    ) +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) + # Increase font sizes
    theme(
      plot.title = element_text(size = DOWNLOAD_PLOT_SIZE$title),
      legend.position = "bottom"
    )

  if (input_scale == "Percent") {
    plt <- plt + scale_y_continuous(labels = scales::label_percent(scale = 1))
  } else {
    plt <- plt + scale_y_continuous(labels = label_number(big.mark = ""))
  }

  Year <- NULL
  Age <- NULL

  # This is the visible plot so we vary the font sizes depending on screen resolution
  plt_visible <-
    plt +
    theme_minimal(base_size = PLOTLY_TEXT_SIZE$font) +
    labs(title = plt_title_adapted$title) +
    theme(
      plot.title = element_text(size = plt_title_adapted$font_size)
    )

  plt_visible <- ggplotly(plt_visible) %>% layout(legend = PLOTLY_LEGEND_OPTS)

  list(
    gg = plt,
    plotly = config(plt_visible, displayModeBar = FALSE)
  )
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
    "95% Lower bound PI",
    "95% Upper bound PI",
    "Type",
    "Population (000s)"
  )

  columns_to_round <- c("Population (000s)", "95% Lower bound PI", "95% Upper bound PI")
  pop_dt[, (columns_to_round) := lapply(.SD, round, 3), .SDcols = columns_to_round]

  num_cols <- names(pop_dt)[sapply(pop_dt, is.numeric)]
  num_cols <- num_cols[num_cols != "Year"]
  res <- pop_dt[, num_cols, with = FALSE]

  min_y <- min(sapply(res, min, na.rm = TRUE))
  min_y <- min_y - (min_y * 0.05)
  max_y <- max(sapply(res, max, na.rm = TRUE))
  max_y <- max_y + (max_y * 0.05)
  max_year <- max(pop_dt$Year)

  min_year <- min(pop_dt$Year)

  plt_title <- paste0("Population Age '", input_age, "': ", country, ", ", min_year, "-", max_year)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  plt <-
    pop_dt %>%
    ggplot(aes(Year, `Population (000s)`, color = Type, , fill = Type, group = Type)) +
    geom_line(aes(linetype = Type)) +
    geom_ribbon(
      data = pop_dt[Type == "UN Projection"],
      aes(
        y = NULL,
        ymin = .data[["95% Lower bound PI"]],
        ymax = .data[["95% Upper bound PI"]],
        color = "95% UN PI",
        fill = "95% UN PI",
        linetype = "95% UN PI"
      ),
      alpha = 0.2
    ) +
    scale_color_manual(
      values = c("Projection" = "#F8766D", "UN Projection" = "#00BFC4", "95% UN PI" = "#00BFC4")
    ) +
    scale_fill_manual(
      values = c("Projection" = "#F8766D", "UN Projection" = "#00BFC4", "95% UN PI" = "#00BFC4")
    ) +
    scale_linetype_manual(
      values = c("Projection" = "dashed", "UN Projection" = "solid", "95% UN PI" = "solid")
    ) +
    scale_y_continuous(
      limits = c(min_y, max_y),
      expand = expansion(mult = 0),
      labels = label_number(big.mark = "")
    ) +
    labs(title = plt_title) +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) + # Increase font sizes
    theme(
      plot.title = element_text(size = DOWNLOAD_PLOT_SIZE$title),
      legend.position = "bottom"
    )

  Year <- NULL
  Type <- NULL
  Population <- NULL
  Type <- NULL
  type_value <- NULL
  un_pop_95low <- NULL
  un_pop_95high <- NULL
  `Population per 1000 persons` <- NULL
  `Population (000s)` <- NULL

  # This is the visible plot so we vary the font sizes depending on screen resolution
  plt_visible <-
    plt +
    theme_minimal(base_size = PLOTLY_TEXT_SIZE$font) +
    labs(title = plt_title_adapted$title) +
    theme(
      plot.title = element_text(size = plt_title_adapted$font_size)
    )

  plt_visible <- ggplotly(plt_visible, tooltip = c("x", "y", "group", "ymax", "ymin")) %>% layout(legend = PLOTLY_LEGEND_OPTS)

  list(
    gg = plt,
    plotly = config(plt_visible, displayModeBar = FALSE)
  )
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
  names(tfr_dt) <- c("Year", "95% Lower bound PI", "95% Upper bound PI", "Type", "Births per woman")

  max_year <- max(tfr_dt$Year)
  min_year <- min(tfr_dt$Year)

  plt_title <- paste0("Total Fertility Rate: ", country, ", ", min_year, "-", max_year)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  columns_to_round <- c("Births per woman", "95% Lower bound PI", "95% Upper bound PI")
  tfr_dt[, (columns_to_round) := lapply(.SD, round, 3), .SDcols = columns_to_round]

  plt <-
    ggplot(tfr_dt, aes(Year, `Births per woman`, group = Type, color = Type, fill = Type)) +
    geom_line(aes(linetype = Type)) +
    geom_ribbon(
      data = tfr_dt[Type == "UN Projection"],
      aes(
        y = NULL,
        ymin = .data[["95% Lower bound PI"]],
        ymax = .data[["95% Upper bound PI"]],
        color = "95% UN PI",
        fill = "95% UN PI",
        linetype = "95% UN PI"
      ),
      alpha = 0.2
    ) +
    scale_color_manual(
      values = c("Projection" = "#F8766D", "UN Projection" = "#00BFC4", "95% UN PI" = "#00BFC4")
    ) +
    scale_fill_manual(
      values = c("Projection" = "#F8766D", "UN Projection" = "#00BFC4", "95% UN PI" = "#00BFC4")
    ) +
    scale_linetype_manual(
      values = c("Projection" = "dashed", "UN Projection" = "solid", "95% UN PI" = "solid")
    ) +
    labs(title = plt_title) +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) + # Increase font sizes
    theme(
      plot.title = element_text(size = DOWNLOAD_PLOT_SIZE$title),
      legend.position = "bottom"
    )

  Year <- NULL
  TFR <- NULL
  Type <- NULL
  un_tfr_95low <- NULL
  un_tfr_95high <- NULL
  type_value <- NULL
  `Births per woman` <- NULL

  # This is the visible plot so we vary the font sizes depending on screen resolution
  plt_visible <-
    plt +
    theme_minimal(base_size = PLOTLY_TEXT_SIZE$font) +
    labs(title = plt_title_adapted$title) +
    theme(
      plot.title = element_text(size = plt_title_adapted$font_size),
      legend.position = "bottom"
    )


  plt_visible <- ggplotly(plt_visible, tooltip = c("x", "y", "group", "ymax", "ymin")) %>% layout(legend = PLOTLY_LEGEND_OPTS)

  list(
    gg = plt,
    plotly = config(plt_visible, displayModeBar = FALSE)
  )
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

  columns_to_round <- c("Population Growth Rate")
  dt[, (columns_to_round) := lapply(.SD, round, 3), .SDcols = columns_to_round]

  plt_title <- paste0("Population growth rates for broad age groups: ", country, ", ", min_year, "-", max_year)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  plt <-
    dt %>%
    ggplot(aes(Year, `Population Growth Rate`, color = Age, group = Age)) +
    geom_line() +
    scale_color_manual(values = color_labels()) +
    labs(title = plt_title) +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) + # Increase font sizes
    theme(
      plot.title = element_text(size = DOWNLOAD_PLOT_SIZE$title),
      legend.position = "bottom"
    )

  Year <- NULL
  `Population Growth Rate` <- NULL
  Age <- NULL

  # This is the visible plot so we vary the font sizes depending on screen resolution
  plt_visible <-
    plt +
    theme_minimal(base_size = PLOTLY_TEXT_SIZE$font) +
    labs(title = plt_title_adapted$title) +
    theme(
      plot.title = element_text(size = plt_title_adapted$font_size),
      legend.position = "bottom"
    )

  plt_visible <- ggplotly(plt_visible, tooltip = c("x", "y", "color")) %>% layout(legend = PLOTLY_LEGEND_OPTS)

  list(
    gg = plt,
    plotly = config(plt_visible, displayModeBar = FALSE)
  )
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

  columns_to_round <- c("TFR")
  dt[, (columns_to_round) := lapply(.SD, round, 3), .SDcols = columns_to_round]

  plt_title <- paste0("Total Fertility Rate: ", country, ", ", min_year, "-", max_year)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  plt <-
    dt %>%
    ggplot(aes(x = Year, y = TFR)) +
    geom_line(size = 2, alpha = 0.7) +
    labs(
      title = plt_title,
      y = "Total Fertility Rate"
    ) +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) + # Increase font sizes
    theme(
      plot.title = element_text(size = DOWNLOAD_PLOT_SIZE$title)
    )

  plt_visible <-
    plt +
    theme_minimal(base_size = PLOTLY_TEXT_SIZE$font) +
    labs(title = plt_title_adapted$title) +
    theme(
      plot.title = element_text(size = plt_title_adapted$font_size)
    )

  plt_visible <- ggplotly(plt_visible)

  list(
    gg = plt,
    plotly = config(plt_visible, displayModeBar = FALSE)
  )
}

#' Prepare Population Age Groups Table
#'
#' This function takes a data table to prepare a population age groups table.
#'
#' @param wpp_dt Data table with population data.
#'
#' @importFrom DT datatable
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

  DT::datatable(
    summary_table,
    options = list(
      responsive = TRUE,
      paging = FALSE, # Disable pagination
      searching = FALSE, # Disable searching
      info = FALSE, # Disable info like "Showing 1 of N"
      columnDefs = list(
        list(orderable = FALSE, targets = "_all")
      )
    )
  )
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
#' @importFrom ggplot2 ggplot geom_line geom_ribbon labs theme_minimal scale_color_manual scale_linetype_manual geom_blank .data
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

  names(melt_data) <- c("Year", "95% Lower bound PI", "95% Upper bound PI", "Type", var_name)

  columns_to_round <- c(var_name, "95% Lower bound PI", "95% Upper bound PI")
  melt_data[, (columns_to_round) := lapply(.SD, round, 3), .SDcols = columns_to_round]

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

  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  # Plot the data
  plt <-
    ggplot(
      melt_data,
      aes(x = Year, y = !!sym(var_name), group = Type, color = Type, fill = Type)
    ) +
    geom_line(aes(linetype = Type)) +
    geom_ribbon(
      data = melt_data[Type == "UN Projection"],
      aes(
        y = NULL,
        ymin = .data[["95% Lower bound PI"]],
        ymax = .data[["95% Upper bound PI"]],
        color = "95% UN PI",
        fill = "95% UN PI",
        linetype = "95% UN PI"
      ),
      alpha = 0.2
    ) +
    scale_color_manual(
      values = c("Projection" = "#F8766D", "UN Projection" = "#00BFC4", "95% UN PI" = "#00BFC4")
    ) +
    scale_fill_manual(
      values = c("Projection" = "#F8766D", "UN Projection" = "#00BFC4", "95% UN PI" = "#00BFC4")
    ) +
    scale_linetype_manual(
      values = c("Projection" = "dashed", "UN Projection" = "solid", "95% UN PI" = "solid")
    ) +
    labs(
      title = plt_title,
    ) +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) + # Increase font sizes
    theme(
      plot.title = element_text(size = DOWNLOAD_PLOT_SIZE$title),
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

  # This is the visible plot so we vary the font sizes depending on screen resolution
  plt_visible <-
    plt +
    theme_minimal(base_size = PLOTLY_TEXT_SIZE$font) +
    labs(title = plt_title_adapted$title) +
    theme(
      plot.title = element_text(size = plt_title_adapted$font_size),
      legend.position = "bottom"
    )

  plt_visible <- ggplotly(plt_visible, tooltip = c("x", "y", "color", "ymax", "ymin")) %>% layout(legend = PLOTLY_LEGEND_OPTS)

  list(
    gg = plt,
    plotly = config(plt_visible, displayModeBar = FALSE)
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

  names(melt_data) <- c("Year", "95% Lower bound PI", "95% Upper bound PI", "Type", var_name)

  max_year <- max(melt_data$Year)
  min_year <- min(melt_data$Year)

  columns_to_round <- c(var_name, "95% Lower bound PI", "95% Upper bound PI")
  melt_data[, (columns_to_round) := lapply(.SD, round, 3), .SDcols = columns_to_round]

  title_type <- ifelse(
    data_type == "yadr",
    "Young-age dependency ratio (Age <20 / Age 20-64)",
    "Old-age dependency ratio (Age 65+ / Age 20-64)"
  )

  plt_title <- paste0(title_type, ": ", country, ", ", min_year, "-", max_year)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  # Plot the data
  plt <-
    ggplot(
      melt_data,
      aes(x = Year, y = !!sym(var_name), group = Type, color = Type, fill = Type)
    ) +
    geom_line(aes(linetype = Type)) +
    geom_ribbon(
      data = melt_data[Type == "UN Projection"],
      aes(
        y = NULL,
        ymin = .data[["95% Lower bound PI"]],
        ymax = .data[["95% Upper bound PI"]],
        color = "95% UN PI",
        fill = "95% UN PI",
        linetype = "95% UN PI"
      ), ,
      alpha = 0.2
    ) +
    scale_color_manual(
      values = c("Projection" = "#F8766D", "UN Projection" = "#00BFC4", "95% UN PI" = "#00BFC4")
    ) +
    scale_fill_manual(
      values = c("Projection" = "#F8766D", "UN Projection" = "#00BFC4", "95% UN PI" = "#00BFC4")
    ) +
    scale_linetype_manual(
      values = c("Projection" = "dashed", "UN Projection" = "solid", "95% UN PI" = "solid")
    ) +
    labs(
      title = plt_title,
    ) +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) + # Increase font sizes
    theme(
      plot.title = element_text(size = DOWNLOAD_PLOT_SIZE$title),
      legend.position = "bottom"
    )

  year <- NULL
  Year <- NULL
  value <- NULL
  Type <- NULL
  low <- NULL
  high <- NULL
  data_type <- NULL

  # This is the visible plot so we vary the font sizes depending on screen resolution
  plt_visible <-
    plt +
    theme_minimal(base_size = PLOTLY_TEXT_SIZE$font) +
    labs(title = plt_title_adapted$title) +
    theme(
      plot.title = element_text(size = plt_title_adapted$font_size),
      legend.position = "bottom"
    )

  plt_visible <- ggplotly(plt_visible, tooltip = c("x", "y", "color", "ymax", "ymin")) %>% layout(legend = PLOTLY_LEGEND_OPTS)

  list(
    gg = plt,
    plotly = config(plt_visible, displayModeBar = FALSE)
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
#' @importFrom ggplot2 aes_string ggplot geom_point labs theme_minimal scale_shape_manual
#' @importFrom plotly ggplotly
#' @importFrom data.table data.table setnames
#' @export
#'
create_un_projection_plot <- function(dt, end_year, name_mappings, percent_x = FALSE) {
  # Extract the title from the name_mappings and separate it from column mappings
  plt_title <- name_mappings[["title"]]
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

  columns_to_round <- c(names(combined_data)[2], names(combined_data)[3])
  combined_data[, (columns_to_round) := lapply(.SD, round, 3), .SDcols = columns_to_round]

  # Add text for ggplotly to display on hover
  combined_data$text <- paste("Year:", combined_data$year)

  if (names(combined_data)[3] == "Population") {
    names(combined_data)[3] <- "Population per 1000 persons"
  }

  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  # Create the ggplot object with the mappings
  plt <-
    ggplot(
      combined_data,
      aes(
        x = !!sym(names(combined_data)[2]),
        y = !!sym(names(combined_data)[3]),
        color = Type,
        group = Type,
        shape = Type,
        text = text
      ),
    ) +
    geom_point() +
    scale_shape_manual(
      values = c("Projection" = 3, "UN Projection" = 16)
    ) +
    labs(title = plt_title) +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) + # Increase font sizes
    theme(
      plot.title = element_text(size = DOWNLOAD_PLOT_SIZE$title),
      legend.position = "bottom"
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

  # This is the visible plot so we vary the font sizes depending on screen resolution
  plt_visible <-
    plt +
    theme_minimal(base_size = PLOTLY_TEXT_SIZE$font) +
    labs(title = plt_title_adapted$title) +
    theme(
      plot.title = element_text(size = plt_title_adapted$font_size),
      legend.position = "bottom"
    )

  plt_visible <- ggplotly(plt_visible, tooltip = c("x", "y", "color", "text")) %>% layout(legend = PLOTLY_LEGEND_OPTS)

  list(
    gg = plt,
    plotly = config(plt_visible, displayModeBar = FALSE)
  )
}
