# Global variables to avoid R CMD check warnings
utils::globalVariables(c("Life Expectancy", "..col_ordering"))

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
#' @param i18n The internationalization object for translations.
#'
#' @importFrom ggplot2 aes ggplot geom_bar coord_flip labs theme_minimal theme scale_x_continuous scale_x_discrete scale_y_continuous element_blank element_text
#' @importFrom data.table melt
#' @importFrom plotly ggplotly layout config
#' @importFrom scales cut_short_scale label_number
#'
#' @return A ggplot2 object.
#' @export
#'
create_pop_pyramid_plot <- function(dt, country = NULL, input_year = NULL, i18n = NULL) {
  # Robust guard: if data is missing or empty, return an empty plot
  if (is.null(dt) || !all(c("age", "popF", "popM") %in% names(dt)) || nrow(dt) == 0) {
    empty <- ggplot() + theme_minimal() + labs(title = i18n$translate("Population by age and sex: "))
    return(plotly::ggplotly(empty))
  }
  # Normalize to a fresh, unkeyed data.table to avoid low-level data.table issues
  dt <- data.table::as.data.table(as.data.frame(dt))
  data.table::setkey(dt, NULL)
  if ("year" %in% names(dt)) {
    dt <- dt[year == input_year]
    id_vars <- c("year", "age")
    cols_to_pick <- c("Year", "Sex", "Age", "Population")
  } else {
    id_vars <- c("age")
    cols_to_pick <- c("Sex", "Age", "Population")
  }

  pop_dt <-
    melt(
      dt,
      id.vars = id_vars,
      measure.vars = c("popF", "popM"),
      variable.name = "gender",
      value.name = "population"
    )

  # Convert age to factor with proper ordering
  # Extract numeric values for sorting (handle "100+" type labels)
  age_numeric <- as.numeric(gsub("\\+.*", "", gsub("-.*", "", pop_dt$age)))
  # Create ordered factor based on numeric values
  pop_dt$age <- factor(pop_dt$age, levels = unique(pop_dt$age[order(age_numeric)]))
  pop_dt$sex <- pop_dt$gender
  pop_dt$gender <- NULL

  pop_dt[sex == "popM", sex := "Males"]
  pop_dt[sex == "popF", sex := "Females"]

  names(pop_dt) <- tools::toTitleCase(names(pop_dt))
  pop_dt <- pop_dt[, cols_to_pick, with = FALSE]
  pop_dt[["Population"]] <- round(pop_dt[["Population"]], 3)
  pop_index <- which(grepl("Population", names(pop_dt)))
  names(pop_dt)[pop_index] <- paste0(names(pop_dt)[pop_index], " (000s)")

  translate_title <- i18n$translate("Population by age and sex: ")
  plt_title <- paste0(translate_title, country, ", ", input_year)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  tmp_dt <- as.data.frame(pop_dt)
  males <- tmp_dt[["Sex"]] == "Males"
  tmp_dt[males, "Population (000s)"] <- -as.numeric(tmp_dt[males, "Population (000s)"])

  tmp_dt <- tmp_dt[c("Age", "Population (000s)", "Sex")]

  tmp_dt$Sex <- i18n$translate(as.character(tmp_dt$Sex))
  
  # Set factor levels to ensure Males appears first in legend
  males_label <- i18n$translate("Males")
  females_label <- i18n$translate("Females")
  sex_colors <- stats::setNames(
    c("#F8766D", "#00BFC4"),
    c(females_label, males_label)
  )
  tmp_dt$Sex <- factor(tmp_dt$Sex, levels = c(males_label, females_label))
  
  names(tmp_dt) <- i18n$translate(names(tmp_dt))

  cols_nm <- names(tmp_dt)

  plt <-
    ggplot(
      tmp_dt,
      aes(x = .data[[cols_nm[1]]], y = .data[[cols_nm[2]]], fill = .data[[cols_nm[3]]])
    ) +
    geom_bar(alpha = 0.7, stat = "identity") +
    scale_fill_manual(values = sex_colors) +
    scale_x_discrete(breaks = seq(0, 100, by = 5)) +
    scale_y_continuous(labels = function(x) paste0(abs(x))) +
    coord_flip() +
    labs(title = plt_title) +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) +
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
#' @param i18n The internationalization object for translations.
#'
#' @importFrom ggplot2 aes ggplot geom_line labs theme_minimal scale_color_manual
#' @importFrom plotly ggplotly
#' @importFrom data.table as.data.table
#' @importFrom stats setNames
#'
#' @return A ggplot2 object.
#' @export
#'
create_e0_plot <- function(dt, end_year, country, i18n = NULL) {
  LifeExpectancy <- Year <- e0M <- e0F <- value <- Sex <- NULL  # To avoid R CMD check notes

  # Ensure the data is in the correct format
  dt <- as.data.table(dt)
  setnames(dt, c("year", "e0M", "e0F"), c("Year", "e0M", "e0F"))

  # Filter data up to end_year
  dt <- dt[Year <= end_year]

  # Pivot the data to long format
  dt_long <- melt(dt, id.vars = "Year",
                  measure.vars = c("e0M", "e0F"),
                  variable.name = "Sex", value.name = "Life Expectancy")

  # Update Sex labels
  dt_long[, Sex := ifelse(Sex == "e0M", "Male", "Female")]

  # Round the life expectancy values
  dt_long[, `Life Expectancy` := round(`Life Expectancy`, 3)]

  # Get min and max years for the title
  max_year <- max(dt$Year)
  min_year <- min(dt$Year)

  # Create plot title

  translated_title <- i18n$translate("Life Expectancy: ")
  plt_title <- paste0(translated_title, country, ", ", min_year, "-", max_year)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)


  dt_long$Sex <- i18n$translate(as.character(dt_long$Sex))
  names(dt_long) <- i18n$translate(names(dt_long))
  col_nm <- names(dt_long)

  cl_translate <- setNames(
    c("#F8766D", "#00BFC4"),
    c(i18n$translate("Female"), i18n$translate("Male"))
  )

  # Create ggplot
  plt <- ggplot(
    dt_long,
    aes(x = .data[[col_nm[1]]], y = .data[[col_nm[3]]], color = .data[[col_nm[2]]])
  ) +
    geom_line(size = 2, alpha = 0.7) +
    labs(
      title = plt_title,
      y = i18n$translate("Life Expectancy (years)"),
      color = i18n$translate("Sex")
    ) +
    scale_color_manual(values = cl_translate) +
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

#' Create Comparative Pop Time Plot
#'
#' Generates a population-over-time comparison plot across multiple simulations.
#'
#' @param dt Data table containing combined population data with a `simulation` column.
#' @param input_age Translated age group selected in the UI.
#' @param i18n The internationalization object for translations.
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon scale_color_manual scale_fill_manual
#'   scale_linetype_manual scale_y_continuous labs theme_minimal theme element_text expansion
#' @importFrom plotly ggplotly
#' @importFrom data.table melt as.data.table setkey copy
#' @importFrom stats setNames
#' @importFrom scales hue_pal label_number
#'
#' @return A named list containing the ggplot object and plotly widget.
#' @export
#'
create_pop_time_compare_plot <- function(dt, input_age, i18n) {
  if (is.null(dt) || nrow(dt) == 0) return(NULL)

  pop_dt <- data.table::as.data.table(dt)
  data.table::setkey(pop_dt, NULL)
  if (!"simulation" %in% names(pop_dt)) return(NULL)

  pop_dt <-
    data.table::melt(
      pop_dt,
      id.vars = c("year", "age", "simulation", "un_pop_95low", "un_pop_95high"),
      measure.vars = c("pop", "un_pop_median"),
      variable.name = "type_value",
      value.name = "value"
    )

  pop_dt[, age := i18n$translate(as.character(age))]
  pop_dt <- pop_dt[age == input_age]
  if (nrow(pop_dt) == 0) return(NULL)

  pop_dt[type_value == "pop", type_value := "Projection"]
  pop_dt[type_value == "un_pop_median", type_value := "UN Projection"]

  pop_dt <- pop_dt[, .(
    Year = year,
    Age = age,
    Simulation = as.character(simulation),
    Type = type_value,
    `Population (000s)` = value,
    `95% Lower bound PI` = un_pop_95low,
    `95% Upper bound PI` = un_pop_95high
  )]

  if (nrow(pop_dt) == 0) return(NULL)

  numeric_cols <- c("Population (000s)", "95% Lower bound PI", "95% Upper bound PI")
  pop_dt[, (numeric_cols) := lapply(.SD, round, 3), .SDcols = numeric_cols]

  type_labels <- i18n$translate(c("Projection", "UN Projection"))
  pop_dt[, Type := i18n$translate(as.character(Type))]
  pop_dt[, Type := factor(Type, levels = type_labels)]

  sim_levels <- unique(pop_dt$Simulation)
  if (length(sim_levels) == 0) return(NULL)
  pop_dt[, Simulation := factor(Simulation, levels = sim_levels)]

  value_data <- pop_dt[, ..numeric_cols]
  min_y <- min(sapply(value_data, min, na.rm = TRUE))
  min_y <- min_y - (min_y * 0.05)
  max_y <- max(sapply(value_data, max, na.rm = TRUE))
  max_y <- max_y + (max_y * 0.05)

  color_palette <- stats::setNames(scales::hue_pal()(length(sim_levels)), sim_levels)
  linetype_values <- stats::setNames(
    c("solid", "longdash"),
    type_labels
  )
  linetype_values <- linetype_values[!is.na(names(linetype_values))]

  ribbon_dt <- pop_dt[Type == i18n$translate("UN Projection")]

  compare_label <- i18n$translate("Comparison")
  plt_title <- paste0(i18n$translate("Population Over Time"), " (", compare_label, ") - ", input_age)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  plt <-
    ggplot(
      pop_dt,
      aes(
        x = .data[["Year"]],
        y = .data[["Population (000s)"]],
        color = .data[["Simulation"]],
        group = interaction(.data[["Simulation"]], .data[["Type"]])
      )
    ) +
    geom_line(aes(linetype = .data[["Type"]])) +
    scale_color_manual(values = color_palette) +
    scale_linetype_manual(values = linetype_values, na.translate = FALSE) +
    scale_y_continuous(
      limits = c(min_y, max_y),
      expand = expansion(mult = 0),
      labels = label_number(big.mark = "")
    ) +
    labs(title = plt_title) +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) +
    theme(
      plot.title = element_text(size = DOWNLOAD_PLOT_SIZE$title),
      legend.position = "bottom"
    )

  if (nrow(ribbon_dt) > 0) {
    plt <- plt +
      geom_ribbon(
        data = ribbon_dt,
        aes(
          x = .data[["Year"]],
          ymin = .data[["95% Lower bound PI"]],
          ymax = .data[["95% Upper bound PI"]],
          fill = .data[["Simulation"]]
        ),
        inherit.aes = FALSE,
        alpha = 0.12
      ) +
      scale_fill_manual(values = color_palette)
  }

  plt <- plt + ggplot2::guides(fill = "none")

  Year <- NULL
  Simulation <- NULL
  Type <- NULL

  plt_visible <-
    plt +
    theme_minimal(base_size = PLOTLY_TEXT_SIZE$font) +
    labs(title = plt_title_adapted$title) +
    theme(
      plot.title = element_text(size = plt_title_adapted$font_size)
    )

  plt_visible <- ggplotly(plt_visible, tooltip = c("x", "y", "colour", "linetype")) %>%
    layout(legend = PLOTLY_LEGEND_OPTS)

  export_dt <- data.table::copy(pop_dt)
  export_dt[, `:=`(
    Age = as.character(Age),
    Simulation = as.character(Simulation),
    Type = as.character(Type)
  )]

  list(
    gg = plt,
    plotly = config(plt_visible, displayModeBar = FALSE),
    data = as.data.frame(export_dt)
  )
}

create_tfr_compare_plot <- function(dt, i18n) {
  if (is.null(dt) || nrow(dt) == 0) return(NULL)

  tfr_dt <- data.table::as.data.table(dt)
  data.table::setkey(tfr_dt, NULL)
  if (!"simulation" %in% names(tfr_dt)) return(NULL)

  tfr_dt <-
    data.table::melt(
      tfr_dt,
      id.vars = c("year", "simulation", "un_tfr_95low", "un_tfr_95high"),
      measure.vars = c("tfr", "un_tfr_median"),
      variable.name = "type_value",
      value.name = "value"
    )

  tfr_dt[type_value == "tfr", type_value := "Projection"]
  tfr_dt[type_value == "un_tfr_median", type_value := "UN Projection"]

  tfr_dt <- tfr_dt[, .(
    Year = year,
    Simulation = as.character(simulation),
    Type = type_value,
    `Births per woman` = value,
    `95% Lower bound PI` = un_tfr_95low,
    `95% Upper bound PI` = un_tfr_95high
  )]

  numeric_cols <- c("Births per woman", "95% Lower bound PI", "95% Upper bound PI")
  tfr_dt[, (numeric_cols) := lapply(.SD, round, 3), .SDcols = numeric_cols]

  type_labels <- i18n$translate(c("Projection", "UN Projection"))
  tfr_dt[, Type := i18n$translate(as.character(Type))]
  tfr_dt[, Type := factor(Type, levels = type_labels)]

  sim_levels <- unique(tfr_dt$Simulation)
  if (length(sim_levels) == 0) return(NULL)
  tfr_dt[, Simulation := factor(Simulation, levels = sim_levels)]

  value_data <- tfr_dt[, ..numeric_cols]
  min_vals <- sapply(value_data, function(x) suppressWarnings(min(x, na.rm = TRUE)))
  max_vals <- sapply(value_data, function(x) suppressWarnings(max(x, na.rm = TRUE)))
  min_vals <- min_vals[is.finite(min_vals)]
  max_vals <- max_vals[is.finite(max_vals)]
  if (!length(min_vals) || !length(max_vals)) return(NULL)
  min_y <- min(min_vals)
  max_y <- max(max_vals)
  buffer_min <- if (min_y == 0) 0 else abs(min_y) * 0.05
  buffer_max <- if (max_y == 0) 0 else abs(max_y) * 0.05
  min_y <- min_y - buffer_min
  max_y <- max_y + buffer_max

  color_palette <- stats::setNames(scales::hue_pal()(length(sim_levels)), sim_levels)
  linetype_values <- stats::setNames(
    c("solid", "longdash"),
    type_labels
  )
  linetype_values <- linetype_values[!is.na(names(linetype_values))]

  ribbon_dt <- tfr_dt[Type == i18n$translate("UN Projection")]

  compare_label <- i18n$translate("Comparison")
  plt_title <- paste0(i18n$translate("Projected Total Fertility Rate"), " (", compare_label, ")")
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  plt <-
    ggplot(
      tfr_dt,
      aes(
        x = .data[["Year"]],
        y = .data[["Births per woman"]],
        color = .data[["Simulation"]],
        group = interaction(.data[["Simulation"]], .data[["Type"]])
      )
    ) +
    geom_line(aes(linetype = .data[["Type"]])) +
    scale_color_manual(values = color_palette) +
    scale_linetype_manual(values = linetype_values, na.translate = FALSE) +
    scale_y_continuous(
      limits = c(min_y, max_y),
      expand = expansion(mult = 0),
      labels = label_number(accuracy = 0.1)
    ) +
    labs(
      title = plt_title,
      y = i18n$translate("Births per woman")
    ) +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) +
    theme(
      plot.title = element_text(size = DOWNLOAD_PLOT_SIZE$title),
      legend.position = "bottom"
    )

  if (nrow(ribbon_dt) > 0) {
    plt <- plt +
      geom_ribbon(
        data = ribbon_dt,
        aes(
          x = .data[["Year"]],
          ymin = .data[["95% Lower bound PI"]],
          ymax = .data[["95% Upper bound PI"]],
          fill = .data[["Simulation"]]
        ),
        inherit.aes = FALSE,
        alpha = 0.12
      ) +
      scale_fill_manual(values = color_palette)
  }

  plt <- plt + ggplot2::guides(fill = "none")

  Year <- NULL
  Simulation <- NULL
  Type <- NULL

  plt_visible <-
    plt +
    theme_minimal(base_size = PLOTLY_TEXT_SIZE$font) +
    labs(title = plt_title_adapted$title) +
    theme(
      plot.title = element_text(size = plt_title_adapted$font_size)
    )

  plt_visible <- ggplotly(plt_visible, tooltip = c("x", "y", "colour", "linetype")) %>%
    layout(legend = PLOTLY_LEGEND_OPTS)

  export_dt <- data.table::copy(tfr_dt)
  export_dt[, `:=`(
    Simulation = as.character(Simulation),
    Type = as.character(Type)
  )]

  list(
    gg = plt,
    plotly = config(plt_visible, displayModeBar = FALSE),
    data = as.data.frame(export_dt)
  )
}

create_e0_compare_plot <- function(dt, selected_sex, i18n) {
  if (is.null(dt) || nrow(dt) == 0) return(NULL)
  if (is.null(selected_sex) || !nzchar(selected_sex)) return(NULL)

  e0_dt <- data.table::as.data.table(dt)
  data.table::setkey(e0_dt, NULL)
  required_cols <- c("year", "sex", "e0", "un_e0_median", "un_e0_95low", "un_e0_95high", "simulation")
  if (!all(required_cols %in% names(e0_dt))) return(NULL)

  e0_dt[, sex := as.character(sex)]
  e0_dt <- e0_dt[sex == selected_sex]
  if (nrow(e0_dt) == 0) return(NULL)

  map_sex_label <- function(x) {
    data.table::fcase(
      x %in% c("B", "Total", "Both"), "Total",
      x %in% c("M", "Male"), "Male",
      x %in% c("F", "Female"), "Female",
      default = as.character(x)
    )
  }

  e0_dt[, Sex := map_sex_label(sex)]
  e0_dt[, Sex := i18n$translate(as.character(Sex))]

  e0_dt <-
    data.table::melt(
      e0_dt,
      id.vars = c("year", "Sex", "simulation", "un_e0_95low", "un_e0_95high"),
      measure.vars = c("e0", "un_e0_median"),
      variable.name = "type_value",
      value.name = "value"
    )

  e0_dt[type_value == "e0", type_value := "Projection"]
  e0_dt[type_value == "un_e0_median", type_value := "UN Projection"]

  e0_dt <- e0_dt[, .(
    Year = year,
    Sex,
    Simulation = as.character(simulation),
    Type = type_value,
    `Life Expectancy (years)` = value,
    `95% Lower bound PI` = un_e0_95low,
    `95% Upper bound PI` = un_e0_95high
  )]

  numeric_cols <- c("Life Expectancy (years)", "95% Lower bound PI", "95% Upper bound PI")
  e0_dt[, (numeric_cols) := lapply(.SD, round, 3), .SDcols = numeric_cols]

  type_labels <- i18n$translate(c("Projection", "UN Projection"))
  e0_dt[, Type := i18n$translate(as.character(Type))]
  e0_dt[, Type := factor(Type, levels = type_labels)]

  sim_levels <- unique(e0_dt$Simulation)
  if (length(sim_levels) == 0) return(NULL)
  e0_dt[, Simulation := factor(Simulation, levels = sim_levels)]

  value_data <- e0_dt[, ..numeric_cols]
  min_vals <- sapply(value_data, function(x) suppressWarnings(min(x, na.rm = TRUE)))
  max_vals <- sapply(value_data, function(x) suppressWarnings(max(x, na.rm = TRUE)))
  min_vals <- min_vals[is.finite(min_vals)]
  max_vals <- max_vals[is.finite(max_vals)]
  if (!length(min_vals) || !length(max_vals)) return(NULL)
  min_y <- min(min_vals)
  max_y <- max(max_vals)
  buffer_min <- if (min_y == 0) 0 else abs(min_y) * 0.05
  buffer_max <- if (max_y == 0) 0 else abs(max_y) * 0.05
  min_y <- min_y - buffer_min
  max_y <- max_y + buffer_max

  color_palette <- stats::setNames(scales::hue_pal()(length(sim_levels)), sim_levels)
  linetype_values <- stats::setNames(
    c("solid", "longdash"),
    type_labels
  )
  linetype_values <- linetype_values[!is.na(names(linetype_values))]

  ribbon_dt <- e0_dt[Type == i18n$translate("UN Projection")]

  compare_label <- i18n$translate("Comparison")
  sex_label <- unique(e0_dt$Sex)
  sex_label <- if (length(sex_label) > 0) sex_label[[1]] else ""
  title_base <- i18n$translate("Life Expectancy Over Time")
  plt_title <- paste0(title_base, " (", compare_label, ") - ", sex_label)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  plt <-
    ggplot(
      e0_dt,
      aes(
        x = .data[["Year"]],
        y = .data[["Life Expectancy (years)"]],
        color = .data[["Simulation"]],
        group = interaction(.data[["Simulation"]], .data[["Type"]])
      )
    ) +
    geom_line(aes(linetype = .data[["Type"]])) +
    scale_color_manual(values = color_palette) +
    scale_linetype_manual(values = linetype_values, na.translate = FALSE) +
    scale_y_continuous(
      limits = c(min_y, max_y),
      expand = expansion(mult = 0),
      labels = label_number(accuracy = 0.1)
    ) +
    labs(
      title = plt_title,
      y = i18n$translate("Life Expectancy (years)")
    ) +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) +
    theme(
      plot.title = element_text(size = DOWNLOAD_PLOT_SIZE$title),
      legend.position = "bottom"
    )

  plt <- plt + ggplot2::guides(fill = "none")

  plt_visible <-
    plt +
    theme_minimal(base_size = PLOTLY_TEXT_SIZE$font) +
    labs(title = plt_title_adapted$title) +
    theme(
      plot.title = element_text(size = plt_title_adapted$font_size)
    )

  plt_visible <- ggplotly(plt_visible, tooltip = c("x", "y", "colour", "linetype")) %>%
    layout(legend = PLOTLY_LEGEND_OPTS)

  export_dt <- data.table::copy(e0_dt)
  export_dt[, `:=`(
    Sex = as.character(Sex),
    Simulation = as.character(Simulation),
    Type = as.character(Type)
  )]

  list(
    gg = plt,
    plotly = config(plt_visible, displayModeBar = FALSE),
    data = as.data.frame(export_dt)
  )
}

create_mig_compare_plot <- function(dt, i18n) {
  if (is.null(dt) || nrow(dt) == 0) return(NULL)

  mig_dt <- data.table::as.data.table(dt)
  data.table::setkey(mig_dt, NULL)
  if (!"simulation" %in% names(mig_dt)) return(NULL)

  mig_dt <-
    data.table::melt(
      mig_dt,
      id.vars = c("year", "simulation", "un_mig_95low", "un_mig_95high"),
      measure.vars = c("mig", "un_mig_median"),
      variable.name = "type_value",
      value.name = "value"
    )

  mig_dt[type_value == "mig", type_value := "Projection"]
  mig_dt[type_value == "un_mig_median", type_value := "UN Projection"]

  mig_dt <- mig_dt[, .(
    Year = year,
    Simulation = as.character(simulation),
    Type = type_value,
    `Net Migration` = value,
    `95% Lower bound PI` = un_mig_95low,
    `95% Upper bound PI` = un_mig_95high
  )]

  numeric_cols <- c("Net Migration", "95% Lower bound PI", "95% Upper bound PI")
  mig_dt[, (numeric_cols) := lapply(.SD, round, 3), .SDcols = numeric_cols]

  type_labels <- i18n$translate(c("Projection", "UN Projection"))
  mig_dt[, Type := i18n$translate(as.character(Type))]
  mig_dt[, Type := factor(Type, levels = type_labels)]

  sim_levels <- unique(mig_dt$Simulation)
  if (length(sim_levels) == 0) return(NULL)
  mig_dt[, Simulation := factor(Simulation, levels = sim_levels)]

  value_data <- mig_dt[, ..numeric_cols]
  min_vals <- sapply(value_data, function(x) suppressWarnings(min(x, na.rm = TRUE)))
  max_vals <- sapply(value_data, function(x) suppressWarnings(max(x, na.rm = TRUE)))
  min_vals <- min_vals[is.finite(min_vals)]
  max_vals <- max_vals[is.finite(max_vals)]
  if (!length(min_vals) || !length(max_vals)) return(NULL)
  min_y <- min(min_vals)
  max_y <- max(max_vals)
  buffer_min <- if (min_y == 0) 0 else abs(min_y) * 0.05
  buffer_max <- if (max_y == 0) 0 else abs(max_y) * 0.05
  min_y <- min_y - buffer_min
  max_y <- max_y + buffer_max

  color_palette <- stats::setNames(scales::hue_pal()(length(sim_levels)), sim_levels)
  linetype_values <- stats::setNames(
    c("solid", "longdash"),
    type_labels
  )
  linetype_values <- linetype_values[!is.na(names(linetype_values))]

  ribbon_dt <- mig_dt[Type == i18n$translate("UN Projection")]

  compare_label <- i18n$translate("Comparison")
  title_base <- i18n$translate("Projected Net Migration")
  plt_title <- paste0(title_base, " (", compare_label, ")")
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  plt <-
    ggplot(
      mig_dt,
      aes(
        x = .data[["Year"]],
        y = .data[["Net Migration"]],
        color = .data[["Simulation"]],
        group = interaction(.data[["Simulation"]], .data[["Type"]])
      )
    ) +
    geom_line(aes(linetype = .data[["Type"]])) +
    scale_color_manual(values = color_palette) +
    scale_linetype_manual(values = linetype_values, na.translate = FALSE) +
    scale_y_continuous(
      limits = c(min_y, max_y),
      expand = expansion(mult = 0),
      labels = label_number(big.mark = ",")
    ) +
    labs(
      title = plt_title,
      y = i18n$translate("Net Migration")
    ) +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) +
    theme(
      plot.title = element_text(size = DOWNLOAD_PLOT_SIZE$title),
      legend.position = "bottom"
    )

  if (nrow(ribbon_dt) > 0) {
    plt <- plt +
      geom_ribbon(
        data = ribbon_dt,
        aes(
          x = .data[["Year"]],
          ymin = .data[["95% Lower bound PI"]],
          ymax = .data[["95% Upper bound PI"]],
          fill = .data[["Simulation"]]
        ),
        inherit.aes = FALSE,
        alpha = 0.12
      ) +
      scale_fill_manual(values = color_palette)
  }

  plt <- plt + ggplot2::guides(fill = "none")

  plt_visible <-
    plt +
    theme_minimal(base_size = PLOTLY_TEXT_SIZE$font) +
    labs(title = plt_title_adapted$title) +
    theme(
      plot.title = element_text(size = plt_title_adapted$font_size)
    )

  plt_visible <- ggplotly(plt_visible, tooltip = c("x", "y", "colour", "linetype")) %>%
    layout(legend = PLOTLY_LEGEND_OPTS)

  export_dt <- data.table::copy(mig_dt)
  export_dt[, `:=`(
    Simulation = as.character(Simulation),
    Type = as.character(Type)
  )]

  list(
    gg = plt,
    plotly = config(plt_visible, displayModeBar = FALSE),
    data = as.data.frame(export_dt)
  )
}

create_deaths_births_compare_plot <- function(dt_list, option, i18n) {
  if (is.null(dt_list) || length(dt_list) == 0) return(NULL)
  if (is.null(option) || !nzchar(option)) return(NULL)

  option_parts <- strsplit(option, "_")[[1]]
  if (length(option_parts) != 2) return(NULL)
  data_type <- option_parts[[1]]
  value_type <- option_parts[[2]]
  if (!data_type %in% c("birth", "death")) return(NULL)
  if (!value_type %in% c("counts", "rates")) return(NULL)

  dataset <- dt_list[[data_type]]
  if (is.null(dataset) || nrow(dataset) == 0) return(NULL)

  dt <- data.table::as.data.table(dataset)
  data.table::setkey(dt, NULL)
  if (!"simulation" %in% names(dt)) return(NULL)

  if (identical(value_type, "counts")) {
    value_col <- if (identical(data_type, "birth")) "births" else "deaths"
    median_col <- if (identical(data_type, "birth")) "un_births_median" else "un_deaths_median"
    lower_col <- if (identical(data_type, "birth")) "un_births_95low" else "un_deaths_95low"
    upper_col <- if (identical(data_type, "birth")) "un_births_95high" else "un_deaths_95high"
  } else {
    value_col <- if (identical(data_type, "birth")) "cbr" else "cdr"
    median_col <- if (identical(data_type, "birth")) "un_cbr_median" else "un_cdr_median"
    lower_col <- if (identical(data_type, "birth")) "un_cbr_95low" else "un_cdr_95low"
    upper_col <- if (identical(data_type, "birth")) "un_cbr_95high" else "un_cdr_95high"
  }
  needed_cols <- c("year", "simulation", value_col, median_col, lower_col, upper_col)
  if (!all(needed_cols %in% names(dt))) return(NULL)

  prepared <- dt[, .(
    Year = year,
    Simulation = as.character(simulation),
    Projection = get(value_col),
    `UN Projection` = get(median_col),
    `95% Lower bound PI` = as.numeric(get(lower_col)),
    `95% Upper bound PI` = as.numeric(get(upper_col))
  )]

  cat("[COMPARE_DEATHS_BIRTHS] Prepared sample:
")
  print(utils::head(prepared, 5))

  melt_dt <- data.table::melt(
    prepared,
    id.vars = c("Year", "Simulation", "95% Lower bound PI", "95% Upper bound PI"),
    measure.vars = c("Projection", "UN Projection"),
    variable.name = "Type",
    value.name = "value"
  )

  cat("[COMPARE_DEATHS_BIRTHS] Melted sample:
")
  print(utils::head(melt_dt, 5))

  var_name <- if (identical(value_type, "counts")) {
    paste0("Number of ", tolower(data_type), "s (thousands)")
  } else {
    paste0(tools::toTitleCase(data_type), "s per 1,000 population")
  }

  data.table::setnames(melt_dt, "value", var_name)

  numeric_cols <- c(var_name, "95% Lower bound PI", "95% Upper bound PI")
  melt_dt[, (numeric_cols) := lapply(.SD, function(x) round(as.numeric(x), 3)), .SDcols = numeric_cols]

  cat("[COMPARE_DEATHS_BIRTHS] Final sample:
")
  print(utils::head(melt_dt, 5))

  type_labels <- i18n$translate(c("Projection", "UN Projection"))
  melt_dt[, Type := i18n$translate(as.character(Type))]
  melt_dt[, Type := factor(Type, levels = type_labels)]

  sim_levels <- unique(melt_dt$Simulation)
  if (length(sim_levels) == 0) return(NULL)
  melt_dt[, Simulation := factor(Simulation, levels = sim_levels)]

  value_data <- melt_dt[, ..numeric_cols]
  min_vals <- sapply(value_data, function(x) suppressWarnings(min(x, na.rm = TRUE)))
  max_vals <- sapply(value_data, function(x) suppressWarnings(max(x, na.rm = TRUE)))
  min_vals <- min_vals[is.finite(min_vals)]
  max_vals <- max_vals[is.finite(max_vals)]
  if (!length(min_vals) || !length(max_vals)) return(NULL)

  pad <- function(v) if (is.na(v) || v == 0) 0 else abs(v) * 0.05
  min_y <- min(min_vals) - pad(min(min_vals))
  max_y <- max(max_vals) + pad(max(max_vals))

  color_palette <- stats::setNames(scales::hue_pal()(length(sim_levels)), sim_levels)
  linetype_values <- stats::setNames(c("solid", "longdash"), type_labels)
  ribbon_dt <- melt_dt[Type == i18n$translate("UN Projection")]

  compare_label <- i18n$translate("Comparison")
  variant_labels <- stats::setNames(
    i18n$translate(c("Birth Counts", "Birth Rates", "Death Counts", "Death Rates")),
    c("birth_counts", "birth_rates", "death_counts", "death_rates")
  )
  variant_label <- variant_labels[[option]] %||% option
  plt_title <- paste0(i18n$translate("Deaths and Births"), " (", compare_label, ") - ", variant_label)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  plt <- ggplot(
    melt_dt,
    aes(
      x = .data[["Year"]],
      y = .data[[var_name]],
      color = .data[["Simulation"]],
      group = interaction(.data[["Simulation"]], .data[["Type"]])
    )
  ) +
    geom_ribbon(
      data = ribbon_dt,
      aes(
        x = .data[["Year"]],
        ymin = .data[["95% Lower bound PI"]],
        ymax = .data[["95% Upper bound PI"]],
        fill = .data[["Simulation"]]
      ),
      alpha = 0.12
    ) +
    geom_line(aes(linetype = .data[["Type"]])) +
    scale_color_manual(values = color_palette) +
    scale_fill_manual(values = color_palette) +
    scale_linetype_manual(values = linetype_values, na.translate = FALSE) +
    scale_y_continuous(limits = c(min_y, max_y), expand = expansion(mult = 0)) +
    labs(
      title = plt_title,
      y = i18n$translate(var_name)
    ) +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) +
    theme(
      plot.title = element_text(size = DOWNLOAD_PLOT_SIZE$title),
      legend.position = "bottom"
    )

  plt <- plt + ggplot2::guides(fill = "none")

  plt_visible <-
    plt +
    theme_minimal(base_size = PLOTLY_TEXT_SIZE$font) +
    labs(title = plt_title_adapted$title) +
    theme(plot.title = element_text(size = plt_title_adapted$font_size))

  plt_visible <- ggplotly(plt_visible, tooltip = c("x", "y", "colour", "linetype")) %>%
    layout(legend = PLOTLY_LEGEND_OPTS)

  export_dt <- data.table::copy(melt_dt)
  export_dt[, `:=`(
    Simulation = as.character(Simulation),
    Type = as.character(Type)
  )]

  list(
    gg = plt,
    plotly = config(plt_visible, displayModeBar = FALSE),
    data = as.data.frame(export_dt)
  )
}
create_dependency_compare_plot <- function(dt_list, option, i18n) {
  if (is.null(dt_list) || length(dt_list) == 0) return(NULL)
  if (is.null(option) || !nzchar(option)) return(NULL)
  if (!option %in% c("oadr", "yadr")) return(NULL)

  dataset <- dt_list[[option]]
  if (is.null(dataset) || nrow(dataset) == 0) return(NULL)

  dt <- data.table::as.data.table(dataset)
  data.table::setkey(dt, NULL)
  if (!"simulation" %in% names(dt)) return(NULL)

  value_col <- option
  median_col <- paste0("un_", option, "_median")
  if (!all(c("year", value_col, median_col, "low", "high") %in% names(dt))) return(NULL)

  dt <- dt[, .(year, simulation, value = get(value_col), median = get(median_col), low = as.numeric(low), high = as.numeric(high))]

  melt_dt <- data.table::melt(
    dt,
    id.vars = c("year", "simulation", "low", "high"),
    measure.vars = c("value", "median"),
    variable.name = "type_value",
    value.name = "value"
  )

  melt_dt[type_value == "value", type_value := "Projection"]
  melt_dt[type_value == "median", type_value := "UN Projection"]

  var_name <- if (identical(option, "yadr")) {
    "Persons age <20 per 100 persons age 20-64"
  } else {
    "Persons age 65+ per 100 persons age 20-64"
  }

  melt_dt <- melt_dt[, .(
    Year = year,
    Simulation = as.character(simulation),
    Type = type_value,
    value,
    `95% Lower bound PI` = low,
    `95% Upper bound PI` = high
  )]
  setnames(melt_dt, "value", var_name)

  numeric_cols <- c(var_name, "95% Lower bound PI", "95% Upper bound PI")
  melt_dt[, (numeric_cols) := lapply(.SD, round, 3), .SDcols = numeric_cols]

  type_labels <- i18n$translate(c("Projection", "UN Projection"))
  melt_dt[, Type := i18n$translate(Type)]
  melt_dt[, Type := factor(Type, levels = type_labels)]

  sim_levels <- unique(melt_dt$Simulation)
  if (length(sim_levels) == 0) return(NULL)
  melt_dt[, Simulation := factor(Simulation, levels = sim_levels)]

  melt_dt[, (var_name) := as.numeric(get(var_name))]

  value_data <- melt_dt[, ..numeric_cols]
  min_vals <- sapply(value_data, function(x) suppressWarnings(min(x, na.rm = TRUE)))
  max_vals <- sapply(value_data, function(x) suppressWarnings(max(x, na.rm = TRUE)))
  min_vals <- min_vals[is.finite(min_vals)]
  max_vals <- max_vals[is.finite(max_vals)]
  if (!length(min_vals) || !length(max_vals)) return(NULL)
  min_y <- min(min_vals)
  max_y <- max(max_vals)
  buffer_min <- if (min_y == 0) 0 else abs(min_y) * 0.05
  buffer_max <- if (max_y == 0) 0 else abs(max_y) * 0.05
  min_y <- min_y - buffer_min
  max_y <- max_y + buffer_max

  color_palette <- stats::setNames(scales::hue_pal()(length(sim_levels)), sim_levels)
  linetype_values <- stats::setNames(
    c("solid", "longdash"),
    type_labels
  )
  linetype_values <- linetype_values[!is.na(names(linetype_values))]

  ribbon_dt <- melt_dt[Type == i18n$translate("UN Projection")]

  compare_label <- i18n$translate("Comparison")
  option_labels <- stats::setNames(
    i18n$translate(c(
      "Old-age dependency ratio (Age 65+ / Age 20-64)",
      "Young-age dependency ratio (Age <20 / Age 20-64)"
    )),
    c("oadr", "yadr")
  )
  title_base <- i18n$translate("YADR and OADR")
  option_label <- option_labels[[option]] %||% option
  plt_title <- paste0(title_base, " (", compare_label, ") - ", option_label)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  melt_dt[, (var_name) := as.numeric(get(var_name))]

  plt <-
    ggplot(
      melt_dt,
      aes(
        x = .data[["Year"]],
        y = .data[[var_name]],
        color = .data[["Simulation"]],
        group = interaction(.data[["Simulation"]], .data[["Type"]])
      )
    ) +
    geom_line(aes(linetype = .data[["Type"]])) +
    scale_color_manual(values = color_palette) +
    scale_linetype_manual(values = linetype_values, na.translate = FALSE) +
    scale_y_continuous(
      limits = c(min_y, max_y),
      expand = expansion(mult = 0)
    ) +
    labs(
      title = plt_title,
      y = i18n$translate(var_name)
    ) +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) +
    theme(
      plot.title = element_text(size = DOWNLOAD_PLOT_SIZE$title),
      legend.position = "bottom"
    )

  if (nrow(ribbon_dt) > 0) {
    plt <- plt +
      geom_ribbon(
        data = ribbon_dt,
        aes(
          x = .data[["Year"]],
          ymin = .data[["95% Lower bound PI"]],
          ymax = .data[["95% Upper bound PI"]],
          fill = .data[["Simulation"]]
        ),
        inherit.aes = FALSE,
        alpha = 0.12
      ) +
      scale_fill_manual(values = color_palette)
  }

  plt <- plt + ggplot2::guides(fill = "none")

  plt_visible <-
    plt +
    theme_minimal(base_size = PLOTLY_TEXT_SIZE$font) +
    labs(title = plt_title_adapted$title) +
    theme(
      plot.title = element_text(size = plt_title_adapted$font_size)
    )

  plt_visible <- ggplotly(plt_visible, tooltip = c("x", "y", "colour", "linetype")) %>%
    layout(legend = PLOTLY_LEGEND_OPTS)

  export_dt <- data.table::copy(melt_dt)
  export_dt[, `:=`(
    Simulation = as.character(Simulation),
    Type = as.character(Type)
  )]

  list(
    gg = plt,
    plotly = config(plt_visible, displayModeBar = FALSE),
    data = as.data.frame(export_dt)
  )
}

#' Create Migration Plot
#'
#' This function takes a data table to create a net migration plot.
#'
#' @param dt Data table with migration data.
#' @param end_year The date in YYYY-MM-DD where the projection should end.
#' @param country The country for which the data is plotted.
#' @param i18n The internationalization object for translations.
#'
#' @importFrom ggplot2 aes ggplot geom_line labs theme_minimal
#' @importFrom plotly ggplotly
#' @importFrom data.table as.data.table
#'
#' @return A list containing ggplot2 and plotly objects.
#' @export
#'
create_mig_plot <- function(dt, end_year, country, i18n) {
  Migration <- Year <- mig <- value <- Type <- NULL  # To avoid R CMD check notes

  # Ensure the data is in the correct format
  dt <- as.data.table(dt)
  setnames(dt, c("year", "mig"), c("Year", "Migration"))

  # Filter data up to end_year
  dt <- dt[Year <= end_year]

  # Pivot the data to long format
  dt_long <- melt(dt, id.vars = "Year",
                  measure.vars = "Migration",
                  variable.name = "Type", value.name = "Migration")

  max_year <- max(dt_long$Year)
  min_year <- min(dt_long$Year)

  translated_title <- i18n$translate("Net Migration: ")
  plt_title <- paste0(translated_title, country, ", ", min_year, "-", max_year)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  names(dt_long) <- i18n$translate(names(dt_long))
  col_nm <- names(dt_long)

  plt <- ggplot(
    dt_long,
    aes(x = .data[[col_nm[1]]], y = .data[[col_nm[3]]])
    ) +
    geom_line(size = 2, alpha = 0.7) +
    labs(title = plt_title,
         x = i18n$translate("Year"),
         y = i18n$translate("Net Migration")) +
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

#' Create Projected Migration Plot
#'
#' This function takes a data table to create a projected net migration plot.
#'
#' @param dt Data table with projected migration data.
#' @param end_year The date in YYYY-MM-DD where the projection should end.
#' @param country The country for which the data is plotted.
#' @param i18n The internationalization object for translations.
#'
#' @importFrom ggplot2 aes ggplot geom_line geom_ribbon labs theme_minimal scale_color_manual scale_fill_manual scale_linetype_manual
#' @importFrom plotly ggplotly layout
#' @importFrom data.table as.data.table
#' @importFrom stats setNames
#'
#' @return A list containing ggplot2 and plotly objects.
#' @export
#'
create_mig_projected_plot <- function(dt, end_year, country, i18n) {
  `Net Migration` <- type_value <- Year <- Migration <- Type <- NULL

  mig_dt <- melt(
    dt,
    id.vars = c("year", "un_mig_95low", "un_mig_95high"),
    measure.vars = c("mig", "un_mig_median"),
    variable.name = "type_value",
    value.name = "value"
  )

  mig_dt <- mig_dt[mig_dt$year <= as.numeric(end_year), ]
  mig_dt[type_value == "mig", type_value := "Projection"]
  mig_dt[type_value == "un_mig_median", type_value := "UN Projection"]

  mig_dt <- mig_dt[, c("year", "type_value", "value", "un_mig_95low", "un_mig_95high")]
  names(mig_dt) <- c("Year", "Type", "Net Migration", "95% Lower bound PI", "95% Upper bound PI")

  max_year <- max(mig_dt$Year)
  min_year <- min(mig_dt$Year)
  translated_title <- i18n$translate("Net Migration: ")
  plt_title <- paste0(translated_title, country, ", ", min_year, "-", max_year)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)


  columns_to_round <- c("Net Migration", "95% Lower bound PI", "95% Upper bound PI")
  mig_dt[, (columns_to_round) := lapply(.SD, round, 3), .SDcols = columns_to_round]


  mig_dt$Type <- i18n$translate(as.character(mig_dt$Type))

  un_proj_df <- mig_dt[Type == i18n$translate("UN Projection"), ]
  names(un_proj_df) <- i18n$translate(names(un_proj_df))
  names(mig_dt) <- i18n$translate(names(mig_dt))

  col_vals <- setNames(
    c("#F8766D", "#00BFC4", "#00BFC4"),
    i18n$translate(c("Projection", "UN Projection", "95% UN PI"))
  )

  shape_vals <- setNames(
    c("dashed", "solid", "solid"),
    i18n$translate(c("Projection", "UN Projection", "95% UN PI"))
  )

  col_nm <- names(mig_dt)

  mig_dt <- as.data.frame(mig_dt)

  plt <- ggplot(
    mig_dt,
    aes(
      x = .data[[col_nm[1]]],
      y = .data[[col_nm[3]]],
      group = .data[[col_nm[2]]],
      color = .data[[col_nm[2]]],
      fill = .data[[col_nm[2]]]
    )
  ) +
    geom_line(aes(linetype = .data[[col_nm[2]]])) +
    geom_ribbon(
      data = un_proj_df,
      aes(
        y = NULL,
        ymin = .data[[col_nm[[4]]]],
        ymax = .data[[col_nm[[5]]]],
        color = i18n$translate("95% UN PI"),
        fill = i18n$translate("95% UN PI"),
        linetype = i18n$translate("95% UN PI")
      ),
      alpha = 0.2
    ) +
    scale_color_manual(
      values = col_vals
    ) +
    scale_fill_manual(
      values = col_vals
    ) +
    scale_linetype_manual(
      values = shape_vals
    ) +
    labs(title = plt_title) +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) +
    theme(
      plot.title = element_text(size = DOWNLOAD_PLOT_SIZE$title),
      legend.position = "bottom"
    )

  plt_visible <-
    plt +
    theme_minimal(base_size = PLOTLY_TEXT_SIZE$font) +
    labs(title = plt_title_adapted$title) +
    theme(
      plot.title = element_text(size = plt_title_adapted$font_size),
      legend.position = "bottom"
    )

  plt_visible <-
    ggplotly(plt_visible, tooltip = c("x", "y", "ymax", "ymin")) %>%
    layout(legend = update_plotly_legend_opts(PLOTLY_LEGEND_OPTS))

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
#' @param input_sex the sex to filter on the e0 projected as a string.
#' @param country The country for which the data is plotted
#' @param i18n The internationalization object for translations.
#'
#' @importFrom ggplot2 aes ggplot geom_line labs theme_minimal scale_color_manual
#' @importFrom plotly ggplotly
#' @importFrom data.table fcase
#' @importFrom stats setNames
#'
#' @return A ggplot2 object.
#' @export
#'
create_e0_projected_plot <- function(dt, input_sex, country, i18n) {
  `Life Expectancy` <- sex <- type_value <- Year <- LifeExpectancy <- Type <- NULL

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

  # Translate type values
  e0_dt[type_value == "e0", type_value := "Projection"]
  e0_dt[type_value == "un_e0_median", type_value := "UN Projection"]

  e0_dt <- e0_dt[, c("year", "sex", "type_value", "value", "un_e0_95low", "un_e0_95high")]

  # Translate column names
  names(e0_dt) <- c(
    "Year",
    "Sex",
    "Type",
    "Life Expectancy",
    "95% Lower bound PI",
    "95% Upper bound PI"
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

  # Translate title
  translated_title <- i18n$translate("Life Expectancy: ")
  plt_title <- paste0(translated_title, country, ", ", min_year, "-", max_year)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  e0_dt$Type <- i18n$translate(as.character(e0_dt$Type))

  tmp_dt <- e0_dt[Type == i18n$translate("UN Projection")]
  names(tmp_dt) <- i18n$translate(names(tmp_dt))

  # Translate column values and names for tooltips
  e0_dt$Sex <- i18n$translate(as.character(e0_dt$Sex))
  names(e0_dt) <- i18n$translate(names(e0_dt))
  col_nm <- names(e0_dt)

  # Create color and line type scales with translations
  col_vals <- setNames(
    c("#F8766D", "#00BFC4", "#00BFC4"),
    i18n$translate(c("Projection", "UN Projection", "95% UN PI"))
  )

  shape_vals <- setNames(
    c("dashed", "solid", "solid"),
    i18n$translate(c("Projection", "UN Projection", "95% UN PI"))
  )

  plt <-
    e0_dt %>%
    ggplot(
      aes(
      .data[[col_nm[1]]], 
      .data[[col_nm[4]]], 
      color = .data[[col_nm[3]]], 
      fill = .data[[col_nm[3]]], 
      group = .data[[col_nm[3]]])
    ) +
    geom_line(aes(linetype = .data[[col_nm[3]]])) +
    geom_ribbon(
      data = tmp_dt,
      aes(
        y = NULL,
        ymin = .data[[col_nm[5]]],
        ymax = .data[[col_nm[6]]],
        color = i18n$translate("95% UN PI"),
        fill = i18n$translate("95% UN PI"),
        linetype = i18n$translate("95% UN PI")
      ),
      alpha = 0.2
    ) +
    scale_color_manual(
      values = col_vals
    ) +
    scale_fill_manual(
      values = col_vals
    ) +
    scale_linetype_manual(
      values = shape_vals
    ) +
    scale_y_continuous(
      limits = c(min_y, max_y),
      expand = expansion(mult = 0),
      labels = label_number(big.mark = "")
    ) +
    labs(
      title = plt_title,
      y = i18n$translate("Life Expectancy (years)")
    ) +
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
      plot.title = element_text(size = plt_title_adapted$font_size),
      legend.position = "bottom"
    )

  plt_visible <-
    ggplotly(plt_visible, tooltip = c("x", "y", "ymax", "ymin")) %>%
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
#' @param i18n The internationalization object for translations.
#'
#' @importFrom ggplot2 ggplot aes_string geom_line theme_minimal theme
#' @importFrom data.table melt
#' @importFrom plotly ggplotly layout
#' @importFrom rlang sym !!
#'
#' @return A ggplot2 object.
#' @export
#'
create_age_group_plot <- function(dt, input_scale, country, i18n) {
  # Add req() to ensure input_scale is not NULL or NA
  req(input_scale)
  
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
  axis_label <- ifelse(input_scale == "Percent", 
                      i18n$translate("(Percent)"), 
                      i18n$translate("(000s)"))

  type_pop <- paste0("Population ", axis_label)
  names(pop_dt) <- c("Year", "Age", "Type", type_pop)

  pop_dt[[type_pop]] <- round(pop_dt[[type_pop]], 3)

  min_year <- min(pop_dt$Year)
  max_year <- max(pop_dt$Year)

  plt_title <- paste0(i18n$translate("Population by Broad Age Groups"), ": ", country, ", ", min_year, "-", max_year)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  # Translate column names just before ggplot
  names(pop_dt) <- i18n$translate(names(pop_dt))
  col_nm <- names(pop_dt)

  plt <-
    pop_dt %>%
    ggplot(aes(
      x = .data[[col_nm[1]]], 
      y = .data[[col_nm[4]]], 
      color = .data[[col_nm[2]]]
    )) +
    geom_line() +
    scale_color_manual(values = color_labels()) +
    labs(
      title = plt_title,
      y = col_nm[4],
      color = i18n$translate("Age groups")
    ) +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) +
    theme(
      plot.title = element_text(size = DOWNLOAD_PLOT_SIZE$title),
      legend.position = "bottom"
    )

  if (input_scale == i18n$translate("Percent")) {
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

  plt_visible <- ggplotly(plt_visible, tooltip = c("x", "y", "color")) %>% 
    layout(legend = PLOTLY_LEGEND_OPTS)

  list(
    gg = plt,
    plotly = config(plt_visible, displayModeBar = FALSE)
  )
}


#' Create Pop Age Group Time Plot
#'
#' This function takes a data table and an input of age group to create a time age group plot.
#'
#' @param dt Data table with population data
#' @param input_age The age group to plot
#' @param country A string with the current country name
#' @param i18n The internationalization object for translations
#'
#' @importFrom ggplot2 ggplot aes_string geom_line theme_minimal theme geom_ribbon scale_color_manual geom_rect expansion scale_fill_manual
#' @importFrom data.table melt
#' @importFrom plotly ggplotly layout
#' @importFrom stats setNames
#'
#' @return A ggplot2 object.
#' @export
#'
create_pop_time_plot <- function(dt, input_age, country, i18n) {
  pop_dt <-
    melt(
      dt,
      id.vars = c("year", "age", "un_pop_95low", "un_pop_95high"),
      measure.vars = c("pop", "un_pop_median"),
      variable.name = "type_value",
      value.name = "value"
    )

  pop_dt$age <- i18n$translate(as.character(pop_dt$age))
  pop_dt <- pop_dt[pop_dt$age == input_age, ]
  
  # Check if pop_dt is empty after filtering
  if (nrow(pop_dt) == 0) {
    return(NULL)  # Return NULL if no data matches the selected age group
  }
  
  pop_dt[type_value == "pop", type_value := "Projection"]
  pop_dt[type_value == "un_pop_median", type_value := "UN Projection"]

  pop_dt <- pop_dt[, c("year", "age", "type_value", "value", "un_pop_95low", "un_pop_95high")]

  names(pop_dt) <- c(
    "Year",
    "Age",
    "Type",
    "Population (000s)",
    "95% Lower bound PI",
    "95% Upper bound PI"
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

  col_vals <- setNames(
    c("#F8766D", "#00BFC4", "#00BFC4"),
    i18n$translate(c("Projection", "UN Projection", "95% UN PI"))
  )

  shape_vals <- setNames(
    c("dashed", "solid", "solid"),
    i18n$translate(c("Projection", "UN Projection", "95% UN PI"))
  )

  plt_title <- paste0(i18n$translate("Population by age and sex: "), input_age, ", ", country, ", ", min_year, "-", max_year)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  pop_dt$Type <- i18n$translate(as.character(pop_dt$Type))
  tmp_dt <- pop_dt[Type == i18n$translate("UN Projection")]
  names(pop_dt) <- i18n$translate(names(pop_dt))
  names(tmp_dt) <- i18n$translate(names(tmp_dt))

  col_names <- names(pop_dt)

  plt <-
    pop_dt %>%
    ggplot(
      aes(
        .data[[col_names[1]]], 
        .data[[col_names[4]]], 
        color = .data[[col_names[3]]], 
        fill = .data[[col_names[3]]], 
        group = .data[[col_names[3]]]
      )
    ) +
    geom_line(aes(linetype = .data[[col_names[3]]])) +
    geom_ribbon(
      data = tmp_dt,
      aes(
        y = NULL,
        ymin = .data[[col_names[5]]],
        ymax = .data[[col_names[6]]],
        color = i18n$translate("95% UN PI"),
        fill = i18n$translate("95% UN PI"),
        linetype = i18n$translate("95% UN PI")
      ),
      alpha = 0.2
    ) +
    scale_color_manual(values = col_vals) +
    scale_fill_manual(values = col_vals) +
    scale_linetype_manual(values = shape_vals) +
    scale_y_continuous(
      limits = c(min_y, max_y),
      expand = expansion(mult = 0),
      labels = label_number(big.mark = "")
    ) +
    labs(title = plt_title) +
    theme_minimal(base_size = DOWNLOAD_PLOT_SIZE$font) +
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

  plt_visible <- ggplotly(plt_visible, tooltip = c("x", "y", "ymax", "ymin")) %>% layout(legend = PLOTLY_LEGEND_OPTS)

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
#' @param i18n The internationalization object for translations.
#'
#' @importFrom ggplot2 ggplot aes_string geom_line theme_minimal theme geom_ribbon scale_y_continuous scale_color_manual
#' @importFrom data.table melt
#' @importFrom plotly ggplotly
#' @importFrom stats setNames
#'
#' @return A ggplot2 object.
#' @export
#'
create_tfr_projected_plot <- function(dt, end_year, country, i18n) {
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

  tfr_dt <- tfr_dt[, c("Year", "Type", "Births per woman", "95% Lower bound PI", "95% Upper bound PI")]

  max_year <- max(tfr_dt$Year)
  min_year <- min(tfr_dt$Year)


  plt_title <- paste0(i18n$translate("Total Fertility Rate: "), country, ", ", min_year, "-", max_year)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  columns_to_round <- c("Births per woman", "95% Lower bound PI", "95% Upper bound PI")
  tfr_dt[, (columns_to_round) := lapply(.SD, round, 3), .SDcols = columns_to_round]

  tfr_dt$Type <- i18n$translate(as.character(tfr_dt$Type))
  tmp_dt <- tfr_dt[Type == i18n$translate("UN Projection")]
  names(tmp_dt) <- i18n$translate(names(tmp_dt))

  names(tfr_dt) <- i18n$translate(names(tfr_dt))

  col_names <- names(tfr_dt)

  col_vals <- setNames(
    c("#F8766D", "#00BFC4", "#00BFC4"),
    i18n$translate(c("Projection", "UN Projection", "95% UN PI"))
  )

  shape_vals <- setNames(
    c("dashed", "solid", "solid"),
    i18n$translate(c("Projection", "UN Projection", "95% UN PI"))
  )


  plt <-
    ggplot(
      tfr_dt, 
      aes(
        .data[[col_names[1]]], 
        .data[[col_names[3]]], 
        group = .data[[col_names[2]]], 
        color = .data[[col_names[2]]], 
        fill = .data[[col_names[2]]])
    ) +
    geom_line(aes(linetype = .data[[col_names[2]]])) +
    geom_ribbon(
      data = tmp_dt,
      aes(
        y = NULL,
        ymin = .data[[col_names[4]]],
        ymax = .data[[col_names[5]]],
        color = i18n$translate("95% UN PI"),
        fill = i18n$translate("95% UN PI"),
        linetype = i18n$translate("95% UN PI")
      ),
      alpha = 0.2
    ) +
    scale_color_manual(
      values = col_vals
    ) +
    scale_fill_manual(
      values = col_vals
    ) +
    scale_linetype_manual(
      values = shape_vals
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

  plt_visible <- ggplotly(plt_visible, tooltip = c("x", "y", "ymax", "ymin")) %>% layout(legend = PLOTLY_LEGEND_OPTS)

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
#' @param i18n The internationalization object for translations.
#'
#' @importFrom ggplot2 ggplot aes_string geom_line theme_minimal theme geom_ribbon scale_color_manual
#' @importFrom data.table melt
#' @importFrom plotly ggplotly layout
#'
#' @return A ggplot2 object.
#' @export
#'
create_annual_growth_plot <- function(dt, end_year, country, i18n) {
  dt <- dt[dt$year <= as.numeric(end_year), ]
  dt$value <- dt$growth_rate
  dt$growth_rate <- NULL
  dt$type_value <- dt$age
  dt$age <- NULL

  dt <- dt[, c("year", "type_value", "value")]
  names(dt) <- c("Year", "Age", "Population Growth Rate")

  max_year <- max(dt$Year)
  min_year <- min(dt$Year)

  columns_to_round <- c("Population Growth Rate")
  dt[, (columns_to_round) := lapply(.SD, round, 3), .SDcols = columns_to_round]

  plt_title <- paste0(i18n$translate("Population growth rates for broad age groups: "), country, ", ", min_year, "-", max_year)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  names(dt) <- i18n$translate(names(dt))

  col_names <- names(dt)

  plt <-
    dt %>%
    ggplot(aes(.data[[col_names[1]]], .data[[col_names[3]]], color = .data[[col_names[2]]], group = .data[[col_names[2]]])) +
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

  plt_visible <- ggplotly(
    plt_visible,
    tooltip = c("x", "y", "color")
  ) %>%
    layout(legend = PLOTLY_LEGEND_OPTS)

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
#' @param i18n The internationalization object for translations.
#'
#' @importFrom ggplot2 aes ggplot geom_line labs theme_minimal scale_color_manual
#' @importFrom plotly ggplotly
#'
#' @return A ggplot2 object.
#' @export
#'
create_tfr_plot <- function(dt, end_year, country, i18n) {
  Year <- NULL
  TFR <- NULL

  names(dt) <- c("Year", "TFR")

  dt <- dt[Year <= end_year]

  max_year <- max(dt$Year)
  min_year <- min(dt$Year)

  columns_to_round <- c("TFR")
  dt[, (columns_to_round) := lapply(.SD, round, 3), .SDcols = columns_to_round]

  plt_title <- paste0(i18n$translate("Total Fertility Rate: "), country, ", ", min_year, "-", max_year)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  names(dt) <- i18n$translate(names(dt))
  col_names <- names(dt)

  plt <-
    dt %>%
    ggplot(aes(.data[[col_names[1]]], .data[[col_names[2]]])) +
    geom_line(size = 2, alpha = 0.7) +
    labs(
      title = plt_title,
      y = i18n$translate("Total Fertility Rate")
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
#' @param i18n The internationalization object for translations.
#'
#' @importFrom DT datatable
#'
#' @return A shiny.semantic DataTable object.
#' @export
#'
prepare_pop_agegroups_table <- function(wpp_dt, i18n) {
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
    data.frame(age_group = i18n$t("Total"), population = sum(summary_table$population))
  )

  # Format population
  summary_table$population_formatted <- ifelse(
    summary_table$population >= 1000,
    paste0(round(summary_table$population / 1000, 1), "M"),
    paste0(round(summary_table$population, 1), "K")
  )

  # Calculate percentage
  len_pop <- length(summary_table$population)
  total_population <- sum(summary_table$population[-len_pop])
  summary_table$percentage <- round(c(summary_table$population[1:(nrow(summary_table) - 1)] / total_population * 100, 100), 0)

  summary_table$population <- NULL
  summary_table$percentage <- paste0(summary_table$percentage, "%")
  names(summary_table) <- c("age_group", "population", "percentage")
  row.names(summary_table) <- NULL

  display_names <- i18n$translate(c("Age groups", "Population", "Percentage"))

  DT::datatable(
    summary_table,
    colnames = display_names,
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
#' @param i18n The internationalization object for translations.
#'
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot geom_line geom_ribbon labs theme_minimal scale_color_manual scale_linetype_manual geom_blank .data
#' @importFrom plotly ggplotly
#' @importFrom data.table setnames .SD :=
#' @importFrom stats setNames
#'
#' @export
#'
create_deaths_births_plot <- function(forecast_birth, forecast_death, data_type, value_type, end_year, country, i18n) {
  # Validate input
  if (!(data_type %in% c("birth", "death")) | !(value_type %in% c("counts", "rates"))) {
    stop("Invalid data_type or value_type")
  }

  # Add req() to ensure data_type and value_type are not NULL or NA
  req(data_type, value_type)
  
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

  melt_data <- melt_data[, c("year", "type_value", "value", "low", "high")]
  names(melt_data) <- c("Year", "Type", var_name, "95% Lower bound PI", "95% Upper bound PI")

  columns_to_round <- c(var_name, "95% Lower bound PI", "95% Upper bound PI")
  melt_data[, (columns_to_round) := lapply(.SD, round, 3), .SDcols = columns_to_round]

  max_year <- max(melt_data$Year)
  min_year <- min(melt_data$Year)

  # Determine the type of data (Births/Deaths)
  data_type_title <- ifelse(data_type == "birth", "Birth", "Death")

  # Construct the title based on value_type
  plt_title <- ifelse(
    value_type == "rates",
    paste0(
      i18n$translate(paste0("Crude ", data_type_title, " Rate: ")),
      country, ", ", min_year, "-", max_year
    ),
    paste0(
      i18n$translate(paste0("Annual number of ", tolower(data_type_title), "s: ")),
      country, ", ", min_year, "-", max_year
    )
  )

  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  melt_data$Type <- i18n$translate(as.character(melt_data$Type))

  tmp_dt <- melt_data[Type == i18n$translate("UN Projection")]
  names(tmp_dt) <- i18n$translate(names(tmp_dt))

  names(melt_data) <- i18n$translate(names(melt_data))
  col_names <- names(melt_data)

  col_vals <- setNames(
    c("#F8766D", "#00BFC4", "#00BFC4"),
    i18n$translate(c("Projection", "UN Projection", "95% UN PI"))
  )

  shape_vals <- setNames(
    c("dashed", "solid", "solid"),
    i18n$translate(c("Projection", "UN Projection", "95% UN PI"))
  )
  
  # Plot the data
  plt <-
    ggplot(
      melt_data,
      aes(x = .data[[col_names[1]]], y = .data[[col_names[3]]], group = .data[[col_names[2]]], color = .data[[col_names[2]]], fill = .data[[col_names[2]]])
    ) +
    geom_line(aes(linetype = .data[[col_names[2]]])) +
    geom_ribbon(
      data = tmp_dt,
      aes(
        y = NULL,
        ymin = .data[[col_names[4]]],
        ymax = .data[[col_names[5]]],
        color = i18n$translate("95% UN PI"),
        fill = i18n$translate("95% UN PI"),
        linetype = i18n$translate("95% UN PI")
      ),
      alpha = 0.2
    ) +
    scale_color_manual(
      values = col_vals
    ) +
    scale_fill_manual(
      values = col_vals
    ) +
    scale_linetype_manual(
      values = shape_vals
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

  plt_visible <- ggplotly(plt_visible, tooltip = c("x", "y", "ymax", "ymin")) %>% layout(legend = PLOTLY_LEGEND_OPTS)

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
#' @param i18n The internationalization object for translations.
#'
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot geom_line geom_ribbon labs theme_minimal scale_color_manual
#' @importFrom plotly ggplotly
#' @importFrom stats setNames
#'
#' @export
#'
create_yadr_oadr_plot <- function(oadr, yadr, data_type, end_year, country, i18n) {
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

  melt_data <- melt_data[, c("year", "type_value", "value", "low", "high")]
  names(melt_data) <- c("Year", "Type", var_name, "95% Lower bound PI", "95% Upper bound PI")

  max_year <- max(melt_data$Year)
  min_year <- min(melt_data$Year)

  columns_to_round <- c(var_name, "95% Lower bound PI", "95% Upper bound PI")
  melt_data[, (columns_to_round) := lapply(.SD, round, 3), .SDcols = columns_to_round]

  title_type <- ifelse(
    data_type == "yadr",
    "Young-age dependency ratio (Age <20 / Age 20-64)",
    "Old-age dependency ratio (Age 65+ / Age 20-64)"
  )

  plt_title <- paste0(i18n$translate(title_type), ": ", country, ", ", min_year, "-", max_year)
  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  melt_data$Type <- i18n$translate(as.character(melt_data$Type))
  tmp_dt <- melt_data[Type == i18n$translate("UN Projection")]
  names(tmp_dt) <- i18n$translate(names(tmp_dt))

  names(melt_data) <- i18n$translate(names(melt_data))
  col_names <- names(melt_data)

  col_vals <- setNames(
    c("#F8766D", "#00BFC4", "#00BFC4"),
    i18n$translate(c("Projection", "UN Projection", "95% UN PI"))
  )

  shape_vals <- setNames(
    c("dashed", "solid", "solid"),
    i18n$translate(c("Projection", "UN Projection", "95% UN PI")) 
  )

  # Plot the data
  plt <-
    ggplot(
      melt_data,
      aes(x = .data[[col_names[1]]], y = .data[[col_names[3]]], group = .data[[col_names[2]]], color = .data[[col_names[2]]], fill = .data[[col_names[2]]])
    ) +
    geom_line(aes(linetype = .data[[col_names[2]]])) +
    geom_ribbon(
      data = tmp_dt,
      aes(
        y = NULL,
        ymin = .data[[col_names[4]]],
        ymax = .data[[col_names[5]]],
        color = i18n$translate("95% UN PI"),
        fill = i18n$translate("95% UN PI"),
        linetype = i18n$translate("95% UN PI")
      ), ,
      alpha = 0.2
    ) +
    scale_color_manual(
      values = col_vals
    ) +
    scale_fill_manual(
      values = col_vals
    ) +
    scale_linetype_manual(
      values = shape_vals
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

  plt_visible <- ggplotly(plt_visible, tooltip = c("x", "y", "ymax", "ymin")) %>% layout(legend = PLOTLY_LEGEND_OPTS)

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
#' @param i18n The internationalization object for translations.
#'
#' @return A list containing a ggplot object and an interactive plotly object.
#'
#' @importFrom ggplot2 aes_string ggplot geom_point labs theme_minimal scale_shape_manual
#' @importFrom plotly ggplotly
#' @importFrom data.table data.table setnames
#' @importFrom stats setNames
#' @export
#'
create_un_projection_plot <- function(dt, end_year, name_mappings, percent_x = FALSE, i18n) {
  # Extract the title from the name_mappings and separate it from column mappings
  title_parts <- strsplit(name_mappings[["title"]], ":")[[1]]
  translated_first_part <- i18n$translate(title_parts[1])
  plt_title <- paste0(translated_first_part, title_parts[2])
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
  combined_data$text <- paste(i18n$translate("Year:"), combined_data$year)

  if (names(combined_data)[3] == "Population") {
    names(combined_data)[3] <- "Population per 1000 persons"
  }

  plt_title_adapted <- adjust_title_and_font(PLOTLY_TEXT_SIZE$type, plt_title)

  col_to_move <- "Type"
  first_col <- names(combined_data)[1]

  col_ordering <- c(
    first_col,
    col_to_move,
    names(combined_data)[!names(combined_data) %in% c(first_col, col_to_move)]
  )

  combined_data <- combined_data[, ..col_ordering]

  combined_data$Type <- i18n$translate(as.character(combined_data$Type))
  names(combined_data) <- i18n$translate(names(combined_data))

  col_names <- names(combined_data)

  shape_vals <- setNames(
   c(3, 16),
   i18n$translate(c("Projection", "UN Projection"))
  )

  # Create the ggplot object with the mappings
  plt <-
    ggplot(
      combined_data,
      aes(
        x = .data[[col_names[3]]],
        y = .data[[col_names[4]]],
        color = .data[[col_names[2]]],
        group = .data[[col_names[2]]],
        shape = .data[[col_names[2]]],
        text = .data[[col_names[5]]]
      ),
    ) +
    geom_point() +
    scale_shape_manual(
      values = shape_vals
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

  if (grepl("Population", col_names[3])) {
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

#' Create All Report Plots
#'
#' This function generates non-reactive versions of all plots that appear in the forecast page.
#' It takes the necessary inputs and creates each plot, then returns them as a named list.
#'
#' @param simulation_results List containing all forecast results data
#' @param country String containing the country name
#' @param start_year Numeric value for the projection start year
#' @param end_year Numeric value for the projection end year
#' @param pop_year Numeric value for the population pyramid year to display
#' @param age_group String indicating the age group for population over time plot
#' @param i18n The internationalization object
#'
#' @return A named list containing all generated plots
#' @export
#'
create_all_report_plots <- function(
    simulation_results,
    country,
    start_year,
    end_year,
    pop_year = start_year + 1, # Default for pyramid plot
    age_group = NULL, # For pop_time_plot, if available from UI context
    i18n = NULL
) {
  # Initialize an empty list to store all plots
  plot_list <- list()

  # 1. Population Pyramid (once)
  plot_list$pyramid <- create_pop_pyramid_plot(
    simulation_results$population_by_age_and_sex,
    country = country,
    input_year = pop_year,
    i18n = i18n
  )

  # 2. Population by Broad Age Groups (Absolute and Percentage)
  pop_display_options <- c("Absolute", "Percent")
  for (display_type in pop_display_options) {
    plot_name <- paste0("pop_age_group_", tolower(display_type))
    plot_list[[plot_name]] <- create_age_group_plot(
      simulation_results$population_by_broad_age_group,
      display_type,
      country,
      i18n
    )
  }

  # 3. Population Over Time by Specific Age Group (if age_group is provided)
  if (!is.null(age_group) && nzchar(age_group)) {
    plot_list$pop_time <- create_pop_time_plot(
      simulation_results$population_by_time,
      age_group, # This specific age group comes from the UI context
      country,
      i18n
    )
  }

  # 4. Projected Total Fertility Rate (once)
  plot_list$tfr_projected <- create_tfr_projected_plot(
    simulation_results$tfr_by_time,
    end_year,
    country,
    i18n
  )

  # 5. Annual Growth Plot (once)
  plot_list$annual_growth <- create_annual_growth_plot(
    simulation_results$annual_growth_rate,
    end_year,
    country,
    i18n
  )

  # 6. Deaths and Births Plots (all 4 variants)
  death_birth_variants <- list(
    list(name = "birth_counts", type = "birth", value = "counts"),
    list(name = "birth_rates", type = "birth", value = "rates"),
    list(name = "death_counts", type = "death", value = "counts"),
    list(name = "death_rates", type = "death", value = "rates")
  )
  for (variant in death_birth_variants) {
    plot_list[[variant$name]] <- create_deaths_births_plot(
      simulation_results$births_counts_rates,
      simulation_results$deaths_counts_rates,
      variant$type,
      variant$value,
      end_year,
      country,
      i18n
    )
  }

  # 7. YADR and OADR Plots (both variants)
  dependency_options <- c("oadr", "yadr")
  for (dep_type in dependency_options) {
    plot_name <- paste0(dep_type, "_ratio")
    plot_list[[plot_name]] <- create_yadr_oadr_plot(
      simulation_results$oadr,
      simulation_results$yadr,
      dep_type,
      end_year,
      country,
      i18n
    )
  }

  # 8. Population Size and Aging Plot (once)
  dt_psa <- simulation_results$pop_aging_and_pop_size
  max_year_psa <- max(dt_psa$year)
  min_year_psa <- min(dt_psa$year)
  title_psa <- paste0(
    i18n$t("Population size and percent of population 65+"), # Using existing translation key
    ": ", country, ", ", min_year_psa, "-", max_year_psa
  )
  plot_list$pop_size_aging <- create_un_projection_plot(
    dt_psa,
    end_year,
    c(
      "pop" = "Population",
      "percent65" = "% of population 65+",
      "title" = title_psa
    ),
    percent_x = TRUE,
    i18n = i18n
  )

  # 9. CDR and Life Expectancy Plot (once)
  dt_cdr_e0 <- simulation_results$cdr_by_e0
  max_year_cdr_e0 <- max(dt_cdr_e0$year)
  min_year_cdr_e0 <- min(dt_cdr_e0$year)
  title_cdr_e0 <- paste0(
    i18n$t("Crude death rate and life expectancy at birth:"), # Using existing translation key
    " ", country, ", ", min_year_cdr_e0, "-", max_year_cdr_e0
  )
  plot_list$cdr_e0_scatter <- create_un_projection_plot(
    dt_cdr_e0,
    end_year,
    c(
      "cdr" = "Crude Death Rate",
      "e0" = "Life Expectancy",
      "title" = title_cdr_e0
    ),
    i18n = i18n
  )

  # 10. CBR and TFR Plot (once)
  dt_cbr_tfr <- simulation_results$cbr_by_tfr
  max_year_cbr_tfr <- max(dt_cbr_tfr$year)
  min_year_cbr_tfr <- min(dt_cbr_tfr$year)
  title_cbr_tfr <- paste0(
    i18n$t("Crude birth rate and total fertility rate:"), # Using existing translation key
    " ", country, ", ", min_year_cbr_tfr, "-", max_year_cbr_tfr
  )
  plot_list$cbr_tfr_scatter <- create_un_projection_plot(
    dt_cbr_tfr,
    end_year,
    c(
      "cbr" = "Crude Birth Rate",
      "tfr" = "Total Fertility Rate",
      "title" = title_cbr_tfr
    ),
    i18n = i18n
  )

  # 11. Life Expectancy Over Time Plots (Total, Male, Female)
  sex_options <- c("Total", "Male", "Female")
  for (s_opt in sex_options) {
    plot_name <- paste0("e0_time_", tolower(s_opt))
    plot_list[[plot_name]] <- create_e0_projected_plot(
      simulation_results$e0_by_time,
      s_opt,
      country,
      i18n
    )
  }

  # 12. Projected Net Migration Plot (once)
  plot_list$mig_projected_time <- create_mig_projected_plot(
    simulation_results$mig_by_time,
    end_year,
    country,
    i18n
  )

  return(plot_list)
}
