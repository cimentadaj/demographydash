#' Data Utility Functions
#'
#' @description
#' Utility functions to eliminate code duplication in data handling operations.
#' These functions provide common operations for column mapping, validation,
#' and reference year extraction across the application.

#' Map Column Names to Standard Format
#'
#' @description
#' Maps display column names back to standard internal names using i18n translation.
#' Eliminates duplication of column mapping logic across multiple functions.
#'
#' @param data Data frame to map column names for
#' @param i18n Translation object for getting display names
#'
#' @return Data frame with standardized column names (age, popM, popF)
#' @export
#'
map_column_names <- function(data, i18n) {
  # Create mapping from translated display names to standard names
  col_mapping <- list()
  col_mapping[[i18n$t("Age")]] <- "age"
  col_mapping[[i18n$t("Male (in thousands)")]] <- "popM" 
  col_mapping[[i18n$t("Female (in thousands)")]] <- "popF"
  
  # Apply mapping by name, not position
  for (old_name in names(col_mapping)) {
    if (old_name %in% names(data)) {
      names(data)[names(data) == old_name] <- col_mapping[[old_name]]
    }
  }
  
  data
}

#' Ensure Standard Column Order and Presence
#'
#' @description
#' Validates that required columns exist and ensures standard column order.
#' Used consistently across data processing functions.
#'
#' @param data Data frame to validate and reorder
#' @param required_cols Character vector of required column names (default: c("age", "popM", "popF"))
#'
#' @return Data frame with validated columns in standard order
#' @export
#'
ensure_standard_columns <- function(data, required_cols = c("age", "popM", "popF")) {
  # Check required columns exist
  if (!all(required_cols %in% names(data))) {
    missing_cols <- setdiff(required_cols, names(data))
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", "), 
               "Found:", paste(names(data), collapse = ", ")))
  }
  
  # Ensure standard column order, handling both data.frame and data.table
  if (inherits(data, "data.table")) {
    data[, ..required_cols]
  } else {
    data[, required_cols]
  }
}

#' Map Standard Column Names to Display Names
#'
#' @description
#' Maps standard internal names to translated display names using i18n.
#' Used when preparing data for display in UI components.
#'
#' @param data Data frame to map column names for
#' @param i18n Translation object for getting display names
#'
#' @return Data frame with translated display column names
#' @export
#'
map_column_names_to_display <- function(data, i18n) {
  # Create mapping from standard names to translated display names
  col_mapping <- list()
  if ("age" %in% names(data)) {
    col_mapping[["age"]] <- i18n$t("Age")
  }
  if ("popM" %in% names(data)) {
    col_mapping[["popM"]] <- i18n$t("Male (in thousands)")
  }
  if ("popF" %in% names(data)) {
    col_mapping[["popF"]] <- i18n$t("Female (in thousands)")
  }
  
  # Apply mapping by name, not position
  for (old_name in names(col_mapping)) {
    if (old_name %in% names(data)) {
      names(data)[names(data) == old_name] <- col_mapping[[old_name]]
    }
  }
  
  data
}

#' Extract Reference Year from Date Input
#'
#' @description
#' Standardizes reference year extraction from various date input formats.
#' Eliminates duplication of date parsing logic across multiple observers.
#'
#' @param ref_date Date input from Shiny (can be Date, character, or NULL)
#' @param fallback_year Numeric fallback year if ref_date is invalid
#'
#' @return Numeric year value
#' @export
#'
extract_reference_year <- function(ref_date, fallback_year) {
  if (!is.null(ref_date)) {
    if (is.character(ref_date)) {
      as.numeric(format(as.Date(ref_date), "%Y"))
    } else if (inherits(ref_date, "Date")) {
      as.numeric(format(ref_date, "%Y"))
    } else {
      fallback_year
    }
  } else {
    fallback_year
  }
}

#' Safe, memoized WPP population fetch
#'
#' Fetches UN WPP population with simple memoization per session and returns
#' a fresh, unkeyed data.table to avoid low-level data.table key issues.
#'
#' @param country Country name (character)
#' @param year Numeric year (single)
#' @param n Age grouping: 1 for single ages, 5 for 5-year groups
#' @return data.table with columns age, popF, popM
#' @export
safe_get_wpp_pop <- local({
  cache <- new.env(parent = emptyenv())
  function(country, year, n = 1) {
    key <- paste0(country, "|", year, "|", n)
    if (exists(key, envir = cache, inherits = FALSE)) {
      return(get(key, envir = cache, inherits = FALSE))
    }
    dt <- OPPPserver::get_wpp_pop(country, year = year, n = n)
    # Return a fresh, unkeyed data.table copy
    dt <- data.table::as.data.table(as.data.frame(dt))
    data.table::setkey(dt, NULL)
    assign(key, dt, envir = cache)
    dt
  }
})
