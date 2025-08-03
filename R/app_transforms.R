#' Population Data Transformation Functions
#'
#' @description
#' Functions for transforming population data between different formats,
#' including conversion between 5-year and single-year age groups and
#' adjustment of open age groups.

#' Transform Population Data
#'
#' @description
#' Main transformation function that handles graduation from 5-year to 1-year
#' age groups and adjusts open age groups as needed.
#'
#' @param data Data frame with columns: age, popF, popM
#' @param age_type Either "Single Ages" or "5-Year Groups"
#' @param oag_current Current open age group value
#' @param oag_target Target open age group value (default 100)
#' @param interp_method Interpolation method for graduation (default "beers(ord)")
#' @param country Country code for OAG adjustment
#' @param ref_year Reference year for OAG extension (optional)
#'
#' @return Transformed data frame with single ages and OAG 100+
#' @importFrom OPPPserver graduate_pop extend_oag reduce_oag
#' @export
#'
transform_population_data <- function(data, age_type, oag_current, oag_target = 100, 
                                    interp_method = "beers(ord)", country = NULL, ref_year = NULL) {
  # Step 1: Validate and standardize column names
  data <- validate_population_data(data)
  
  # Step 2: Graduate from 5-year to 1-year if needed
  if (age_type == "5-Year Groups") {
    # For graduate_pop, we need numeric age values (0, 5, 10, etc.)
    # Extract starting age from labels like "0-4", "5-9", etc.
    age_starts <- as.numeric(gsub("^([0-9]+).*", "\\1", data$age))
    data$age <- age_starts
    # graduate_pop will graduate all non-age columns
    data <- graduate_pop(data, method = interp_method)
  }
  
  # Step 3: Adjust OAG if needed
  if (oag_current != oag_target) {
    data <- adjust_oag(data, oag_current, oag_target, country = country, ref_year = ref_year)
  }
  
  # Return as data frame
  data
}

#' Validate Population Data
#'
#' @description
#' Validates and standardizes population data column names and formats.
#'
#' @param data Data frame to validate
#'
#' @return Validated data frame with standardized column names
#' @export
#'
validate_population_data <- function(data) {
  # Standardize column names
  if (all(c("Age", "Female", "Male") %in% names(data))) {
    names(data) <- c("age", "popF", "popM")
  }
  
  # Don't convert age to numeric - preserve labels for 5-year groups
  # Just remove commas from population columns
  data$popF <- as.numeric(gsub(",", "", data$popF))
  data$popM <- as.numeric(gsub(",", "", data$popM))
  
  # Basic validation
  if (anyNA(data$popF) || anyNA(data$popM)) stop("Population data contains missing values")
  if (any(data$popF < 0) || any(data$popM < 0)) stop("Population cannot be negative")
  
  data
}

#' Adjust Open Age Group
#'
#' @description
#' Adjusts the open age group (OAG) of population data by extending or
#' reducing it to the target value.
#'
#' @param data Data frame with population data
#' @param oag_current Current OAG value
#' @param oag_target Target OAG value (default 100)
#' @param country Country code (uses input$wpp_country in production)
#' @param ref_year Reference year for OAG extension
#'
#' @return Data frame with adjusted OAG
#' @importFrom OPPPserver extend_oag reduce_oag
#' @export
#'
adjust_oag <- function(data, oag_current, oag_target = 100, country = NULL, ref_year = NULL) {
  # Convert OAG marker (e.g., "80+") to numeric before processing
  last_age_idx <- nrow(data)
  if (grepl("\\+$", data$age[last_age_idx])) {
    # Extract numeric part from "80+"
    data$age[last_age_idx] <- as.numeric(gsub("\\+", "", data$age[last_age_idx]))
  }
  
  # Ensure all age values are numeric
  data$age <- as.numeric(data$age)
  
  # Use reference year if provided, otherwise default to 2024
  year_to_use <- ifelse(!is.null(ref_year), ref_year, 2024)
  
  # In production, country will be passed from input$wpp_country
  if (is.null(country)) {
    stop("Country parameter is required for OAG adjustment")
  }
  
  if (oag_current < oag_target) {
    extend_oag(data, country = country, year = year_to_use, oag_new = oag_target)
  } else if (oag_current > oag_target) {
    reduce_oag(data, oag_target)
  } else {
    data
  }
}