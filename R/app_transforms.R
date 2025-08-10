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
                                    interp_method = "un", country = NULL, ref_year = NULL) {
  # Step 1: Validate and standardize column names
  data <- validate_population_data(data)
  
  # Step 2: Graduate from 5-year to 1-year if needed
  if (age_type == "5-Year Groups") {
    # For graduate_pop, we need numeric age values (0, 5, 10, etc.)
    # Extract starting age from labels like "0-4", "5-9", etc.
    age_starts <- as.numeric(gsub("^([0-9]+).*", "\\1", data$age))
    data$age <- age_starts
    # graduate_pop will graduate all non-age columns
    data <- graduate_pop(data, method = interp_method, country = country, year = ref_year)
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
  # Standardize column names by mapping them properly
  if (all(c("Age", "Female", "Male") %in% names(data))) {
    # Map columns by name, not by position
    col_mapping <- c("Age" = "age", "Female" = "popF", "Male" = "popM")
    for (old_name in names(col_mapping)) {
      names(data)[names(data) == old_name] <- col_mapping[old_name]
    }
  } else if (all(c("age", "popF", "popM") %in% names(data))) {
    # Already has the correct column names
  } else {
    # Try to identify columns by content or throw an error
    stop("Data must have columns: Age/age, Female/popF, Male/popM")
  }
  
  # Don't convert age to numeric - preserve labels for 5-year groups
  # Just remove commas from population columns
  data$popF <- as.numeric(gsub(",", "", data$popF))
  data$popM <- as.numeric(gsub(",", "", data$popM))
  
  # Basic validation
  if (anyNA(data$popF) || anyNA(data$popM)) stop("Population data contains missing values")
  if (any(data$popF < 0) || any(data$popM < 0)) stop("Population cannot be negative")
  
  # Ensure we return only the required columns in the correct order
  data[, c("age", "popF", "popM")]
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

#' Prepare Population Table for Display
#'
#' @description
#' Main function to prepare population data for display in rhandsontable.
#' Handles both UN and Custom data with appropriate transformations.
#'
#' @param data_source "UN Data" or "Custom Data"
#' @param un_cache_single Cached UN single age data
#' @param un_cache_5yr Cached UN 5-year data
#' @param un_display_type What to display for UN data
#' @param custom_stored_data Stored custom data
#' @param custom_stored_type Type of stored custom data
#' @param custom_stored_oag OAG of stored custom data
#' @param custom_display_type What to display for custom data
#' @param custom_display_oag Target OAG for custom display
#' @param custom_method Interpolation method for custom data
#' @param country Country code
#' @param ref_year Reference year
#'
#' @return Data frame ready for display in handsontable
#' @export
#'
prepare_population_table <- function(
  data_source = "UN Data",
  un_cache_single = NULL,
  un_cache_5yr = NULL,
  un_display_type = "Single Ages",
  custom_stored_data = NULL,
  custom_stored_type = "Single Ages",
  custom_stored_oag = 100,
  custom_display_type = "Single Ages",
  custom_display_oag = 100,
  custom_method = "beers(ord)",
  country = NULL,
  ref_year = NULL
) {
  
  if (data_source == "UN Data") {
    # Use single age cache as base and transform if needed
    base_data <- un_cache_single
    
    # Apply transformation if display type is different
    if (un_display_type == "5-Year Groups") {
      # Transform from single ages to 5-year groups
      return(transform_population(
        base_data,
        from_type = "Single Ages",
        to_type = "5-Year Groups",
        from_oag = 100,
        to_oag = 100,
        method = "un",
        country = country,
        ref_year = ref_year
      ))
    } else {
      # Single ages - return as is
      return(base_data)
    }
  } else {
    # Custom Data - may need transformation or empty template
    return(prepare_custom_table(
      data = custom_stored_data,
      from_type = custom_stored_type,
      from_oag = custom_stored_oag,
      to_type = custom_display_type,
      to_oag = custom_display_oag,
      method = custom_method,
      country = country,
      ref_year = ref_year
    ))
  }
}

#' Prepare Custom Table
#'
#' @description
#' Handles all Custom Data logic including empty templates and transformations.
#'
#' @param data Input data (can be NULL)
#' @param from_type Current age type of data
#' @param from_oag Current OAG of data
#' @param to_type Target age type for display
#' @param to_oag Target OAG for display
#' @param method Interpolation method
#' @param country Country code
#' @param ref_year Reference year
#'
#' @return Data frame ready for display
#' @export
#'
prepare_custom_table <- function(
  data, from_type, from_oag, to_type, to_oag, method, country, ref_year
) {
  
  # Validate OAG values
  if (!is.null(to_oag) && !is.na(to_oag)) {
    if (to_oag < 35 || to_oag > 100) {
      to_oag <- 100
    }
  } else {
    to_oag <- 100
  }
  
  if (!is.null(from_oag) && !is.na(from_oag)) {
    if (from_oag < 35 || from_oag > 100) {
      from_oag <- 100
    }
  } else {
    from_oag <- 100
  }
  
  # Case 1: No data - generate empty template
  if (is.null(data) || nrow(data) == 0) {
    return(generate_empty_template(to_type, to_oag))
  }
  
  # Ensure data has required columns
  if (!all(c("age", "popM", "popF") %in% names(data))) {
    return(generate_empty_template(to_type, to_oag))
  }
  
  # Case 2: Data exists - check if transformation is needed
  if (from_type == to_type && from_oag == to_oag) {
    return(data)
  }
  
  # Case 3: Need transformation
  
  tryCatch({
    result <- transform_population(
      data, from_type, to_type, from_oag, to_oag, 
      method, country, ref_year
    )
    return(result)
  }, error = function(e) {
    return(generate_empty_template(to_type, to_oag))
  })
}

#' Generate Empty Template
#'
#' @description
#' Creates an empty population data template with appropriate age structure.
#'
#' @param age_type "Single Ages" or "5-Year Groups"
#' @param oag Open age group value
#'
#' @return Data frame with age labels and NA values
#' @export
#'
generate_empty_template <- function(age_type, oag) {
  age_labels <- create_age_labels(age_type, oag)
  
  data.frame(
    age = age_labels,
    popM = NA_real_,
    popF = NA_real_,
    stringsAsFactors = FALSE
  )
}

#' Create Age Labels
#'
#' @description
#' Generates appropriate age labels based on age type and OAG.
#'
#' @param age_type "Single Ages" or "5-Year Groups"
#' @param oag Open age group value
#'
#' @return Character vector of age labels
#' @export
#'
create_age_labels <- function(age_type, oag) {
  if (age_type == "Single Ages") {
    c(as.character(0:(oag-1)), paste0(oag, "+"))
  } else {
    # 5-year groups
    starts <- seq(0, oag-5, by = 5)
    labels <- paste0(starts, "-", pmin(starts + 4, oag - 1))
    c(labels, paste0(oag, "+"))
  }
}

#' Transform Population
#'
#' @description
#' Core transformation logic for age groups and OAG adjustments.
#'
#' @param data Input data frame
#' @param from_type Source age type
#' @param to_type Target age type
#' @param from_oag Source OAG
#' @param to_oag Target OAG
#' @param method Interpolation method
#' @param country Country code
#' @param ref_year Reference year
#'
#' @return Transformed data frame
#' @importFrom OPPPserver graduate_pop sum_to_pop5 extend_oag reduce_oag
#' @export
#'
transform_population <- function(
  data, from_type, to_type, from_oag, to_oag, method, country, ref_year
) {
  
  # Make a copy to avoid modifying original
  data <- data.frame(data)
  
  # Ensure we have the expected columns in the right order
  required_cols <- c("age", "popM", "popF")
  if (!all(required_cols %in% names(data))) {
    stop(paste("[TRANSFORM-POP] Missing required columns. Found:", 
               paste(names(data), collapse=", "),
               "Expected:", paste(required_cols, collapse=", ")))
  }
  
  # Ensure standard column order
  if (inherits(data, "data.table")) {
    data <- data[, ..required_cols]
  } else {
    data <- data[, required_cols]
  }
  
  # Step 1: Age group conversion
  if (from_type != to_type) {
    data <- convert_age_groups_internal(
      data, from_type, to_type, method, country, ref_year
    )
  }
  
  # Step 2: OAG adjustment
  if (from_oag != to_oag) {
    data <- adjust_oag_internal(
      data, from_oag, to_oag, country, ref_year
    )
  }
  
  # Step 3: Ensure proper age labels match the actual data
  # The data might have a different OAG than requested if transformation didn't extend it
  actual_rows <- nrow(data)
  if (to_type == "Single Ages") {
    # For single ages: if we have 51 rows, OAG is 50 (ages 0-49 plus 50+)
    # if we have 101 rows, OAG is 100 (ages 0-99 plus 100+)
    actual_oag <- actual_rows - 1
  } else {
    # 5-year groups: calculate OAG from number of rows
    # if we have 21 rows, last group starts at (21-1)*5 = 100
    actual_oag <- (actual_rows - 1) * 5  
  }
  data$age <- create_age_labels(to_type, actual_oag)
  
  # Step 4: Ensure output maintains standard column order (age, popM, popF)
  required_cols <- c("age", "popM", "popF")
  if (inherits(data, "data.table")) {
    data <- data[, ..required_cols]
  } else {
    data <- data[, required_cols]
  }
  
  return(data)
}

#' Internal Age Group Conversion
#'
#' @description
#' Converts between single ages and 5-year groups.
#'
#' @param data Input data
#' @param from Source age type
#' @param to Target age type
#' @param method Interpolation method
#' @param country Country code
#' @param ref_year Reference year
#'
#' @return Converted data
#' @importFrom OPPPserver graduate_pop sum_to_pop5
#' @importFrom data.table as.data.table
#'
convert_age_groups_internal <- function(data, from, to, method, country, ref_year) {
  if (from == "5-Year Groups" && to == "Single Ages") {
    # Prepare data for graduate_pop (numeric ages)
    
    data$age <- extract_numeric_ages(data$age, "5-year")
    # graduate_pop can handle data.frame
    result <- graduate_pop(data, method = method, country = country, year = ref_year)
    
    
    # Ensure result is a data.frame
    as.data.frame(result)
  } else if (from == "Single Ages" && to == "5-Year Groups") {
    # Ensure numeric ages
    
    data$age <- extract_numeric_ages(data$age, "single")
    # sum_to_pop5 expects a data.table
    if (!inherits(data, "data.table")) {
      data <- data.table::as.data.table(data)
    }
    result <- sum_to_pop5(data)
    
    
    # Convert back to data.frame for consistency
    as.data.frame(result)
  } else {
    data
  }
}

#' Internal OAG Adjustment
#'
#' @description
#' Adjusts the open age group of population data.
#'
#' @param data Input data
#' @param from_oag Current OAG
#' @param to_oag Target OAG
#' @param country Country code
#' @param ref_year Reference year
#'
#' @return Data with adjusted OAG
#' @importFrom OPPPserver extend_oag reduce_oag
#'
adjust_oag_internal <- function(data, from_oag, to_oag, country, ref_year) {
  # Ensure numeric ages
  age_type <- detect_age_type(data$age)
  data$age <- extract_numeric_ages(data$age, age_type)
  
  if (from_oag < to_oag) {
    
    result <- extend_oag(data, country = country, year = ref_year, oag_new = to_oag)
    
    as.data.frame(result)
  } else if (from_oag > to_oag) {
    
    # reduce_oag expects a data.table
    if (!inherits(data, "data.table")) {
      data <- data.table::as.data.table(data)
    }
    result <- reduce_oag(data, to_oag)
    
    as.data.frame(result)
  } else {
    data
  }
}

#' Extract Numeric Ages
#'
#' @description
#' Extracts numeric age values from age labels.
#'
#' @param age_vector Vector of age labels
#' @param type "single" or "5-year"
#'
#' @return Numeric age vector
#'
extract_numeric_ages <- function(age_vector, type) {
  if (type == "5-year") {
    as.numeric(gsub("^([0-9]+).*", "\\1", age_vector))
  } else {
    as.numeric(gsub("\\+", "", age_vector))
  }
}

#' Detect Age Type
#'
#' @description
#' Detects whether age labels are single ages or 5-year groups.
#'
#' @param age_vector Vector of age labels
#'
#' @return "single" or "5-year"
#'
detect_age_type <- function(age_vector) {
  if (any(grepl("-", age_vector[1:min(5, length(age_vector))]))) {
    "5-year"
  } else {
    "single"
  }
}

#' Transform Data to Canonical Format
#'
#' @description
#' Convenience function to transform population data to canonical format
#' (Single Ages with OAG 100). This is the most common transformation pattern
#' used throughout the application for data storage.
#'
#' @param data Input data frame
#' @param from_type Source age type
#' @param from_oag Source OAG
#' @param method Interpolation method
#' @param country Country code
#' @param ref_year Reference year
#'
#' @return Data frame in canonical format (Single Ages, OAG 100)
#' @export
#'
transform_to_canonical <- function(data, from_type, from_oag, method, country, ref_year) {
  # Skip transformation if already in canonical format
  if (from_type == "Single Ages" && from_oag == 100) {
    return(data)
  }
  
  # Transform to canonical format
  transform_population(
    data,
    from_type = from_type,
    to_type = "Single Ages", 
    from_oag = from_oag,
    to_oag = 100,
    method = method,
    country = country,
    ref_year = ref_year
  )
}

#' Transform 5-Year Groups to Single Ages with Standard Parameters
#'
#' @description
#' Convenience function for the common transformation from 5-year groups to single ages
#' using UN method and standard OAG of 100. Used frequently in UN data processing.
#'
#' @param data Input data frame with 5-year groups
#' @param country Country code
#' @param ref_year Reference year
#'
#' @return Data frame with single ages, OAG 100
#' @export
#'
transform_5yr_to_single <- function(data, country, ref_year) {
  transform_population(
    data,
    from_type = "5-Year Groups",
    to_type = "Single Ages",
    from_oag = 100,
    to_oag = 100,
    method = "un",
    country = country,
    ref_year = ref_year
  )
}