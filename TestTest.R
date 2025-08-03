# TestTest.R - Standalone test for enhanced modal functionality
# Phase 0: Base Setup - Replicate existing functionality
#
# MIGRATION NOTES:
# 1. Replace COUNTRY with input$wpp_country throughout
# 2. Replace initial_un_data <- generate_sample_data() with reactive_pop()
# 3. Copy all functions except the UI and server (only modal-related code)
# 4. The enhanced create_modal_ui replaces the original in app_handles.R

# Load required libraries
library(shiny)
library(shiny.semantic)
library(rhandsontable)
library(shinyjs)
library(OPPPserver)  # For graduate_pop, extend_oag, reduce_oag functions
library(tibble)  # For tibble conversions

# Utility operator for default values
`%||%` <- function(x, y) if (is.null(x)) y else x

# Configuration constants
# In production, this will be replaced with input$wpp_country
COUNTRY <- "Spain"

# Copy of existing create_modal_ui function - EXACT copy from original
create_modal_ui <- function(modal_id, header_title, output_id, file_input_id, download_button_id, hide_button_id, additional_header = NULL, i18n = NULL) {
  modal(
    id = modal_id,
    header = div(
      div(
        style = "display: flex; justify-content: space-between;",
        header_title,
        action_button("customize_help", if (!is.null(i18n)) i18n$t("Instructions") else "Instructions", class = "ui red button")
      ),
      # Phase 1: Add data source toggle - using Semantic UI
      div(
        class = "ui two item menu",
        style = "margin-top: 15px;",
        tags$a(
          id = paste0(modal_id, "_source_un"),
          class = "item active",
          onclick = paste0("$('#", modal_id, "_source_un').addClass('active'); $('#", modal_id, "_source_custom').removeClass('active'); Shiny.setInputValue('", modal_id, "_source', 'UN Data');"),
          icon("database"),
          "UN Data"
        ),
        tags$a(
          id = paste0(modal_id, "_source_custom"),
          class = "item",
          onclick = paste0("$('#", modal_id, "_source_custom').addClass('active'); $('#", modal_id, "_source_un').removeClass('active'); Shiny.setInputValue('", modal_id, "_source', 'Custom Data');"),
          icon("upload"),
          "Custom Data"
        )
      ),
      # Unified Data Configuration Accordion
      div(
        id = paste0(modal_id, "_data_config_accordion"),
        class = "ui styled accordion",
        style = "margin-top: 15px; width: 100%;",
        div(
          class = "title",
          style = "background-color: #f8f8f9; padding: 12px;",
          icon("caret right"),
          tags$span(style = "font-weight: 600; margin-left: 8px;", "Data Configuration")
        ),
        div(
          class = "content",
          style = "padding: 15px;",
          # Dynamic content based on data source
          conditionalPanel(
            condition = paste0("input.", modal_id, "_source == null || input.", modal_id, "_source == 'UN Data'"),
            div(
              class = "ui form",
              div(
                class = "field",
                style = "width: 200px;",
                tags$label(style = "font-size: 0.9em;", "Age Type"),
                selectInput(
                  paste0(modal_id, "_un_age_type"),
                  NULL,
                  choices = c("Single Ages", "5-Year Groups"),
                  selected = "Single Ages",
                  width = "100%"
                )
              )
            )
          ),
          conditionalPanel(
            condition = paste0("input.", modal_id, "_source == 'Custom Data'"),
            div(
              style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
              # Left column - Data Structure
              div(
                div(
                  class = "ui form",
                  h5(class = "ui header", style = "margin-top: 0;", "Data Structure"),
                  div(
                    style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px;",
                    div(
                      class = "field",
                      style = "margin-bottom: 0;",
                      tags$label(style = "font-size: 0.9em;", "Age Type"),
                      selectInput(
                        paste0(modal_id, "_age_type"),
                        NULL,
                        choices = c("Single Ages", "5-Year Groups"),
                        selected = "Single Ages",
                        width = "100%"
                      )
                    ),
                    div(
                      class = "field",
                      style = "margin-bottom: 0;",
                      tags$label(
                        style = "font-size: 0.9em;", 
                        "Open Age Group",
                        tags$i(
                          class = "info circle icon",
                          style = "margin-left: 5px; color: #999; cursor: help;",
                          `data-content` = "The oldest age group that will aggregate all ages above it (e.g., 85+ means all people 85 and older)",
                          `data-variation` = "tiny"
                        ),
                        tags$span(
                          id = paste0(modal_id, "_oag_status"),
                          style = "margin-left: 10px; font-size: 0.8em; color: #e74c3c; display: none;",
                          "≥35"
                        )
                      ),
                      numericInput(
                        paste0(modal_id, "_oag"),
                        NULL,
                        value = 100,
                        min = 35,
                        max = 120,
                        step = 5,
                        width = "100%"
                      )
                    )
                  )
                ),
                # Always show date below the grid
                div(
                  class = "ui form",
                  style = "margin-top: 12px;",
                  div(
                    class = "field",
                    tags$label(
                      style = "font-size: 0.9em;", 
                      "Reference Date",
                      tags$i(
                        class = "info circle icon",
                        style = "margin-left: 5px; color: #999; cursor: help;",
                        `data-content` = "The date your population data represents. Used to align projections to specific time points",
                        `data-variation` = "tiny"
                      )
                    ),
                    dateInput(
                      paste0(modal_id, "_ref_date"),
                      NULL,
                      value = paste0(format(Sys.Date(), "%Y"), "-12-31"),
                      width = "100%"
                    )
                  )
                )
              ),
              # Right column - Processing Methods
              div(
                div(
                  class = "ui form",
                  h5(class = "ui header", style = "margin-top: 0;", "Processing Methods"),
                  # Show interpolation options only for 5-Year Groups
                  conditionalPanel(
                    condition = paste0("input.", modal_id, "_age_type == '5-Year Groups'"),
                    div(
                      class = "field",
                      style = "margin-bottom: 10px;",
                      tags$label(
                        style = "font-size: 0.9em;", 
                        "Interpolation",
                        tags$i(
                          class = "info circle icon",
                          style = "margin-left: 5px; color: #999; cursor: help;",
                          `data-content` = "Method to convert 5-year age groups into single ages. Beers Ordinary is recommended for demographic data",
                          `data-variation` = "tiny"
                        )
                      ),
                      selectInput(
                        paste0(modal_id, "_interp_method"),
                        NULL,
                        choices = c(
                          "Sprague" = "sprague",
                          "Beers Ordinary" = "beers(ord)",
                          "Beers Modified" = "beers(mod)",
                          "Grabill" = "grabill",
                          "Monotonic Spline" = "mono",
                          "Uniform" = "uniform",
                          "PCLM" = "pclm"
                        ),
                        selected = "beers(ord)",
                        width = "100%"
                      )
                    )
                  ),
                  # Show note when extrapolation will be applied
                  conditionalPanel(
                    condition = paste0("(input.", modal_id, "_age_type == 'Single Ages' && input.", modal_id, "_oag != 100) || (input.", modal_id, "_age_type == '5-Year Groups' && input.", modal_id, "_oag != 100)"),
                    div(
                      style = "color: #666; font-size: 0.9em; padding: 10px; background-color: #fef9e7; border-radius: 4px; margin-top: 15px;",
                      icon("info circle"),
                      "OAG will be automatically extended to 100+"
                    )
                  ),
                  # Show message when no extrapolation needed
                  conditionalPanel(
                    condition = paste0("input.", modal_id, "_age_type == '5-Year Groups' && input.", modal_id, "_oag == 100"),
                    div(
                      style = "color: #666; font-size: 0.9em; padding: 10px; background-color: #f8f8f9; border-radius: 4px; margin-top: 15px;",
                      icon("info circle"),
                      "No extrapolation needed for OAG 100+"
                    )
                  ),
                  # Show message when no processing needed at all
                  conditionalPanel(
                    condition = paste0("input.", modal_id, "_age_type == 'Single Ages' && input.", modal_id, "_oag == 100"),
                    div(
                      style = "color: #666; font-size: 0.9em; padding: 10px; background-color: #f8f8f9; border-radius: 4px;",
                      icon("info circle"),
                      "No processing required for single ages with OAG 100+"
                    )
                  )
                )
              )
            )
          )  # End of conditionalPanel for Custom Data
        )  # End of accordion content
      )  # End of unified accordion
    ),
    # World-class UI/UX notice about data format
    div(
      class = "ui message",
      style = "margin: 0 0 10px 0; background-color: #f3f4f6; border-left: 4px solid #3b82f6;",
      div(
        style = "display: flex; align-items: flex-start; gap: 12px;",
        icon("info circle", style = "color: #3b82f6; font-size: 20px; margin-top: 2px;"),
        div(
          div(
            style = "font-weight: 600; color: #1f2937; margin-bottom: 4px;",
            "Important: Population values are in thousands"
          ),
          div(
            style = "color: #6b7280; font-size: 14px; line-height: 1.5;",
            "Enter values without commas or decimal points • For 1 million people, enter 1000 • For 500,000 people, enter 500"
          )
        )
      )
    ),
    rHandsontableOutput(output_id),
    footer = div(
      div(
        class = "footer-container",
        div(
          class = "button-container",
          div(
            style = "display: flex; gap: 5px",
            shiny::downloadButton(download_button_id, if (!is.null(i18n)) i18n$t("Download") else "Download", class = "ui blue button"),
            actionButton(paste0(modal_id, "_ok_btn"), if (!is.null(i18n)) i18n$t("Apply") else "Apply", class = "ui positive button")
          )
        ),
      ),
      br(),
      div(i18n$t("Edit the table or paste your own data form an Excel sheet. Click on the 'Apply' button to save your changes."), style = "color: #8B0000; font-weight: bold; font-size: 12px;")
    ),
    class = "small scrolling"
  )
}

# Generate sample UN population data
generate_sample_data <- function(n = 1, year = 2024) {
  # Use real WPP data
  get_wpp_pop(COUNTRY, year = year, n = n)
}

# Phase 2: Core Transformation Function
transform_population_data <- function(data, age_type, oag_current, oag_target = 100, 
                                    interp_method = "beers(ord)", ref_year = NULL) {
  print("LOG: Entered transform_population_data function")
  # Step 1: Validate and standardize column names
  print("LOG: About to validate data")
  data <- validate_population_data(data)
  print(paste("LOG: After validation: nrow =", nrow(data)))
  
  # Step 2: Graduate from 5-year to 1-year if needed
  if (age_type == "5-Year Groups") {
    print("LOG: Processing 5-year groups")
    # For graduate_pop, we need numeric age values (0, 5, 10, etc.)
    # Extract starting age from labels like "0-4", "5-9", etc.
    print("LOG: Extracting age starts")
    age_starts <- as.numeric(gsub("^([0-9]+).*", "\\1", data$age))
    data$age <- age_starts
    print(paste("LOG: Age starts:", paste(head(age_starts), collapse=", ")))
    # graduate_pop will graduate all non-age columns
    print("LOG: About to call graduate_pop")
    data <- tibble::as_tibble(graduate_pop(data, method = interp_method))
    print(paste("LOG: After graduation: nrow =", nrow(data)))
  }
  
  # Step 3: Adjust OAG if needed
  if (oag_current != oag_target) {
    print(paste("LOG: Need OAG adjustment from", oag_current, "to", oag_target))
    data <- adjust_oag(data, oag_current, oag_target, ref_year = ref_year)
    print(paste("LOG: After OAG adjustment: nrow =", nrow(data)))
  } else {
    print("LOG: No OAG adjustment needed")
  }
  
  print("LOG: About to return tibble")
  # Ensure return is a tibble for consistency
  result <- tibble::as_tibble(data)
  print("LOG: Returning from transform_population_data")
  result
}

# Phase 3: Simple Validation Function
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

# Phase 4: OAG Adjustment Helper
# In production, country parameter will receive input$wpp_country
adjust_oag <- function(data, oag_current, oag_target = 100, country = COUNTRY, ref_year = NULL) {
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
  
  if (oag_current < oag_target) {
    result <- extend_oag(data, country = country, year = year_to_use, oag_new = oag_target)
    tibble::as_tibble(result)
  } else if (oag_current > oag_target) {
    tibble::as_tibble(reduce_oag(data, oag_target))
  } else {
    tibble::as_tibble(data)
  }
}

# Phase 4: Helper function to generate age labels
generate_age_labels <- function(age_type, oag) {
  if (age_type == "Single Ages") {
    # Single year ages: 0, 1, 2, ..., oag+
    ages <- c(0:(oag-1), paste0(oag, "+"))
  } else {
    # 5-year age groups: 0-4, 5-9, ..., oag+
    age_starts <- seq(0, oag-1, by = 5)
    age_labels <- c()
    
    for (start in age_starts) {
      if (start + 4 < oag) {
        age_labels <- c(age_labels, paste0(start, "-", start + 4))
      } else if (start < oag) {
        # Last group before OAG
        age_labels <- c(age_labels, paste0(start, "-", oag - 1))
      }
    }
    ages <- c(age_labels, paste0(oag, "+"))
  }
  return(ages)
}

# Format numeric ages from WPP to label format for 5-year groups
format_5year_age_labels <- function(ages) {
  # Convert numeric ages (0, 5, 10...) to labels ("0-4", "5-9"...)
  labels <- character(length(ages))
  
  for (i in seq_along(ages)) {
    age <- ages[i]
    if (i == length(ages)) {
      # Last age is OAG
      labels[i] <- paste0(age, "+")
    } else {
      # Regular 5-year group
      labels[i] <- paste0(age, "-", age + 4)
    }
  }
  
  return(labels)
}

# Initialize population modal accordion - reusable function for migration
initialize_population_modal_accordion <- function(modal_id, initial_state = FALSE) {
  js_code <- paste0("
    // Initialize the single accordion
    $('#", modal_id, "_data_config_accordion').accordion({
      exclusive: false,
      animateChildren: false,
      duration: 200,
      active: ", if(initial_state) "0" else "false", ",
      onOpen: function() {
        // Update state and refresh modal
        Shiny.setInputValue('accordion_data_config_open', true);
        $('#", modal_id, "').modal('refresh');
      },
      onClose: function() {
        // Update state and refresh modal
        Shiny.setInputValue('accordion_data_config_open', false);
        $('#", modal_id, "').modal('refresh');
      }
    });
    
    // Initialize popups for info icons
    $('.info.circle.icon').popup({
      hoverable: true,
      position: 'top center',
      delay: {
        show: 300,
        hide: 0
      }
    });
  ")
  
  return(js_code)
}

# Shiny UI
ui <- semanticPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      /* Custom CSS for modal centering - ONLY what's in original */
      .footer-container {
          display: flex;
          gap: 10px;
          justify-content: center;
      }
      
      .file-input-container, .button-container {
          flex: 0;
      }
      
      .button-container > div {
          display: flex;
          gap: 5px;
          justify-content: center;
      }
      
      /* Phase 3: Fix dropdown z-index */
      .ui.segment {
          position: relative;
          z-index: 100;
      }
    "))
  ),
  div(class = "ui container",
    h1("Modal Enhancement Test"),
    br(),
    action_button("customize_pop", "Customize Population", class = "ui blue button"),
    br(), br(),
    div(id = "result_display",
      h3("Current Data:"),
      verbatimTextOutput("current_data"),
      br(),
      h3("Full Data Frame:"),
      verbatimTextOutput("full_data_display")
    ),
    # Modal will be rendered dynamically
    uiOutput("popup_pop")
  )
)

# Shiny Server
server <- function(input, output, session) {
  # Create mock i18n object
  i18n <- list(
    t = function(text) text,
    translate = function(text) text
  )
  
  # Reactive values
  # In production, replace with: reactive_pop()
  initial_un_data <- generate_sample_data()  # Placeholder for reactive_pop()
  
  current_pop_data <- reactiveVal(initial_un_data)
  un_data <- reactiveVal(initial_un_data)  # Store UN data separately
  un_data_5yr <- reactiveVal(NULL)  # Cache for 5-year grouped data
  custom_data <- reactiveVal(NULL)  # Store custom data separately
  
  # Render the modal UI dynamically
  output$popup_pop <- renderUI({
    # Get reference year for title
    ref_date <- input$modal_population_ref_date
    ref_year <- if (!is.null(ref_date)) {
      if (is.character(ref_date)) format(as.Date(ref_date), "%Y")
      else if (inherits(ref_date, "Date")) format(ref_date, "%Y")
      else "2024"
    } else "2024"
    
    create_modal_ui(
      modal_id = "modal_population",
      header_title = paste("Population data for", COUNTRY, "in base year", ref_year),
      output_id = "tmp_pop_dt",
      file_input_id = "upload_pop",
      download_button_id = "download_pop",
      hide_button_id = "hide_pop",
      additional_header = NULL,
      i18n = i18n
    )
  })
  
  # Show modal when customize button clicked
  observeEvent(input$customize_pop, {
    print("LOG: Customize button clicked")
    show_modal("modal_population")
    # Set initial value for data source
    session$sendCustomMessage(type = "shinyjs-delay", 
      message = list(delay = 100, 
        code = "Shiny.setInputValue('modal_population_source', 'UN Data');"))
  })
  
  # Clear 5-year cache when reference date changes
  observeEvent(input$modal_population_ref_date, {
    un_data_5yr(NULL)  # Reset cache so it fetches fresh data
  })
  
  # Update OAG display and validation
  observeEvent(input$modal_population_oag, {
    oag <- input$modal_population_oag
    if (!is.null(oag)) {
      # Check if OAG is valid
      if (is.na(oag) || oag < 35) {
        shinyjs::show("modal_population_oag_status")
      } else {
        shinyjs::hide("modal_population_oag_status")
        shinyjs::html(id = "modal_population_oag_display", html = as.character(oag))
      }
    }
  })
  
  # Add reactive value to track accordion state
  accordion_state <- reactiveVal(list(data_config = FALSE))
  
  # Track accordion state changes
  observeEvent(input$accordion_data_config_open, {
    current <- accordion_state()
    current$data_config <- input$accordion_data_config_open
    accordion_state(current)
  })
  
  # Initialize accordion when modal opens
  observeEvent(input$customize_pop, {
    shinyjs::delay(200, {  # Wait for modal to render
      # Get saved state
      state <- accordion_state()
      open_state <- if(state$data_config) "0" else ""
      
      # Use the extracted function for accordion initialization
      js_code <- initialize_population_modal_accordion("modal_population", state$data_config)
      shinyjs::runjs(js_code)
    })
  })
  
  # Re-initialize popups when switching between data sources
  observeEvent(input$modal_population_source, {
    print(paste("LOG: Data source changed to:", input$modal_population_source))
    # Re-initialize popups after content change
    shinyjs::delay(50, {
      shinyjs::runjs("
        // Re-initialize popups for new content
        $('.info.circle.icon').popup({
          hoverable: true,
          position: 'top center',
          delay: {
            show: 300,
            hide: 0
          }
        });
      ")
    })
  })
  
  
  # Render the rhandsontable
  output$tmp_pop_dt <- renderRHandsontable({
    # Phase 2: Make table reactive to data source
    data_source <- input$modal_population_source
    
    if (is.null(data_source) || data_source == "UN Data") {
      # Show UN data based on age type selection
      un_age_type <- input$modal_population_un_age_type %||% "Single Ages"
      
      if (un_age_type == "Single Ages") {
        # Use the pre-loaded single age data (no API call needed)
        res <- un_data()
      } else {
        # 5-Year Groups - check cache first
        if (is.null(un_data_5yr())) {
          # Only fetch if not cached
          ref_date <- input$modal_population_ref_date
          ref_year <- if (!is.null(ref_date)) {
            if (is.character(ref_date)) as.numeric(format(as.Date(ref_date), "%Y"))
            else if (inherits(ref_date, "Date")) as.numeric(format(ref_date, "%Y"))
            else 2024
          } else 2024
          
          # Fetch and cache the 5-year grouped data
          data_5yr <- get_wpp_pop(COUNTRY, year = ref_year, n = 5)
          data_5yr$age <- round(data_5yr$age)  # Clean up decimal ages
          # Format ages to match custom data pattern
          data_5yr$age <- format_5year_age_labels(data_5yr$age)
          un_data_5yr(data_5yr)
        }
        res <- un_data_5yr()
      }
      
      names(res) <- c("Age", "Female", "Male")
    } else {
      # Custom data mode
      age_type <- input$modal_population_age_type
      oag <- input$modal_population_oag
      
      # Use defaults if not yet set
      if (is.null(age_type)) age_type <- "Single Ages"
      if (is.null(oag) || is.na(oag) || oag < 35) {
        # Return empty table with error message if OAG is invalid
        return(rhandsontable(
          data.frame(
            Age = "Invalid OAG value",
            Female = NA,
            Male = NA,
            stringsAsFactors = FALSE
          ),
          rowHeaders = NULL,
          readOnly = TRUE,
          stretchH = "all",
          height = 100
        ))
      }
      
      # Check if we have saved custom data with matching configuration
      saved_custom <- custom_data()
      if (!is.null(saved_custom) && 
          nrow(saved_custom) == length(generate_age_labels(age_type, oag))) {
        # Use saved custom data
        res <- saved_custom
        names(res) <- c("Age", "Female", "Male")
      } else {
        # Generate empty template
        age_labels <- generate_age_labels(age_type, oag)
        res <- data.frame(
          Age = age_labels,
          Female = NA_real_,
          Male = NA_real_,
          stringsAsFactors = FALSE
        )
      }
    }
    
    rhandsontable(res, 
                  rowHeaders = NULL,
                  useTypes = TRUE,
                  stretchH = "all",
                  height = 400,
                  minSpareRows = 0,
                  maxRows = nrow(res)) %>%
      hot_col("Age", readOnly = TRUE) %>%
      hot_col(c("Female", "Male"), type = "numeric", format = "0,0")
  })
  
  # Handle Apply button
  observeEvent(input$modal_population_ok_btn, {
    print("LOG: Apply button clicked")
    req(input$tmp_pop_dt)
    
    tryCatch({
      print("LOG: Getting data from table")
      # Get data from table
      data <- hot_to_r(input$tmp_pop_dt)
      print(paste("LOG: Got data with", nrow(data), "rows"))
      print("LOG: Checking data source")
      print(paste("LOG: modal_population_source =", input$modal_population_source))
      
      # Apply transformations if using custom data
      if (!is.null(input$modal_population_source) && input$modal_population_source == "Custom Data") {
        print("LOG: Confirmed Custom Data mode")
        # Get transformation parameters with defaults
        print("LOG: Getting age_type")
        age_type <- input$modal_population_age_type %||% "Single Ages"
        print(paste("LOG: age_type =", age_type))
        print("LOG: Getting oag_current")
        oag_current <- input$modal_population_oag %||% 100
        print(paste("LOG: oag_current =", oag_current))
        print("LOG: Getting interp_method")
        interp_method <- input$modal_population_interp_method %||% "beers(ord)"
        print(paste("LOG: interp_method =", interp_method))
        
        # Extract year from reference date
        print("LOG: Getting ref_date")
        ref_year <- NULL  # Initialize ref_year
        tryCatch({
          ref_date <- input$modal_population_ref_date
          print(paste("LOG: ref_date =", as.character(ref_date)))
          print(paste("LOG: ref_date class =", paste(class(ref_date), collapse = " ")))
          if (!is.null(ref_date)) {
            print("LOG: ref_date is not null, attempting to extract year")
            # If ref_date is a string, convert to Date first
            if (is.character(ref_date)) {
              ref_date_obj <- as.Date(ref_date)
              ref_year <- as.numeric(format(ref_date_obj, "%Y"))
            } else if (inherits(ref_date, "Date")) {
              ref_year <- as.numeric(format(ref_date, "%Y"))
            }
            print(paste("LOG: ref_year =", ref_year))
          } else {
            print("LOG: ref_date is null")
            ref_year <- NULL
          }
        }, error = function(e) {
          print(paste("LOG: ERROR getting ref_date:", e$message))
          ref_year <- NULL
        })
        
        print("LOG: Entering custom data transformation")
        print(paste("LOG: Parameters - Age type:", age_type, "| OAG:", oag_current, "| Method:", interp_method))
        print("LOG: About to call transform_population_data")
        
        # Apply transformation pipeline with detailed error handling
        tryCatch({
          print("LOG: Inside tryCatch for transformation")
          data <- transform_population_data(
            data, 
            age_type = age_type,
            oag_current = oag_current,
            oag_target = 100,  # Always target 100 for consistency
            interp_method = interp_method,
            ref_year = ref_year
          )
          print("Transformation completed successfully")
        }, error = function(e) {
          print(paste("ERROR in transformation pipeline:", e$message))
          print(paste("Call stack:", paste(deparse(e$call), collapse = " ")))
          stop(e)
        })
        
        # Save the raw custom data (before transformation) for later use
        raw_data <- hot_to_r(input$tmp_pop_dt)
        names(raw_data) <- c("age", "popF", "popM")
        custom_data(raw_data)
        
      } else {
        print("LOG: Processing UN Data mode")
        # For UN Data, check if transformation is needed
        un_age_type <- input$modal_population_un_age_type %||% "Single Ages"
        
        if (un_age_type == "5-Year Groups") {
          print("LOG: UN Data with 5-year groups - needs transformation")
          # Get reference year
          ref_date <- input$modal_population_ref_date
          ref_year <- if (!is.null(ref_date)) {
            if (is.character(ref_date)) as.numeric(format(as.Date(ref_date), "%Y"))
            else if (inherits(ref_date, "Date")) as.numeric(format(ref_date, "%Y"))
            else 2024
          } else 2024
          
          # Transform 5-year to single ages using same pipeline
          names(data) <- c("age", "popF", "popM")
          data <- transform_population_data(
            data,
            age_type = "5-Year Groups",
            oag_current = 100,  # WPP data always has OAG 100+
            oag_target = 100,
            interp_method = "beers(ord)",  # Default method
            ref_year = ref_year
          )
        } else {
          # Single ages - just standardize column names
          names(data) <- c("age", "popF", "popM")
        }
      }
      
      print("LOG: Updating reactive value")
      # Update reactive value (this is the final data used by the app)
      current_pop_data(data)
      
      print("LOG: Closing modal")
      # Close modal
      hide_modal("modal_population")
      
      print("LOG: Showing success notification")
      # Show success message
      showNotification("Data updated successfully", type = "message", duration = 3)
      print("LOG: Apply button processing complete")
      
    }, error = function(e) {
      print("LOG: ERROR in Apply button handler")
      print(paste("LOG: Error message:", e$message))
      print(paste("LOG: Error class:", class(e)))
      print("LOG: Error traceback:")
      print(traceback())
      # Show error message to user
      showNotification(
        paste("Error:", e$message), 
        type = "error", 
        duration = 10
      )
    })
  })
  
  # Handle download
  output$download_pop <- downloadHandler(
    filename = function() paste0("population_test_", Sys.Date(), ".csv"),
    content = function(file) {
      # Download exactly what's visible in the table
      req(input$tmp_pop_dt)
      data <- hot_to_r(input$tmp_pop_dt)
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Display current data summary
  output$current_data <- renderPrint({
    data <- current_pop_data()
    cat("Total Population:", sum(data$popF) + sum(data$popM), "thousands\n")
    cat("Male:", sum(data$popM), "thousands\n")
    cat("Female:", sum(data$popF), "thousands\n")
    cat("Age groups:", nrow(data), "\n")
  })
  
  # Display full data frame
  output$full_data_display <- renderPrint({
    data <- current_pop_data()
    print(as.data.frame(data))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
