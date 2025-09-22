# File validation rules for each data type
validation_rules <- list(
  pop = list(
    expected_cols = 3,
    col_types = c("numeric", "numeric", "numeric"),
    col_names = c("age", "popF", "popM")
  ),
  tfr = list(
    expected_cols = 2,
    col_types = c("numeric", "numeric"),
    col_names = c("year", "tfr")
  ),
  e0 = list(
    expected_cols = 3,
    col_types = c("numeric", "numeric", "numeric"),
    col_names = c("year", "e0M", "e0F")
  ),
  mig = list(
    expected_cols = 2,
    col_types = c("numeric", "numeric"),
    col_names = c("year", "mig")
  )
)

data_source <- shiny::reactiveValues(
  tfr = "downloaded", # "downloaded" or "uploaded"
  e0 = "downloaded",
  mig = "downloaded"
)


# Generic validation functions
check_column_count <- function(data, expected_cols) {
  ncol(data) == expected_cols
}

check_column_types <- function(data, expected_types) {
  actual_types <- sapply(data, class)
  all(mapply(function(actual, expected) {
    if (expected == "numeric") {
      return(is.numeric(data[[actual]]))
    }
    if (expected == "character") {
      return(is.character(data[[actual]]))
    }
    return(FALSE)
  }, names(data), expected_types))
}

# Main file parser function
file_parser <- function(file, data_type, i18n = NULL) {
  if (!data_type %in% names(validation_rules)) {
    return(list(
      success = FALSE,
      message = if (!is.null(i18n)) i18n$t("Unknown data type") else "Unknown data type"
    ))
  }

  rules <- validation_rules[[data_type]]

  tryCatch(
    {
      # Try reading the CSV
      data <- readr::read_csv(file$datapath)

      # Check number of columns
      if (!check_column_count(data, rules$expected_cols)) {
        return(list(
          success = FALSE,
          message = sprintf(
            if (!is.null(i18n)) i18n$t("Expected %d columns but found %d") else "Expected %d columns but found %d",
            rules$expected_cols, ncol(data)
          )
        ))
      }

      # Check column types
      if (!check_column_types(data, rules$col_types)) {
        return(list(
          success = FALSE,
          message = if (!is.null(i18n)) i18n$t("One or more columns have incorrect data types") else "One or more columns have incorrect data types"
        ))
      }

      # If all checks pass
      return(list(
        success = TRUE,
        message = if (!is.null(i18n)) i18n$t("Validation successful") else "Validation successful",
        data = data
      ))
    },
    error = function(e) {
      return(list(
        success = FALSE,
        message = paste(if (!is.null(i18n)) i18n$t("Error reading file:") else "Error reading file:", e$message)
      ))
    }
  )
}

#' The Application Server-Side Logic
#'
#' This function defines the server logic for the Shiny application,
#' managing data processing, UI rendering, and routing.
#'
#' @param input,output,session Internal parameters for `\{shiny\}`.
#'
#' @importFrom shiny reactive reactiveVal observeEvent renderUI observe downloadHandler
#' @importFrom shinyjs hide show
#' @importFrom utils read.csv
#' @importFrom untheme detect_font_size
#' @importFrom shiny.i18n update_lang
#' @importFrom shiny.semantic multiple_radio
#' @importFrom OPPPserver get_wpp_pop get_wpp_tfr get_wpp_e0 get_wpp_mig
#' @importFrom rintrojs introjs
#' @export
#'
app_server <- function(input, output, session) {
  cat("[PHASE1] Sidebar structure created\n")
  
  i18n <- usei18n_local()
  
  # Track the current visible page
  current_tab <- reactiveVal()
  # Track whether user completed the Input step by clicking Next
  input_next_clicked <- reactiveVal(FALSE)
  
  # Store a desired page when navigating from contexts where current_tab may
  # not yet be initialized
  pending_page <- reactiveVal(NULL)

  # Helper to navigate to a specific page and keep sidebar state consistent
  go_to_page <- function(page_id) {
    all_pages <- c("landing_page", "input_page", "pop_page", "tfr_page", "e0_page", "mig_page", "forecast_page", "results_page")
    for (p in all_pages) {
      shinyjs::hide(p)
    }
    if (identical(page_id, "landing_page")) {
      shinyjs::hide("left_menu")
      shinyjs::show("landing_page")
    } else {
      shinyjs::show("left_menu")
      shinyjs::show(page_id)
    }
    # Debug state when landing on input page
    if (identical(page_id, "input_page")) {
      sim_name <- tryCatch({ simulations$current }, error = function(e) NULL)
      has_results <- tryCatch({
        if (!is.null(sim_name)) file.exists(file.path(sim_base_dir, sim_name, "results", "results.rds")) else FALSE
      }, error = function(e) FALSE)
      next_clicked <- tryCatch({ isTRUE(input_next_clicked()) }, error = function(e) FALSE)
      restoring <- tryCatch({ isTRUE(restoring_inputs()) }, error = function(e) FALSE)
      rloc <- tryCatch({ restored_location() }, error = function(e) NULL)
      tog <- tryCatch({ input$toggle_region }, error = function(e) NULL)
      cnt <- tryCatch({ input$wpp_country }, error = function(e) NULL)
      sy <- tryCatch({ input$wpp_starting_year }, error = function(e) NULL)
      ey <- tryCatch({ input$wpp_ending_year }, error = function(e) NULL)
      cat("[INPUT_PAGE_DEBUG] Arrived at input_page | sim:", sim_name, "| has_results:", has_results,
          "| next_clicked:", next_clicked, "| restoring_inputs:", restoring,
          "| restored_location:", ifelse(is.null(rloc), "<NULL>", rloc),
          "| toggle:", tog, "| wpp_country:", cnt, "| start:", sy, "| end:", ey, "\n")
    }
    # Defer setting current_tab until it's initialized
    pending_page(page_id)
    # Persist the page choice for this simulation
    save_current_page(page_id)
  }

  # Sidebar should not be visible on landing page
  shinyjs::hide("left_menu")

  # --- Phase 2: Session-specific temp directory + simulations reactive ---
  # Setup session-specific directory under /tmp/hasdaney213/{session_token}/
  sim_base_dir <- setup_session_dir(session)
  cat("[PHASE2] Session directory created:", dir.exists(sim_base_dir), "\n")

  simulations <- reactiveValues(
    current = NULL,
    data = list()
  )
  sim_draft_name <- reactiveVal(NULL)
  pending_sim_removal <- reactiveVal(NULL)

  # Track last computed signature to invalidate results when inputs change
  last_computed_signature <- reactiveVal(NULL)

  # --- Phase 3: helpers to save metadata ---
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

  # Create a simple signature for current input selection
  compute_input_signature <- function(agg, loc, sy, ey) {
    paste0(agg %||% "", "|", loc %||% "", "|", sy %||% "", "|", ey %||% "")
  }

  # Process population data through transformation pipeline (reusing Apply button logic)
  process_population_data <- function(raw_data, params, country, ref_year) {
    if (is.null(raw_data) || is.null(params)) return(raw_data)
    
    data_source <- params$data_source %||% "UN Data"
    
    if (data_source == "UN Data") {
      # UN Data mode - same logic as Apply button
      un_age_type <- params$age_type %||% "Single Ages"
      
      if (un_age_type == "Single Ages") {
        # Already single ages - use as-is
        final_data <- raw_data
      } else {
        # 5-year groups - transform to single ages for downstream use
        final_data <- tryCatch({
          transform_5yr_to_single(
            raw_data,
            country = country,
            ref_year = ref_year
          )
        }, error = function(e) {
          cat("[PROCESS_DATA] Error in transform_5yr_to_single:", e$message, "\n")
          # Fallback: return raw data
          raw_data
        })
      }
    } else {
      # Custom Data mode - same logic as Apply button
      current_age_type <- params$age_type %||% "Single Ages"
      current_oag <- params$open_age %||% 100
      selected_method <- params$interp_method %||% "beers(ord)"
      
      # Transform to canonical format (single ages, OAG 100) for downstream processing
      if (current_age_type != "Single Ages" || current_oag != 100) {
        final_data <- tryCatch({
          transform_to_canonical(
            raw_data,
            from_type = current_age_type,
            from_oag = current_oag,
            method = selected_method,
            country = country,
            ref_year = ref_year
          )
        }, error = function(e) {
          cat("[PROCESS_DATA] Error in transform_to_canonical:", e$message, "\n")
          # Fallback: return raw data
          raw_data
        })
      } else {
        final_data <- raw_data
      }
    }
    
    return(final_data)
  }

  ensure_sim_dirs <- function(base_dir, sim_name) {
    sim_dir <- file.path(base_dir, sim_name)
    inputs_dir <- file.path(sim_dir, "inputs")
    results_dir <- file.path(sim_dir, "results")
    if (!dir.exists(sim_dir)) dir.create(sim_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(inputs_dir)) dir.create(inputs_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
    sim_dir
  }

  # --- Signature tracking for gating ---
  compute_input_signature <- function(agg, loc, sy, ey) {
    paste0(agg %||% "", "|", loc %||% "", "|", sy %||% "", "|", ey %||% "")
  }
  sim_saved_signature <- reactiveVal(NULL)
  sim_progressed <- reactiveVal(FALSE)
  results_invalidated <- reactiveVal(0)  # Trigger to force sidebar nav refresh

  # Save only the current page into the simulation metadata
  save_current_page <- function(page_id) {
    sim_name <- simulations$current
    if (is.null(sim_name) || !nzchar(sim_name)) return(invisible(NULL))
    sim_dir <- ensure_sim_dirs(sim_base_dir, sim_name)
    meta_path <- file.path(sim_dir, "metadata.json")
    meta <- list()
    if (file.exists(meta_path)) {
      meta <- tryCatch({ jsonlite::read_json(meta_path, simplifyVector = TRUE) }, error = function(e) list())
    }
    meta$current_page <- page_id
    meta$updated_at <- as.character(Sys.time())
    writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE, na = "null"), meta_path, useBytes = TRUE)
  }

  save_sim_metadata <- function(trigger = NULL) {
    sim_name <- simulations$current
    if (is.null(sim_name) || !nzchar(sim_name)) return(invisible(NULL))
    sim_dir <- ensure_sim_dirs(sim_base_dir, sim_name)
    meta_path <- file.path(sim_dir, "metadata.json")
    # read existing to preserve created_at
    created_at <- as.character(Sys.time())
    if (file.exists(meta_path)) {
      try({
        prev <- jsonlite::read_json(meta_path, simplifyVector = TRUE)
        if (!is.null(prev$created_at)) created_at <- as.character(prev$created_at)
      }, silent = TRUE)
    }
    meta <- list(
      name = sim_name,
      created_at = created_at,
      updated_at = as.character(Sys.time()),
      last_trigger = trigger %||% "unknown",
      country = input$wpp_country %||% NULL,
      start_year = tryCatch({ wpp_starting_year() }, error = function(e) NULL),
      end_year = tryCatch({ wpp_ending_year() }, error = function(e) NULL),
      data_sources = list(
        population = tryCatch({ pop_data_source() }, error = function(e) NULL),
        tfr = data_source$tfr %||% NULL,
        e0 = data_source$e0 %||% NULL,
        mig = data_source$mig %||% NULL
      ),
      aggregation = input$toggle_region %||% NULL,
      input_signature = compute_input_signature(input$toggle_region, input$wpp_country,
                                                tryCatch({ wpp_starting_year() }, error = function(e) NULL),
                                                tryCatch({ wpp_ending_year() }, error = function(e) NULL))
    )
    json_txt <- jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE, na = "null")
    writeLines(json_txt, meta_path, useBytes = TRUE)
    cat("[PHASE3] Saving metadata for simulation:", sim_name, "\n")
    cat("[PHASE3] Metadata saved to:", meta_path, "\n")
    cat("[PHASE3] Metadata content:\n", paste0(json_txt, collapse = "\n"), "\n")
  }

  # Phase 4: Save population data and parameters
  save_population_files <- function(trigger = NULL, raw_data_override = NULL) {
    sim_name <- simulations$current
    if (is.null(sim_name) || !nzchar(sim_name)) return(invisible(NULL))
    sim_dir <- ensure_sim_dirs(sim_base_dir, sim_name)
    inputs_dir <- file.path(sim_dir, "inputs")
    pop_path <- file.path(inputs_dir, "pop.csv")
    params_path <- file.path(inputs_dir, "pop_params.json")

    # Choose data to save - prioritize raw input data when provided
    pop_dt <- tryCatch({ raw_data_override }, error = function(e) NULL)
    if (is.null(pop_dt)) {
      # If a committed dataset exists, use it; else use current reactive_pop()
      pop_dt <- tryCatch({ committed_pop_rv() }, error = function(e) NULL)

      # Skip calling reactive_pop() on forward_pop_page if no committed data exists
      # This prevents blocking the UI during page transition
      if (is.null(pop_dt)) {
        if (!is.null(trigger) && trigger == "forward_pop_page") {
          cat("[PHASE4] Skipping population file save on forward_pop_page - no data yet\n")
          return(invisible(NULL))
        }
        pop_dt <- tryCatch({ reactive_pop() }, error = function(e) NULL)
      }
    }
    if (is.null(pop_dt)) return(invisible(NULL))

    # Preview head in logs before saving
    df_save <- as.data.frame(pop_dt)
    preview <- tryCatch({
      paste(utils::capture.output(print(utils::head(df_save, 10))), collapse = "\n")
    }, error = function(e) NULL)

    # Write pop.csv
    try({
      data.table::fwrite(df_save, pop_path)
    }, silent = TRUE)

    # Params snapshot
    src <- tryCatch({ pop_data_source() }, error = function(e) NULL)
    age_type_val <- NULL
    open_age_val <- NULL
    interp_method_val <- NULL
    if (!is.null(src) && identical(src, "Custom Data")) {
      age_type_val <- tryCatch({ input$modal_population_age_type }, error = function(e) NULL) %||% NULL
      open_age_val <- tryCatch({ input$modal_population_oag }, error = function(e) NULL) %||% NULL
      interp_method_val <- tryCatch({ input$modal_population_interp_method }, error = function(e) NULL) %||% NULL
    } else if (!is.null(src) && identical(src, "UN Data")) {
      # Default to Single Ages if user never touched the UN tab in the modal
      age_type_val <- tryCatch({ input$modal_population_un_age_type }, error = function(e) NULL) %||% "Single Ages"
    }

    params <- list(
      aggregation = input$toggle_region %||% NULL,
      location = input$wpp_country %||% NULL,
      ref_year = tryCatch({ wpp_starting_year() }, error = function(e) NULL),
      data_source = src,
      age_type = age_type_val %||% NULL,
      open_age = open_age_val %||% NULL,
      interp_method = interp_method_val %||% NULL,
      trigger = trigger %||% "unknown",
      saved_at = as.character(Sys.time())
    )
    params_json <- jsonlite::toJSON(params, pretty = TRUE, auto_unbox = TRUE, na = "null")
    writeLines(params_json, params_path, useBytes = TRUE)

    # Update metadata with population summary
    meta_path <- file.path(sim_dir, "metadata.json")
    meta <- list()
    if (file.exists(meta_path)) {
      meta <- tryCatch({ jsonlite::read_json(meta_path, simplifyVector = TRUE) }, error = function(e) list())
    }
    meta$last_pop_saved <- as.character(Sys.time())
    if (is.null(meta$data_sources)) meta$data_sources <- list()
    meta$data_sources$population <- tryCatch({ pop_data_source() }, error = function(e) meta$data_sources$population %||% NULL)
    writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE, na = "null"), meta_path, useBytes = TRUE)

    # Logs
    cat("[PHASE4] Population files saved for:", sim_name, " (trigger: ", (trigger %||% "unknown"), ")\n", sep = "")
    cat("[PHASE4] Population data source:", (src %||% "unknown"), "\n")
    cat("[PHASE4] pop.csv saved to:", pop_path, " rows:", tryCatch({ nrow(df_save) }, error=function(e) NA_integer_), "\n")
    if (!is.null(preview)) {
      cat("[PHASE4] pop.csv head (first 10 rows):\n", preview, "\n")
    }
    cat("[PHASE4] pop_params.json content:\n", params_json, "\n")
    # Invalidate any saved forecast results for this sim (data changed)
    try({ unlink(file.path(sim_dir, "results", "results.rds"), force = TRUE) }, silent = TRUE)
    if (!is.null(simulations$current)) try({ simulation_results(NULL) }, silent = TRUE)
  }

  # Phase 5: Save TFR data and parameters
  save_tfr_files <- function(trigger = NULL, raw_data_override = NULL) {
    sim_name <- simulations$current
    if (is.null(sim_name) || !nzchar(sim_name)) return(invisible(NULL))
    sim_dir <- ensure_sim_dirs(sim_base_dir, sim_name)
    inputs_dir <- file.path(sim_dir, "inputs")
    tfr_path <- file.path(inputs_dir, "tfr.csv")
    params_path <- file.path(inputs_dir, "tfr_params.json")

    # Choose data to save - prioritize raw input data when provided
    tfr_dt <- tryCatch({ raw_data_override }, error = function(e) NULL)
    if (is.null(tfr_dt)) {
      # If a committed dataset exists, use it; else use current reactive_tfr()
      tfr_dt <- tryCatch({ committed_tfr_rv() }, error = function(e) NULL)
      if (is.null(tfr_dt)) tfr_dt <- tryCatch({ reactive_tfr() }, error = function(e) NULL)
    }
    if (is.null(tfr_dt)) return(invisible(NULL))

    # Preview head in logs before saving
    df_save <- as.data.frame(tfr_dt)
    preview <- tryCatch({
      paste(utils::capture.output(print(utils::head(df_save, 10))), collapse = "\n")
    }, error = function(e) NULL)

    # Write tfr.csv
    try({
      data.table::fwrite(df_save, tfr_path)
    }, silent = TRUE)

    # Params snapshot
    src <- tryCatch({ data_source$tfr }, error = function(e) NULL)
    
    params <- list(
      aggregation = input$toggle_region %||% NULL,
      location = input$wpp_country %||% NULL,
      start_year = tryCatch({ tfr_starting_year() }, error = function(e) NULL),
      end_year = tryCatch({ wpp_ending_year() }, error = function(e) NULL),
      data_source = src,
      trigger = trigger %||% "unknown",
      saved_at = as.character(Sys.time())
    )
    params_json <- jsonlite::toJSON(params, pretty = TRUE, auto_unbox = TRUE, na = "null")
    writeLines(params_json, params_path, useBytes = TRUE)

    # Update metadata with TFR summary
    meta_path <- file.path(sim_dir, "metadata.json")
    meta <- list()
    if (file.exists(meta_path)) {
      meta <- tryCatch({ jsonlite::read_json(meta_path, simplifyVector = TRUE) }, error = function(e) list())
    }
    meta$last_tfr_saved <- as.character(Sys.time())
    if (is.null(meta$data_sources)) meta$data_sources <- list()
    meta$data_sources$tfr <- tryCatch({ data_source$tfr }, error = function(e) meta$data_sources$tfr %||% NULL)
    writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE, na = "null"), meta_path, useBytes = TRUE)

    # Logs
    cat("[PHASE5] TFR files saved for:", sim_name, " (trigger: ", (trigger %||% "unknown"), ")\n", sep = "")
    cat("[PHASE5] TFR data source:", (src %||% "unknown"), "\n")
    cat("[PHASE5] tfr.csv saved to:", tfr_path, " rows:", tryCatch({ nrow(df_save) }, error=function(e) NA_integer_), "\n")
    if (!is.null(preview)) {
      cat("[PHASE5] tfr.csv head (first 10 rows):\n", preview, "\n")
    }
    cat("[PHASE5] tfr_params.json content:\n", params_json, "\n")
    # Invalidate saved forecast results (inputs changed)
    try({ unlink(file.path(sim_dir, "results", "results.rds"), force = TRUE) }, silent = TRUE)
    if (!is.null(simulations$current)) try({ simulation_results(NULL) }, silent = TRUE)
  }

  # Phase 6: Save e0 data and parameters
  save_e0_files <- function(trigger = NULL, raw_data_override = NULL) {
    sim_name <- simulations$current
    if (is.null(sim_name) || !nzchar(sim_name)) return(invisible(NULL))
    sim_dir <- ensure_sim_dirs(sim_base_dir, sim_name)
    inputs_dir <- file.path(sim_dir, "inputs")
    e0_path <- file.path(inputs_dir, "e0.csv")
    params_path <- file.path(inputs_dir, "e0_params.json")

    # Choose data to save - prioritize raw input data when provided
    e0_dt <- tryCatch({ raw_data_override }, error = function(e) NULL)
    if (is.null(e0_dt)) {
      # If a committed dataset exists, use it; else use current reactive_e0()
      e0_dt <- tryCatch({ committed_e0_rv() }, error = function(e) NULL)
      if (is.null(e0_dt)) e0_dt <- tryCatch({ reactive_e0() }, error = function(e) NULL)
    }
    if (is.null(e0_dt)) return(invisible(NULL))

    # Preview head in logs before saving
    df_save <- as.data.frame(e0_dt)
    preview <- tryCatch({
      paste(utils::capture.output(print(utils::head(df_save, 10))), collapse = "\n")
    }, error = function(e) NULL)

    # Write e0.csv
    try({
      data.table::fwrite(df_save, e0_path)
    }, silent = TRUE)

    # Params snapshot
    src <- tryCatch({ data_source$e0 }, error = function(e) NULL)
    
    params <- list(
      aggregation = input$toggle_region %||% NULL,
      location = input$wpp_country %||% NULL,
      start_year = tryCatch({ wpp_starting_year() }, error = function(e) NULL),
      end_year = tryCatch({ wpp_ending_year() }, error = function(e) NULL),
      data_source = src,
      trigger = trigger %||% "unknown",
      saved_at = as.character(Sys.time())
    )
    params_json <- jsonlite::toJSON(params, pretty = TRUE, auto_unbox = TRUE, na = "null")
    writeLines(params_json, params_path, useBytes = TRUE)

    # Update metadata with e0 summary
    meta_path <- file.path(sim_dir, "metadata.json")
    meta <- list()
    if (file.exists(meta_path)) {
      meta <- tryCatch({ jsonlite::read_json(meta_path, simplifyVector = TRUE) }, error = function(e) list())
    }
    meta$last_e0_saved <- as.character(Sys.time())
    if (is.null(meta$data_sources)) meta$data_sources <- list()
    meta$data_sources$e0 <- tryCatch({ data_source$e0 }, error = function(e) meta$data_sources$e0 %||% NULL)
    writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE, na = "null"), meta_path, useBytes = TRUE)

    # Logs
    cat("[PHASE6] e0 files saved for:", sim_name, " (trigger: ", (trigger %||% "unknown"), ")\n", sep = "")
    cat("[PHASE6] e0 data source:", (src %||% "unknown"), "\n")
    cat("[PHASE6] e0.csv saved to:", e0_path, " rows:", tryCatch({ nrow(df_save) }, error=function(e) NA_integer_), "\n")
    if (!is.null(preview)) {
      cat("[PHASE6] e0.csv head (first 10 rows):\n", preview, "\n")
    }
    cat("[PHASE6] e0_params.json content:\n", params_json, "\n")
    # Invalidate saved forecast results (inputs changed)
    try({ unlink(file.path(sim_dir, "results", "results.rds"), force = TRUE) }, silent = TRUE)
    if (!is.null(simulations$current)) try({ simulation_results(NULL) }, silent = TRUE)
  }

  # Phase 6: Save migration data and parameters
  save_mig_files <- function(trigger = NULL, raw_data_override = NULL) {
    sim_name <- simulations$current
    if (is.null(sim_name) || !nzchar(sim_name)) return(invisible(NULL))
    sim_dir <- ensure_sim_dirs(sim_base_dir, sim_name)
    inputs_dir <- file.path(sim_dir, "inputs")
    mig_path <- file.path(inputs_dir, "mig.csv")
    params_path <- file.path(inputs_dir, "mig_params.json")

    # Choose data to save - prioritize raw input data when provided
    mig_dt <- tryCatch({ raw_data_override }, error = function(e) NULL)
    if (is.null(mig_dt)) {
      # If a committed dataset exists, use it; else use current reactive_mig()
      mig_dt <- tryCatch({ committed_mig_rv() }, error = function(e) NULL)
      if (is.null(mig_dt)) mig_dt <- tryCatch({ reactive_mig() }, error = function(e) NULL)
    }
    if (is.null(mig_dt)) return(invisible(NULL))

    # Preview head in logs before saving
    df_save <- as.data.frame(mig_dt)
    preview <- tryCatch({
      paste(utils::capture.output(print(utils::head(df_save, 10))), collapse = "\n")
    }, error = function(e) NULL)

    # Write mig.csv
    try({
      data.table::fwrite(df_save, mig_path)
    }, silent = TRUE)

    # Params snapshot
    src <- tryCatch({ data_source$mig }, error = function(e) NULL)
    
    params <- list(
      aggregation = input$toggle_region %||% NULL,
      location = input$wpp_country %||% NULL,
      start_year = tryCatch({ wpp_starting_year() }, error = function(e) NULL),
      end_year = tryCatch({ wpp_ending_year() }, error = function(e) NULL),
      data_source = src,
      trigger = trigger %||% "unknown",
      saved_at = as.character(Sys.time())
    )
    params_json <- jsonlite::toJSON(params, pretty = TRUE, auto_unbox = TRUE, na = "null")
    writeLines(params_json, params_path, useBytes = TRUE)

    # Update metadata with migration summary
    meta_path <- file.path(sim_dir, "metadata.json")
    meta <- list()
    if (file.exists(meta_path)) {
      meta <- tryCatch({ jsonlite::read_json(meta_path, simplifyVector = TRUE) }, error = function(e) list())
    }
    meta$last_mig_saved <- as.character(Sys.time())
    if (is.null(meta$data_sources)) meta$data_sources <- list()
    meta$data_sources$mig <- tryCatch({ data_source$mig }, error = function(e) meta$data_sources$mig %||% NULL)
    writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE, na = "null"), meta_path, useBytes = TRUE)

    # Logs
    cat("[PHASE6] Migration files saved for:", sim_name, " (trigger: ", (trigger %||% "unknown"), ")\n", sep = "")
    cat("[PHASE6] Migration data source:", (src %||% "unknown"), "\n")
    cat("[PHASE6] mig.csv saved to:", mig_path, " rows:", tryCatch({ nrow(df_save) }, error=function(e) NA_integer_), "\n")
    if (!is.null(preview)) {
      cat("[PHASE6] mig.csv head (first 10 rows):\n", preview, "\n")
    }
    cat("[PHASE6] mig_params.json content:\n", params_json, "\n")
    # Invalidate saved forecast results (inputs changed)
    try({ unlink(file.path(sim_dir, "results", "results.rds"), force = TRUE) }, silent = TRUE)
    if (!is.null(simulations$current)) try({ simulation_results(NULL) }, silent = TRUE)
  }

  # Dynamic header and dropdown for simulations (Phase 2)
  output$confirm_remove_sim_modal <- shiny::renderUI({
    shiny.semantic::modal(
      id = "confirm_remove_sim",
      header = shiny::div(class = "ui header", i18n$t("Delete simulation?")),
      shiny::div(shiny::textOutput("confirm_remove_sim_message")),
      footer = shiny::div(
        class = "actions",
        shiny.semantic::action_button("confirm_remove_sim_btn", i18n$t("Delete"), class = "ui red button")
      ),
      class = "small"
    )
  })

  output$confirm_remove_sim_message <- shiny::renderText({
    sim_name <- pending_sim_removal()
    template <- i18n$translate("Are you sure you want to delete the simulation \"%s\"? This action cannot be undone.")
    if (is.null(sim_name) || !nzchar(sim_name)) {
      return("")
    }
    sprintf(template, sim_name)
  })

  output$sim_header <- shiny::renderUI({
    n <- length(names(simulations$data))
    shiny::tags$h4(class = "ui header sim-header", paste0(i18n$translate("Simulations"), " (", n, ")"))
  })

  output$sim_switcher_ui <- shiny::renderUI({
    sim_names <- names(simulations$data)
    if (length(sim_names) == 0) return(NULL)
    selected <- if (!is.null(simulations$current) && simulations$current %in% sim_names) simulations$current else sim_names[[1]]

    pill_nodes <- lapply(sim_names, function(sim_name) {
      classes <- c("sim-pill", if (identical(sim_name, selected)) "active" else NULL)
      shiny::tags$div(
        class = paste(classes, collapse = " "),
        `data-sim` = sim_name,
        onclick = "Shiny.setInputValue('sim_switcher_click', this.dataset.sim, {priority: 'event'})",
        shiny::tags$span(sim_name),
        shiny::tags$span(
          class = "sim-pill-remove",
          `data-sim` = sim_name,
          onclick = "event.stopPropagation(); Shiny.setInputValue('remove_sim', this.dataset.sim, {priority: 'event'})",
          "x"
        )
      )
    })

    shiny::tagList(
      shiny::div(
        class = "hidden-sim-select",
        shiny::selectInput("sim_switcher", label = NULL, choices = sim_names, selected = selected)
      ),
      shiny::div(class = "sim-pill-container", do.call(shiny::tagList, pill_nodes))
    )
  })

  output$sim_add_controls <- shiny::renderUI({
    total <- length(names(simulations$data))
    if (total < 3) {
      shiny::tagList(
        shiny::div(
          class = "menu-actions",
          shiny.semantic::action_button(
            "add_sim",
            i18n$translate("Add a new simulation"),
            icon = icon("plus"),
            class = "ui primary button"
          )
        ),
        shiny::tags$div(class = "ui divider")
      )
    } else {
      shiny::tagList(
        shiny::div(
          class = "ui tiny info message",
          i18n$translate("Only 3 sims are allowed")
        ),
        shiny::tags$div(class = "ui divider")
      )
    }
  })

  output$nav_list_ui <- shiny::renderUI({
    nav_items <- list(
      list(page = "input_page", trigger = "nav_input", label = i18n$translate("Projection Settings")),
      list(page = "pop_page", trigger = "nav_pop", label = i18n$translate("Population")),
      list(page = "tfr_page", trigger = "nav_tfr", label = i18n$translate("TFR")),
      list(page = "e0_page", trigger = "nav_e0", label = i18n$translate("Life Expectancy")),
      list(page = "mig_page", trigger = "nav_mig", label = i18n$translate("Migration"))
    )

    results_invalidated()  # ensure reactive update when results are saved/cleared
    sim_name <- tryCatch({ simulations$current }, error = function(e) NULL)
    if (!is.null(sim_name) && nzchar(sim_name)) {
      results_rds <- file.path(sim_base_dir, sim_name, "results", "results.rds")
      if (file.exists(results_rds)) {
        nav_items <- append(nav_items, list(list(
          page = "forecast_page",
          trigger = "nav_forecast",
          label = i18n$translate("Projection Results")
        )))
      }
    }

    current_page <- tryCatch({ current_tab() }, error = function(e) NULL)
    if (!is.null(current_page) && grepl("^modal_", current_page)) {
      base <- sub("^modal_", "", current_page)
      current_page <- paste0(base, "_page")
    }
    if (is.null(current_page) || !nzchar(current_page)) {
      current_page <- nav_items[[1]]$page
    }

    nav_nodes <- lapply(nav_items, function(item) {
      active <- identical(item$page, current_page)
      indicator_class <- if (active) "dot circle icon nav-status-icon active" else "circle outline icon nav-status-icon"
      shiny::tags$div(
        class = paste(c("item nav-link", if (active) "active"), collapse = " "),
        onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'})", item$trigger),
        shiny::tags$i(class = indicator_class),
        shiny::tags$span(item$label)
      )
    })

    shiny::tags$div(class = "ui list", do.call(shiny::tagList, nav_nodes))
  })

  # Initialize sim from URL (if present)
  observe({
    qry <- isolate(session$clientData$url_search)
    if (!is.null(qry) && nzchar(qry)) {
      params <- shiny::parseQueryString(sub("^\\?", "", qry))
      if (!is.null(params$sim)) {
        shiny.semantic::updateSelectInput(session, "sim_switcher", selected = params$sim)
      }
    }
  })

  # Update URL when simulation changes (preserve other params)
  # Add flag to track when we're creating a new simulation to avoid restore logic
  creating_new_sim <- reactiveVal(FALSE)

  observeEvent(input$sim_switcher_click, {
    clicked_sim <- input$sim_switcher_click
    if (is.null(clicked_sim) || !nzchar(clicked_sim)) return()
    if (!clicked_sim %in% names(simulations$data)) return()
    shiny.semantic::updateSelectInput(session, "sim_switcher", selected = clicked_sim)
  })

  observeEvent(input$sim_switcher, ignoreInit = TRUE, {
    switching_sims(TRUE)

    # Skip restoration if we're creating a new simulation
    if (isTRUE(creating_new_sim())) {
      cat("[SIM_SWITCH] Skipping restoration - creating new simulation\n")
      creating_new_sim(FALSE)  # Reset flag
      simulations$current <- input$sim_switcher
      return()
    }

    # Phase 8: Complete simulation switching with data restoration
    selected_sim <- input$sim_switcher
    old_sim <- simulations$current
    cat("[SIM_SWITCH] ================================================\n")
    cat("[SIM_SWITCH] Starting simulation switch:", old_sim, "->", selected_sim, "\n")
    cat("[SIM_SWITCH] Timestamp:", Sys.time(), "\n")
    cat("[SIM_SWITCH] ================================================\n")

    # Clear computation tracking to prevent cross-sim issues
    last_computed_signature(NULL)
    cat("[SIM_SWITCH] Cleared computation tracking\n")
    
    # Update current selection in memory
    simulations$current <- selected_sim
    
    # Hard reset all in-memory state to avoid cross-simulation leakage
    # This guarantees reactive values and modal caches do not bleed between sims
    reset_simulation_state()
    
    # Load simulation data from disk
    loaded_data <- load_simulation_data(selected_sim)
    
    if (!is.null(loaded_data)) {
      cat("[PHASE8] Data restored from simulation directory\n")

      # Restore all reactive values and inputs
      restore_simulation_state(loaded_data)
      # Navigate to last page if available
      desired_page <- tryCatch({ loaded_data$metadata$current_page }, error = function(e) NULL)
      if (is.null(desired_page) || !nzchar(desired_page)) desired_page <- "input_page"
      go_to_page(desired_page)
      # Determine if Next had been clicked for these exact inputs
      saved_meta <- tryCatch({ loaded_data$metadata }, error = function(e) NULL)
      saved_sig <- tryCatch({ saved_meta$input_signature }, error = function(e) NULL)
      curr_sig <- compute_input_signature(input$toggle_region, input$wpp_country, input$wpp_starting_year, input$wpp_ending_year)
      # Consider Next clicked if last_trigger indicates progression beyond input
      last_trig <- tryCatch({ saved_meta$last_trigger }, error = function(e) NULL)
      progressed <- isTRUE(last_trig %in% c("forward_pop_page","forward_tfr_page","forward_e0_page","forward_mig_page","begin"))
      sim_saved_signature(saved_sig)
      sim_progressed(progressed)
      input_next_clicked(isTRUE(progressed) && !identical(saved_sig, NULL) && identical(saved_sig, curr_sig))
      # Restore state debug can be enabled here if needed
      # If last page is forecast, set up UI and load saved results only (no compute on switch)
      if (identical(desired_page, "forecast_page")) {
        cat("[SIM_SWITCH] Last page was forecast, setting up load-only mode for sim:", selected_sim, "\n")

        if (isTRUE(restoring_inputs())) {
          cat("[SIM_SWITCH] Inputs still restoring, delaying forecast UI setup\n")
          local_obs <- NULL
          local_obs <- observeEvent(restoring_inputs(), {
            if (!isTRUE(restoring_inputs())) {
              cat("[SIM_SWITCH] Restore complete, setting up forecast UI in load mode for sim:", selected_sim, "\n")
              setup_forecast_ui(selected_sim, mode = "load")
              local_obs$destroy()
            }
          }, ignoreInit = FALSE)
        } else {
          cat("[SIM_SWITCH] No restore in progress, setting up forecast UI in load mode for sim:", selected_sim, "\n")
          setup_forecast_ui(selected_sim, mode = "load")
        }
      }
    } else {
      cat("[PHASE8] No saved data found for simulation:", selected_sim, "\n")
      go_to_page("input_page")
    }
    
    # Update URL (keeping existing logic)
    qry <- isolate(session$clientData$url_search)
    params <- list()
    if (!is.null(qry) && nzchar(qry)) {
      params <- shiny::parseQueryString(sub("^\\?", "", qry))
    }
    params$sim <- selected_sim
    if (length(params)) {
      kv <- vapply(names(params), function(k){
        paste(utils::URLencode(k, reserved = TRUE), utils::URLencode(as.character(params[[k]]), reserved = TRUE), sep = "=")
      }, character(1))
      new_qs <- paste0("?", paste(kv, collapse = "&"))
    } else {
      new_qs <- paste0("?sim=", utils::URLencode(selected_sim, reserved = TRUE))
    }
    shiny::updateQueryString(new_qs, mode = "push", session = session)

    cat("[SIM_SWITCH] ================================================\n")
    cat("[SIM_SWITCH] Simulation switch completed:", old_sim, "->", selected_sim, "\n")
    cat("[SIM_SWITCH] Timestamp:", Sys.time(), "\n")
    cat("[SIM_SWITCH] ================================================\n")
  })

  perform_sim_removal <- function(sim_to_remove) {
    if (is.null(sim_to_remove) || !nzchar(sim_to_remove)) return()

    sim_names <- names(simulations$data)
    if (!(sim_to_remove %in% sim_names)) return()

    cat("[SIM_REMOVE] ================================================\n")
    cat("[SIM_REMOVE] Removing simulation:", sim_to_remove, "\n")
    cat("[SIM_REMOVE] Timestamp:", Sys.time(), "\n")

    simulations$data[[sim_to_remove]] <- NULL

    sim_dir <- file.path(sim_base_dir, sim_to_remove)
    try({ unlink(sim_dir, recursive = TRUE, force = TRUE) }, silent = TRUE)

    remaining <- names(simulations$data)
    removing_current <- identical(simulations$current, sim_to_remove)

    if (length(remaining) == 0) {
      cat("[SIM_REMOVE] No simulations remain after removal.\n")
      simulations$current <- NULL
      reset_simulation_state()
      shiny.semantic::updateSelectInput(session, "sim_switcher", choices = remaining, selected = NULL)
      go_to_page("input_page")
      cat("[SIM_REMOVE] ================================================\n")
      return()
    }

    next_sim <- if (!removing_current && !is.null(simulations$current) && simulations$current %in% remaining) {
      simulations$current
    } else {
      remaining[[1]]
    }

    simulations$current <- next_sim
    shiny.semantic::updateSelectInput(session, "sim_switcher", choices = remaining, selected = next_sim)

    cat("[SIM_REMOVE] Switched focus to:", next_sim, "\n")
    cat("[SIM_REMOVE] Remaining simulations:", paste(remaining, collapse = ", "), "\n")
    cat("[SIM_REMOVE] ================================================\n")
  }

  observeEvent(input$remove_sim, {
    sim_to_remove <- input$remove_sim
    if (is.null(sim_to_remove) || !nzchar(sim_to_remove)) return()

    sim_names <- names(simulations$data)
    if (!(sim_to_remove %in% sim_names)) return()

    pending_sim_removal(sim_to_remove)
    shiny.semantic::show_modal("confirm_remove_sim")
  })

  observeEvent(input$confirm_remove_sim_btn, {
    sim_to_remove <- pending_sim_removal()
    pending_sim_removal(NULL)
    shiny.semantic::hide_modal("confirm_remove_sim")
    perform_sim_removal(sim_to_remove)
  })

  # Respond to back/forward navigation updates from JS
  observeEvent(input$sim_from_url, {
    if (is.null(input$sim_from_url)) return()
    shiny.semantic::updateSelectInput(session, "sim_switcher", selected = input$sim_from_url)
  })

    # New Simulation inline flow (sidebar)
  new_sim_form_visible <- reactiveVal(FALSE)
  observeEvent(input$add_sim, {
    if (length(names(simulations$data)) >= 3) {
      shiny::showNotification(i18n$translate("You can only keep up to three simulations at a time"), type = "warning")
      new_sim_form_visible(FALSE)
      return()
    }
    new_sim_form_visible(TRUE)
  })

  output$new_sim_inline <- shiny::renderUI({
    if (!isTRUE(new_sim_form_visible())) return(NULL)
    shiny::div(
      class = "ui form",
      shiny::div(class = "required field",
                shiny::tags$label(i18n$translate("Simulation Name")),
                shiny::textInput("new_sim_name", label = NULL, placeholder = i18n$translate("Enter a simulation name"), width = "100%")
      ),
      shiny::div(style = "display:flex; gap:6px; margin:6px 0;",
                 shiny.semantic::action_button("create_sim_confirm", i18n$translate("Create"), class = "ui primary button"),
                 shiny.semantic::action_button("cancel_sim_create", i18n$translate("Cancel"), class = "ui button"))
    )
  })

  observeEvent(input$cancel_sim_create, { new_sim_form_visible(FALSE) })

  observe({
    if (length(names(simulations$data)) >= 3) {
      new_sim_form_visible(FALSE)
    }
  })

  # Central widget management functions
  reset_modal_widgets <- function(session) {
    cat("[WIDGET_RESET] Resetting all modal widgets to defaults\n")
    updateSelectInput(session, "modal_population_un_age_type", selected = "Single Ages")
    updateSelectInput(session, "modal_population_age_type", selected = "Single Ages")
    updateNumericInput(session, "modal_population_oag", value = 100)
    updateSelectInput(session, "modal_population_interp_method", selected = "beers(ord)")
    # Note: modal_population_source is handled by the modal tab system, not directly
  }
  
  restore_modal_widgets <- function(session, pop_params) {
    if (is.null(pop_params)) return()
    
    cat("[WIDGET_RESTORE] Restoring modal widgets from saved parameters\n")
    
    src <- pop_params$data_source %||% "UN Data"
    
    if (identical(src, "UN Data")) {
      # Only restore UN widgets
      if (!is.null(pop_params$age_type)) {
        updateSelectInput(session, "modal_population_un_age_type", selected = pop_params$age_type)
        cat("[WIDGET_RESTORE] (UN) Age type:", pop_params$age_type, "\n")
      }
      # Do not touch Custom widgets here
    } else if (identical(src, "Custom Data")) {
      # Only restore Custom widgets
      if (!is.null(pop_params$age_type)) {
        updateSelectInput(session, "modal_population_age_type", selected = pop_params$age_type)
        cat("[WIDGET_RESTORE] (Custom) Age type:", pop_params$age_type, "\n")
      }
      if (!is.null(pop_params$open_age) && length(pop_params$open_age) > 0 && !is.list(pop_params$open_age)) {
        updateNumericInput(session, "modal_population_oag", value = pop_params$open_age)
        cat("[WIDGET_RESTORE] (Custom) OAG:", pop_params$open_age, "\n")
      }
      if (!is.null(pop_params$interp_method) && length(pop_params$interp_method) > 0 && !is.list(pop_params$interp_method)) {
        updateSelectInput(session, "modal_population_interp_method", selected = pop_params$interp_method)
        cat("[WIDGET_RESTORE] (Custom) Interp method:", pop_params$interp_method, "\n")
      }
      # Do not touch UN widgets here
    }
  }

  # Load simulation data from disk
  load_simulation_data <- function(sim_name) {
    if (is.null(sim_name) || !nzchar(sim_name)) return(NULL)
    
    sim_dir <- file.path(sim_base_dir, sim_name)
    inputs_dir <- file.path(sim_dir, "inputs")
    
    if (!dir.exists(sim_dir) || !dir.exists(inputs_dir)) {
      cat("[PHASE8] No data directory found for simulation:", sim_name, "\n")
      return(NULL)
    }
    
    result <- list(
      metadata = NULL,
      pop_data = NULL,
      pop_params = NULL,
      tfr_data = NULL,
      tfr_params = NULL
    )
    
    # Load metadata
    meta_path <- file.path(sim_dir, "metadata.json")
    if (file.exists(meta_path)) {
      result$metadata <- tryCatch({
        jsonlite::read_json(meta_path, simplifyVector = TRUE)
      }, error = function(e) NULL)
    }
    
    # Load population data and parameters
    pop_path <- file.path(inputs_dir, "pop.csv")
    pop_params_path <- file.path(inputs_dir, "pop_params.json")
    
    if (file.exists(pop_path)) {
      result$pop_data <- tryCatch({
        data.table::fread(pop_path)
      }, error = function(e) NULL)
    }
    
    if (file.exists(pop_params_path)) {
      result$pop_params <- tryCatch({
        jsonlite::read_json(pop_params_path, simplifyVector = TRUE)
      }, error = function(e) NULL)
    }
    
    # Load TFR data and parameters (if present)
    tfr_path <- file.path(inputs_dir, "tfr.csv")
    tfr_params_path <- file.path(inputs_dir, "tfr_params.json")
    if (file.exists(tfr_path)) {
      result$tfr_data <- tryCatch({
        data.table::fread(tfr_path)
      }, error = function(e) NULL)
    }
    if (file.exists(tfr_params_path)) {
      result$tfr_params <- tryCatch({
        jsonlite::read_json(tfr_params_path, simplifyVector = TRUE)
      }, error = function(e) NULL)
    }
    
    # Load e0 data and parameters (if present)
    e0_path <- file.path(inputs_dir, "e0.csv")
    e0_params_path <- file.path(inputs_dir, "e0_params.json")
    if (file.exists(e0_path)) {
      result$e0_data <- tryCatch({
        data.table::fread(e0_path)
      }, error = function(e) NULL)
    }
    if (file.exists(e0_params_path)) {
      result$e0_params <- tryCatch({
        jsonlite::read_json(e0_params_path, simplifyVector = TRUE)
      }, error = function(e) NULL)
    }

    # Load migration data and parameters (if present)
    mig_path <- file.path(inputs_dir, "mig.csv")
    mig_params_path <- file.path(inputs_dir, "mig_params.json")
    if (file.exists(mig_path)) {
      result$mig_data <- tryCatch({
        data.table::fread(mig_path)
      }, error = function(e) NULL)
    }
    if (file.exists(mig_params_path)) {
      result$mig_params <- tryCatch({
        jsonlite::read_json(mig_params_path, simplifyVector = TRUE)
      }, error = function(e) NULL)
    }

    cat("[PHASE8] Loaded data - Meta:", !is.null(result$metadata),
        "Pop:", !is.null(result$pop_data), "PopParams:", !is.null(result$pop_params),
        "TFR:", !is.null(result$tfr_data), "TfrParams:", !is.null(result$tfr_params),
        "e0:", !is.null(result$e0_data), "e0Params:", !is.null(result$e0_params),
        "Mig:", !is.null(result$mig_data), "MigParams:", !is.null(result$mig_params), "\n")
    
    return(result)
  }

  # Restore simulation state from loaded data
  restore_simulation_state <- function(loaded_data) {
    if (is.null(loaded_data)) return()
    
    cat("[PHASE8] Starting state restoration...\n")
    restoring_inputs(TRUE)
    
    # Restore metadata (input fields)
    if (!is.null(loaded_data$metadata)) {
      # 0) Restore aggregation toggle FIRST so the location selector has correct choices
      agg <- tryCatch({
        # Prefer pop_params aggregation if present, else metadata
        if (!is.null(loaded_data$pop_params$aggregation)) loaded_data$pop_params$aggregation else loaded_data$metadata$aggregation
      }, error = function(e) NULL)

      if (!is.null(agg) && nzchar(agg)) {
        # Translate aggregation label to match current UI language
        val <- tryCatch({ i18n$translate(agg) }, error = function(e) agg)
        restored_aggregation(val)
        # Use shiny.semantic updater for the radio
        shiny.semantic::update_multiple_radio(
          session,
          "toggle_region",
          choices = i18n$translate(c("Country", "Region")),
          selected = val
        )
        cat("[RESTORE_TARGETS_DEBUG] agg_target:", val, "\n")
        # Proactively rebuild selector with correct choices and target selected
        target_loc <- tryCatch({ loaded_data$metadata$country }, error = function(e) NULL)
        if (!is.null(target_loc)) {
          choices <- tryCatch({
            if (identical(val, i18n$translate("Region")) || identical(val, "Region")) OPPPserver::get_wpp_regions() else OPPPserver::get_wpp_countries()
          }, error = function(e) NULL)
          if (!is.null(choices)) {
            cat("[RESTORE_SELECTOR_DEBUG] Forcing selector rebuild | mode:", val, " choices_len:", length(choices), " target:", target_loc, "\n")
            try({ shiny.semantic::updateSelectInput(session, "wpp_country", choices = choices, selected = target_loc) }, silent = TRUE)
          }
        }
      }

      # ATOMIC INPUT UPDATES: Batch all input updates together to prevent intermediate reactivity
      cat("[PHASE8] Starting atomic input updates\n")

      # 1) Years - isolate to prevent intermediate reactive firings
      isolate({
        if (!is.null(loaded_data$metadata$start_year)) {
          updateNumericInput(session, "wpp_starting_year", value = loaded_data$metadata$start_year)
          cat("[PHASE8] Updated start year to:", loaded_data$metadata$start_year, "\n")
        }
        if (!is.null(loaded_data$metadata$end_year)) {
          updateNumericInput(session, "wpp_ending_year", value = loaded_data$metadata$end_year)
          cat("[PHASE8] Updated end year to:", loaded_data$metadata$end_year, "\n")
        }
      })

      # 2) Location (after toggle has switched and selector has re-rendered)
      if (!is.null(loaded_data$metadata$country)) {
        restored_location(loaded_data$metadata$country)
        cat("[PHASE8] Set restored_location target to:", loaded_data$metadata$country, "\n")

        # Use a slightly longer delay to ensure selector is ready
        shinyjs::delay(200, {
          isolate({
            try({
              shiny.semantic::updateSelectInput(session, "wpp_country", selected = loaded_data$metadata$country)
              cat("[PHASE8] Updated wpp_country to:", loaded_data$metadata$country, "\n")
            }, silent = TRUE)
          })
        })
        cat("[RESTORE_TARGETS_DEBUG] location_target:", loaded_data$metadata$country, "\n")
      }

      cat("[PHASE8] Input fields updated successfully\n")
    }
    
    # Restore population data through the transformation pipeline
    if (!is.null(loaded_data$pop_data) && !is.null(loaded_data$pop_params)) {
      cat("[PHASE8] Restoring population data with params:", jsonlite::toJSON(loaded_data$pop_params, auto_unbox = TRUE), "\n")
      cat("[PHASE8] Raw pop data dimensions:", nrow(loaded_data$pop_data), "x", ncol(loaded_data$pop_data), "\n")
      
      # 1. Store raw data for modal (exactly as saved - 5-year or single ages)
      modal_raw_pop_data(loaded_data$pop_data)
      modal_pop_params(loaded_data$pop_params)
      
      # 2. Restore modal widget values to match saved state
      restore_modal_widgets(session, loaded_data$pop_params)
      
      # 3. Transform data for plotting (always to single ages for pyramid)
      country <- loaded_data$metadata$country %||% "Afghanistan"
      ref_year <- loaded_data$metadata$start_year %||% 2024
      
      transformed_data <- process_population_data(
        loaded_data$pop_data,      # Raw data (could be 5-year or single)
        loaded_data$pop_params,     # Contains age_type info
        country,
        ref_year
      )
      
      cat("[PHASE8] Transformed pop data dimensions:", if(!is.null(transformed_data)) paste(nrow(transformed_data), "x", ncol(transformed_data)) else "NULL", "\n")
      
      # 4. Store transformed data for pyramid plot (isolate to prevent intermediate reactivity)
      isolate({
        committed_pop_rv(transformed_data)
        cat("[PHASE8] Stored transformed pop data\n")
      })

      # 5. Restore data source flag (isolate to prevent intermediate reactivity)
      isolate({
        pop_data_source(loaded_data$pop_params$data_source %||% "UN Data")
        cat("[PHASE8] Set pop_data_source to:", loaded_data$pop_params$data_source %||% "UN Data", "\n")
      })
      
      # If this simulation is not Custom Data, ensure no stale custom configs remain
      if (!identical(loaded_data$pop_params$data_source, "Custom Data")) {
        custom_data_configs(list())
        modal_reset_trigger(modal_reset_trigger() + 1)
        cat("[PHASE8] Cleared custom_data_configs for UN Data simulation\n")
      }
      
      # 6. If Custom Data, repopulate the custom_data_configs
      if (loaded_data$pop_params$data_source == "Custom Data") {
        # Create config key from saved params
        age_type <- loaded_data$pop_params$age_type %||% "Single Ages"
        oag <- loaded_data$pop_params$open_age %||% 100
        interp_method <- loaded_data$pop_params$interp_method %||% "beers(ord)"
        
        config_key <- paste0(age_type, "_", oag)
        
        # Store the raw data in custom_data_configs
        all_configs <- list()
        all_configs[[config_key]] <- list(
          data = loaded_data$pop_data,
          interp_method = interp_method
        )
        custom_data_configs(all_configs)
        
        # Set flag to prevent auto-saving after restoration
        # This will be cleared when user actually interacts with the modal
        just_restored_data(TRUE)
        
        cat("[CONFIG_TRACE] RESTORATION: Restored config", config_key, "with", nrow(loaded_data$pop_data), "rows\n")
        cat("[PHASE8] Restored custom data configs for key:", config_key, "\n")
        cat("[CUSTOM_RESTORE_DEBUG] Loaded raw data preview:\n")
        print(head(loaded_data$pop_data, 5))
      }
      
      cat("[PHASE8] Population data restored and processed through pipeline\n")
    } else {
      # No population files found: ensure a clean UN default state
      committed_pop_rv(NULL)
      modal_raw_pop_data(NULL)
      modal_pop_params(NULL)
      pop_data_source("UN Data")
      custom_data_configs(list())
      modal_reset_trigger(modal_reset_trigger() + 1)
      cat("[PHASE8] No pop data/params present; cleared state to defaults (UN Data)\n")
    }
    
    # ATOMIC REACTIVE VALUE UPDATES: Batch all reactive value updates to prevent intermediate firings
    cat("[PHASE8] Starting atomic reactive value updates\n")

    isolate({
      # Restore TFR if available; else leave NULL so reactive_tfr falls back to WPP
      if (!is.null(loaded_data$tfr_data)) {
        tfr_dt <- as.data.frame(loaded_data$tfr_data)
        names(tfr_dt) <- c("year", "tfr")
        committed_tfr_rv(tfr_dt)
        cat("[PHASE8] Restored TFR data for simulation, rows:", nrow(tfr_dt), "\n")
      } else {
        committed_tfr_rv(NULL)
        cat("[PHASE8] No TFR data found; will use WPP defaults\n")
      }

      # Restore e0 if available; else leave NULL so reactive_e0 falls back to WPP
      if (!is.null(loaded_data$e0_data)) {
        e0_dt <- as.data.frame(loaded_data$e0_data)
        names(e0_dt) <- c("year", "e0M", "e0F")
        committed_e0_rv(e0_dt)
        cat("[PHASE8] Restored e0 data for simulation, rows:", nrow(e0_dt), "\n")
      } else {
        committed_e0_rv(NULL)
        cat("[PHASE8] No e0 data found; will use WPP defaults\n")
      }

      # Restore migration if available; else leave NULL so reactive_mig falls back to WPP
      if (!is.null(loaded_data$mig_data)) {
        mig_dt <- as.data.frame(loaded_data$mig_data)
        names(mig_dt) <- c("year", "mig")
        committed_mig_rv(mig_dt)
        cat("[PHASE8] Restored migration data for simulation, rows:", nrow(mig_dt), "\n")
      } else {
        committed_mig_rv(NULL)
        cat("[PHASE8] No migration data found; will use WPP defaults\n")
      }

      cat("[PHASE8] Completed atomic reactive value updates\n")
    })
    
    # Fallback: do not forcibly end restore; rely on match or retry loop
    shinyjs::delay(1500, {
      cat("[RESTORE_FALLBACK_DEBUG] Still restoring after initial delay | toggle:", input$toggle_region,
          " wpp_country:", input$wpp_country, " target_toggle:", tryCatch({ restored_aggregation() }, error=function(e) NA),
          " target_location:", tryCatch({ restored_location() }, error=function(e) NA), "\n")
    })
    
    cat("[PHASE8] State restoration complete\n")
  }

  # When the restored location actually lands in the input, finish restoration and enable nav if results exist
  observeEvent(input$wpp_country, {
    if (is.null(restored_location())) return()
    if (identical(input$wpp_country, restored_location())) {
      restored_location(NULL)
      restoring_inputs(FALSE)
      current_sim <- tryCatch({ simulations$current }, error = function(e) NULL)
      if (!is.null(current_sim)) {
        has_results <- file.exists(file.path(sim_base_dir, current_sim, "results", "results.rds"))
        if (has_results) input_next_clicked(TRUE)
      }
    }
  }, ignoreInit = TRUE)

  # Reset trigger for modal configurations
  modal_reset_trigger <- reactiveVal(0)
  
  # Custom Data configurations - moved to main scope for proper clearing
  custom_data_configs <- reactiveVal(list())
  
  # Track last active modal tab - moved to main scope for proper resetting
  last_active_modal_tab <- reactiveVal("UN Data")
  
  # Flag to prevent saving custom configs during simulation reset
  resetting_simulation <- reactiveVal(FALSE)
  # Flag to indicate inputs are being restored (avoid clearing committed data)
  restoring_inputs <- reactiveVal(FALSE)
  
  # Flag to prevent auto-saving after restoration
  just_restored_data <- reactiveVal(FALSE)
  # Flag indicating a simulation switch/restore is in progress
  switching_sims <- reactiveVal(FALSE)
  
  # Flag to differentiate automatic vs user-initiated tab switches
  opening_modal <- reactiveVal(FALSE)
  
  # The location (country/region) value we want the selector to restore to
  restored_location <- reactiveVal(NULL)
  # The aggregation label (translated) we want the toggle to restore to
  restored_aggregation <- reactiveVal(NULL)

  # Phase 7: Reset simulation state function
  reset_simulation_state <- function() {
    # Set flag to prevent auto-saving during reset
    resetting_simulation(TRUE)

    # Log state before reset
    cat("[RESET_DEBUG] BEFORE RESET - Data source:", pop_data_source(), ", Last active tab:", last_active_modal_tab(), "\n")
    cat("[RESET_DEBUG] BEFORE RESET - Custom configs count:", length(custom_data_configs()), ", keys:", paste(names(custom_data_configs()), collapse=", "), "\n")

    # Clear any previous computation tracking
    last_computed_signature(NULL)
    cat("[RESET_DEBUG] Cleared computation signature tracking\n")

    # Clear all committed reactive values
    committed_pop_rv(NULL)
    committed_tfr_rv(NULL)
    committed_e0_rv(NULL)
    committed_mig_rv(NULL)
    
    # Clear modal raw data
    modal_raw_pop_data(NULL)
    modal_pop_params(NULL)
    
    # Clear custom configs immediately (avoid race with modal observer)
    custom_data_configs(list())
    
    # Reset modal widgets to defaults
    reset_modal_widgets(session)
    
    # Reset modal tab to default
    last_active_modal_tab("UN Data")
    
    # Log state after reset
    cat("[RESET_DEBUG] AFTER RESET - Data source:", pop_data_source(), ", Last active tab:", last_active_modal_tab(), "\n")
    cat("[RESET_DEBUG] AFTER RESET - Custom configs count:", length(custom_data_configs()), ", keys:", paste(names(custom_data_configs()), collapse=", "), "\n")
    
    # Trigger modal configuration reset (clears UN caches + re-render in handles)
    modal_reset_trigger(modal_reset_trigger() + 1)
    
    # Reset data source flags
    pop_data_source("UN Data")
    data_source$tfr <- "downloaded"
    data_source$e0 <- "downloaded"
    data_source$mig <- "downloaded"
    restored_location(NULL)
    
    # Clear the resetting flag after reset is complete
    resetting_simulation(FALSE)
    
    # Log the reset
    cat("[PHASE7] Reactive values cleared for new simulation\n")
  }

  observeEvent(input$create_sim_confirm, {
    if (length(names(simulations$data)) >= 3) {
      shiny::showNotification(i18n$translate("You can only keep up to three simulations at a time"), type = "error")
      new_sim_form_visible(FALSE)
      return()
    }
    nm <- input$new_sim_name
    if (is.null(nm) || !nzchar(trimws(nm))) {
      shiny::showNotification(i18n$translate("Please enter a simulation name"), type = "error")
      return()
    }
    if (nm %in% names(simulations$data)) {
      shiny::showNotification(i18n$translate("A simulation with this name already exists"), type = "error")
      return()
    }
    
    # Phase 7: Complete new simulation creation
    cat("[PHASE7] Plus button clicked - starting new simulation:", nm, "\n")
    
    # Create simulation in list
    simulations$data[[nm]] <- list()
    simulations$current <- nm
    
    # Create simulation directory structure immediately
    sim_dir <- ensure_sim_dirs(sim_base_dir, nm)
    cat("[PHASE7] Created simulation directory:", sim_dir, "\n")
    
    # Reset all reactive values for fresh start
    reset_simulation_state()
    input_next_clicked(FALSE)
    
    # Set flag to prevent restoration logic when simulation switcher is updated
    creating_new_sim(TRUE)
    
    # Hide the inline form
    new_sim_form_visible(FALSE)
    
    # Navigate to input page for new simulation
    # Hide any currently visible page
    for (page in c("landing_page", "pop_page", "tfr_page", "e0_page", "mig_page", "forecast_page", "results_page")) {
      shinyjs::hide(page)
    }
    
    # Show input page
    shinyjs::show("input_page")
    shinyjs::show("left_menu")
    
    # Explicitly reset inputs to defaults
    try({ updateNumericInput(session, "wpp_starting_year", value = 2024) }, silent = TRUE)
    try({ updateNumericInput(session, "wpp_ending_year", value = 2100) }, silent = TRUE)
    # Reset Country/Region toggle to Country (translated)
    try({ shiny.semantic::update_multiple_radio(session, "toggle_region",
      choices = i18n$translate(c("Country", "Region")), selected = i18n$translate("Country")) }, silent = TRUE)
    # Clear country selection
    try({ shiny.semantic::updateSelectInput(session, "wpp_country", selected = NULL) }, silent = TRUE)
    current_tab("input_page")
    
    cat("[PHASE7] Navigated to input page for new simulation\n")
    cat("[PHASE7] Total simulations:", length(simulations$data), "\n")
  })

  # Show "Projection Results" menu immediately when forecast results are generated
  # Track previous state to only trigger when results go from NULL to available
  prev_results_state <- reactiveVal(NULL)
  observe({
    res <- simulation_results()
    prev_state <- prev_results_state()

    # Only trigger when results go from NULL/empty to available (forecast completion)
    if (is.null(prev_state) && !is.null(res)) {
      # Trigger nav list refresh to surface the Projection Results link immediately
      results_invalidated(results_invalidated() + 1)
    }

    # Update previous state
    prev_results_state(if (is.null(res)) NULL else "available")
  })

  # --- Phase 3: Save metadata on each Next action to keep it up to date ---
  observeEvent(input$forward_pop_page, {
    # Before proceeding, if core inputs changed vs last saved signature, clear sim contents
    try({
      sim_name <- simulations$current
      if (!is.null(sim_name) && nzchar(sim_name)) {
        sim_dir <- ensure_sim_dirs(sim_base_dir, sim_name)
        meta_path <- file.path(sim_dir, "metadata.json")
        prev_sig <- NULL
        if (file.exists(meta_path)) {
          prev <- tryCatch({ jsonlite::read_json(meta_path, simplifyVector = TRUE) }, error = function(e) NULL)
          prev_sig <- tryCatch({ prev$input_signature }, error = function(e) NULL)
        }
        curr_sig <- compute_input_signature(input$toggle_region, input$wpp_country, input$wpp_starting_year, input$wpp_ending_year)
        if (!is.null(prev_sig) && !identical(prev_sig, curr_sig)) {
          cat("[SIM_RESET] Input signature changed. Clearing simulation folder contents for:", sim_name, "\n")
          cat("[SIM_RESET] Previous signature:", prev_sig, "| New signature:", curr_sig, "\n")
          # Remove everything inside sim_dir (keep the folder)
          contents <- list.files(sim_dir, full.names = TRUE, all.files = FALSE, recursive = FALSE, include.dirs = TRUE)
          try(unlink(contents, recursive = TRUE, force = TRUE), silent = TRUE)
          # Recreate folders
          ensure_sim_dirs(sim_base_dir, sim_name)
          # Clear in-memory committed data to fully reset pipeline for this sim
          committed_pop_rv(NULL); committed_tfr_rv(NULL); committed_e0_rv(NULL); committed_mig_rv(NULL)
          modal_raw_pop_data(NULL); modal_pop_params(NULL)
          pop_data_source("UN Data"); data_source$tfr <- "downloaded"; data_source$e0 <- "downloaded"; data_source$mig <- "downloaded"
          cat("[SIM_RESET] Cleared disk contents and in-memory commits for:", sim_name, "\n")

          # Trigger nav list refresh to hide the Projection Results link immediately
          results_invalidated(results_invalidated() + 1)
        }
      }
    }, silent = TRUE)

    save_sim_metadata(trigger = "forward_pop_page")
    # Also snapshot population at this transition (in case user didn't customize)
    save_population_files(trigger = "forward_pop_page")
    input_next_clicked(TRUE)
    # Update signature/progress cache for current sim
    curr_sig <- compute_input_signature(input$toggle_region, input$wpp_country, input$wpp_starting_year, input$wpp_ending_year)
    sim_saved_signature(curr_sig)
    sim_progressed(TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$nav_tfr, {
    if (!isTRUE(input_next_clicked())) return()
    save_sim_metadata(trigger = "forward_tfr_page")
    save_population_files(trigger = "forward_tfr_page")
    save_tfr_files(trigger = "forward_tfr_page")
    curr_sig <- compute_input_signature(input$toggle_region, input$wpp_country, input$wpp_starting_year, input$wpp_ending_year)
    sim_saved_signature(curr_sig)
    sim_progressed(TRUE)
  }, ignoreNULL = TRUE)

  observeEvent(input$nav_e0, {
    if (!isTRUE(input_next_clicked())) return()
    save_sim_metadata(trigger = "forward_e0_page")
    save_population_files(trigger = "forward_e0_page")
    save_tfr_files(trigger = "forward_e0_page")
    save_e0_files(trigger = "forward_e0_page")
    curr_sig <- compute_input_signature(input$toggle_region, input$wpp_country, input$wpp_starting_year, input$wpp_ending_year)
    sim_saved_signature(curr_sig)
    sim_progressed(TRUE)
  }, ignoreNULL = TRUE)

  observeEvent(input$nav_mig, {
    if (!isTRUE(input_next_clicked())) return()
    save_sim_metadata(trigger = "forward_mig_page")
    save_population_files(trigger = "forward_mig_page")
    save_tfr_files(trigger = "forward_mig_page")
    save_e0_files(trigger = "forward_mig_page")
    save_mig_files(trigger = "forward_mig_page")
    curr_sig <- compute_input_signature(input$toggle_region, input$wpp_country, input$wpp_starting_year, input$wpp_ending_year)
    sim_saved_signature(curr_sig)
    sim_progressed(TRUE)
  }, ignoreNULL = TRUE)

  # Run Projection from sidebar (same behavior as migration page button)
  observeEvent(input$nav_run_projection, ignoreInit = TRUE, {
    # Guard: do not run while restoring; signature guard is not needed here
    if (isTRUE(restoring_inputs())) return()
    if (!isTRUE(input_next_clicked())) {
      showNotification(i18n$translate("Please first select an input and click Next"), type = "warning", duration = 4)
      return()
    }
    # Move to forecast page and trigger compute for current simulation
    hide("input_page"); hide("pop_page"); hide("tfr_page"); hide("e0_page"); hide("mig_page"); hide("results_page")
    show("forecast_page")
    current_tab("forecast_page")
    save_current_page("forecast_page")

    my_sim <- simulations$current
    cat("[NAV_RUN_PROJECTION] User clicked Run Projection for sim:", my_sim, "\n")

    # Set up forecast UI with compute mode
    setup_forecast_ui(my_sim, mode = "compute")
  })

  # View Projection Results from sidebar (only when results exist)
observeEvent(input$nav_forecast, {
  hide("input_page"); hide("pop_page"); hide("tfr_page"); hide("e0_page"); hide("mig_page"); hide("results_page")
  show("forecast_page")
  current_tab("forecast_page")
  save_current_page("forecast_page")

    my_sim <- simulations$current
    cat("[NAV_FORECAST] User clicked View Projection Results for sim:", my_sim, "\n")

    if (isTRUE(restoring_inputs())) {
      cat("[NAV_FORECAST] Inputs are restoring, delaying forecast UI setup\n")
      # Delay until restore completes, then load-only
      local_obs <- NULL
      local_obs <- observeEvent(restoring_inputs(), {
        if (!isTRUE(restoring_inputs())) {
          cat("[NAV_FORECAST] Restore complete, setting up forecast UI in load mode for sim:", my_sim, "\n")
          setup_forecast_ui(my_sim, mode = "load")
          local_obs$destroy()
        }
      }, ignoreInit = FALSE)
    } else {
      cat("[NAV_FORECAST] No restore in progress, setting up forecast UI in load mode for sim:", my_sim, "\n")
      setup_forecast_ui(my_sim, mode = "load")
    }
})

  # Persist current page to metadata on navigation button clicks (inside server)
  observeEvent(input$nav_input, { save_current_page("input_page") })
  observeEvent(input$nav_pop, { save_current_page("pop_page") })
  observeEvent(input$nav_tfr, { save_current_page("tfr_page") })
  observeEvent(input$nav_e0, { save_current_page("e0_page") })
  observeEvent(input$nav_mig, { save_current_page("mig_page") })
  observeEvent(input$start_analysis, { save_current_page("input_page") })
  observeEvent(input$forward_pop_page, { save_current_page("pop_page") })
  observeEvent(input$begin, { save_current_page("forecast_page") })
  observeEvent(input$nav_forecast, { save_current_page("forecast_page") })

  # Population data is now saved directly in the Apply button handler
  
  # Complete restoration when the restored location lands in the input
  observeEvent(input$wpp_country, {
    rl <- tryCatch({ restored_location() }, error = function(e) NULL)
    if (is.null(rl)) return()
    if (!is.null(input$wpp_country) && identical(input$wpp_country, rl)) {
      cat("[RESTORED_MATCH_DEBUG] wpp_country matched restored_location:", rl, "— finishing restore and enabling nav (if results exist)\n")
      restored_location(NULL)
      # Only finish restoring once aggregation also matched (if set)
      if (is.null(restored_aggregation())) restoring_inputs(FALSE)
      switching_sims(FALSE)
      current_sim <- tryCatch({ simulations$current }, error = function(e) NULL)
      if (!is.null(current_sim)) {
        has_results <- file.exists(file.path(sim_base_dir, current_sim, "results", "results.rds"))
        if (has_results) input_next_clicked(TRUE)
      }
    }
  }, ignoreInit = TRUE)


  # When restoration completes, print the final active inputs
  observeEvent(restoring_inputs(), {
    if (!isTRUE(restoring_inputs())) {
      switching_sims(FALSE)
      # Restore complete; additional UI enabling can be done here if needed
    }
  })
  
  # (Removed verbose input event debug observers)
  # to capture raw input data before transformations

  # Render the toggle_region UI with translated options
  output$toggle_region_ui <- renderUI({
    multiple_radio(
      input_id = "toggle_region",
      label = i18n$translate("Which geographic aggregation do you want to work with?"),
      choices = i18n$translate(c("Country", "Region")),
      selected = i18n$translate("Country"),
      position = "grouped",
      type = "radio"
    )
  })


  # When toggle matches restored aggregation, complete restoration and enable nav if results exist
  observeEvent(input$toggle_region, {
    ra <- tryCatch({ restored_aggregation() }, error = function(e) NULL)
    if (is.null(ra)) return()
    if (!is.null(input$toggle_region) && identical(input$toggle_region, ra)) {
      cat("[RESTORED_MATCH_DEBUG] toggle_region matched restored_aggregation:", ra, "— finishing restore and enabling nav (if results exist)\n")
      # Do not clear restored_aggregation yet; wait until location also matches
      if (identical(input$wpp_country, tryCatch({ restored_location() }, error=function(e) NULL))) {
        # Both match: finish restore
        restored_aggregation(NULL)
        restored_location(NULL)
        restoring_inputs(FALSE)
      }
      current_sim <- tryCatch({ simulations$current }, error = function(e) NULL)
      if (!is.null(current_sim)) {
        has_results <- file.exists(file.path(sim_base_dir, current_sim, "results", "results.rds"))
        if (has_results) input_next_clicked(TRUE)
      }
    }
  }, ignoreInit = TRUE)

  # Clear restoration values when user manually changes toggle (not during restore)
  observeEvent(input$toggle_region, {
    # Clear restoration values if user manually changes toggle (not during restore)
    if (!isTRUE(restoring_inputs()) && !isTRUE(switching_sims())) {
      restored_aggregation(NULL)
      restored_location(NULL)
    }
  }, ignoreInit = TRUE)

  # At the top with other reactives
  selected_tab_index <- reactiveVal(1)  # Start with first tab

  # Define a reactiveVal to store simulation results
  simulation_results <- reactiveVal()
  forecast_ready <- reactive({
    res <- tryCatch({ simulation_results() }, error = function(e) NULL)
    !isTRUE(restoring_inputs()) && !isTRUE(switching_sims()) && !is.null(res)
  })



  # Setup forecast UI using the original begin_forecast function
  # But control compute/load behavior through our centralized manager
  setup_forecast_ui <- function(sim_name, mode = "load") {
    cat("[FORECAST_UI] Setting up forecast UI for sim:", sim_name, "| mode:", mode, "\n")

    # Call the original begin_forecast function with appropriate flags
    # This sets up all the plot reactives and UI components properly
    begin_forecast(
      reactive_pop = reactive_pop,
      reactive_tfr = reactive_tfr,
      reactive_e0 = reactive_e0,
      reactive_mig = reactive_mig,
      wpp_starting_year = wpp_starting_year,
      wpp_ending_year = wpp_ending_year,
      input = input,
      output = output,
      simulation_results = simulation_results,
      i18n = i18n,
      results_dir = file.path(sim_base_dir, sim_name, "results"),
      force = (mode == "compute"),  # Only force compute when explicitly requested
      is_active = reactive({ current_tab() == "forecast_page" }),
      sim_name = sim_name,
      is_current_sim = reactive({ simulations$current == sim_name }),
      prefer_saved = (mode == "load"),  # Prefer saved when in load mode
      allow_compute = reactive(mode == "compute"),  # Only allow compute in compute mode
      restoring_inputs = restoring_inputs  # Pass restore guard to prevent computation during restore
    )

    cat("[FORECAST_UI] Forecast UI setup complete for sim:", sim_name, "\n")

  }

  # Add a reactive expression to track the current tab
  current_tab_name <- reactive({
    # Return the current tab name in the current language
    i18n$translate(TAB_NAMES[selected_tab_index()])
  })

  # Language change observer
  observeEvent(c(input$selected_language), {
    if (!is.null(input$selected_language)) {
      # Before changing language, store which tab we're on
      if (!is.null(input$select_id)) {
        # Find which tab is currently selected (in old language)
        old_lang <- i18n$get_translation_language()
        for (i in seq_along(TAB_NAMES)) {
          if (input$select_id == i18n$translate(TAB_NAMES[i])) {
            selected_tab_index(i)
            break
          }
        }
      }
      
      # Update language
      update_lang(input$selected_language)
      i18n$set_translation_language(input$selected_language)
      
      # Force UI update with new language
      output$select_plot_tab <- renderUI({
        selectInput(
          "select_id",
          i18n$translate("Results"),
          choices = i18n$translate(TAB_NAMES),
          selected = current_tab_name()
        )
      })
    }
  })

  # Initial UI render
  output$select_plot_tab <- renderUI({
    selectInput(
      "select_id",
      i18n$translate("Results"),
      choices = i18n$translate(TAB_NAMES),
      selected = current_tab_name()
    )
  })

  current_tab <- reactiveVal()

  # If a navigation occurred before current_tab existed, apply it now
  observe({
    pg <- tryCatch({ pending_page() }, error = function(e) NULL)
    if (!is.null(pg) && nzchar(pg)) {
      current_tab(pg)
      pending_page(NULL)
    }
  })

  observe({
    req(input$get_screen_width)
    sizes <- detect_font_size(input$get_screen_width)
    PLOTLY_TEXT_SIZE$title <- sizes$title
    PLOTLY_TEXT_SIZE$font <- sizes$font
    PLOTLY_TEXT_SIZE$type <- sizes$type
  })

  # Make the www folder available for loading images and icons
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  # Some functions need the years in number. Coerce them from
  # the beginning and  use from these reactive expressions
  wpp_starting_year <- reactive(as.numeric(input$wpp_starting_year))
  wpp_ending_year <- reactive(as.numeric(input$wpp_ending_year))

  observeEvent(input$pop_help, {
    if (current_tab() == "pop_page") {
      pop_steps <- list(
        list(
          element = "#customize_pop",
          intro = i18n$translate("Customize the starting population if you wish—otherwise keep the UN defaults and continue.")
        ),
        list(
          element = "#show_pop_results_ui",
          intro = i18n$translate("Review the baseline population numbers. They update automatically when you customise."),
          position = "left"
        ),
        list(
          element = "#left_menu .menu-inner",
          intro = i18n$translate("Use the sidebar to jump to Fertility, Life Expectancy, Migration, or Run Projection. Every section is optional."),
          position = "right",
          scrollTo = FALSE
        )
      )
      rintrojs::introjs(session, options = list(steps = pop_steps, scrollToElement = FALSE))
    }
  })

  observeEvent(input$customize_help, {
    if (grepl("modal_", current_tab())) {
      # Extract the base name (everything after "modal_")
      base_name <- sub("modal_", "", current_tab())

      # Dynamically construct widget IDs
      dt_output_id <- paste0("tmp_", base_name, "_dt")
      download_id <- paste0("download_", base_name)
      # The file input widget for some reason is no recognized
      # for that reason we simply pick the parent class from the footer
      file_input_id <- "footer-container"

      steps_df <- data.frame(
        element = c(
          paste0("#", dt_output_id),
          paste0("#", download_id),
          paste0("#", file_input_id)
        ),
        intro = c(
          i18n$translate("This is the starting year data for this indicator. If you paste new data, it should have exactly this format: same number of columns, same order of columns and importantly, the same metric. Some of these indicators are expressed in thousands, for example."),
          i18n$translate("To use your own data, download the current data to see the correct format. You can then copy your data from Excel or another source and paste it directly into the table."),
          i18n$translate("Edit the table directly or paste your data from Excel. The data should be formatted exactly as shown above. Click 'Apply' when done to save your changes.")
        )
      )

      rintrojs::introjs(session, options = list(
        steps = steps_df,
        scrollToElement = FALSE
      ))
    }
  })

  observeEvent(input$forecast_help, {
    if (current_tab() == "forecast_page") {
      forecast_steps <- list(
        list(
          element = "#select_id",
          intro = i18n$translate("Choose which projection story to view—population structure, fertility, migration, and more."),
          position = "right"
        ),
        list(
          element = "#show_forecast_results_ui",
          intro = i18n$translate("Explore the charts and downloads generated by the last Run Projection. They refresh whenever you rerun."),
          position = "left"
        ),
        list(
          element = "#left_menu .menu-inner",
          intro = i18n$translate("Need changes? Use the sidebar to revisit inputs or click Run Projection again from the Migration section."),
          position = "right",
          scrollTo = FALSE
        )
      )
      rintrojs::introjs(session, options = list(steps = forecast_steps, scrollToElement = FALSE))
    }
  })

  # TODO: remove this and import directly run_forecast. Need to fix
  # dependency issue
  library(OPPPserver)

  # ReactiveVals to store data committed from modals
  committed_pop_rv <- reactiveVal(NULL)
  committed_tfr_rv <- reactiveVal(NULL)
  committed_e0_rv  <- reactiveVal(NULL)
  committed_mig_rv <- reactiveVal(NULL)
  
  # Store raw data for modal display (exactly as saved)
  modal_raw_pop_data <- reactiveVal(NULL)
  modal_pop_params <- reactiveVal(NULL)
  
  # Track data source for population (UN Data or Custom Data)
  pop_data_source <- reactiveVal("UN Data")

  # Observe changes to inputs and reset committed data (skip during restoration/reset)
  observeEvent(list(input$wpp_country, input$wpp_starting_year, input$wpp_ending_year, input$toggle_region), {
    # If this change corresponds to the target restored location, do not re-gate
    rl <- tryCatch({ restored_location() }, error = function(e) NULL)
    if (!is.null(rl) && !is.null(input$wpp_country) && identical(input$wpp_country, rl)) {
      cat("[INPUT_CHANGE_DEBUG] Change matches restored_location; skipping gating | wpp_country:", input$wpp_country, "\n")
      return()
    }
    if (isTRUE(restoring_inputs()) || isTRUE(resetting_simulation())) {
      cat("[INPUT_CHANGE_DEBUG] Ignored change during restore/reset | toggle:", input$toggle_region,
          "| wpp_country:", input$wpp_country, "| start:", input$wpp_starting_year, "| end:", input$wpp_ending_year, "\n")
      return()
    }

    # Recompute gating based on signature match
    curr_sig <- compute_input_signature(input$toggle_region, input$wpp_country, input$wpp_starting_year, input$wpp_ending_year)
    saved_sig <- tryCatch({ sim_saved_signature() }, error = function(e) NULL)
    progressed <- tryCatch({ isTRUE(sim_progressed()) }, error = function(e) FALSE)
    next_ok <- isTRUE(progressed) && !is.null(saved_sig) && identical(curr_sig, saved_sig)
    input_next_clicked(next_ok)

    # Only clear committed data if the signature changed (i.e., inputs invalidated prior commits)
    if (!next_ok) {
      committed_pop_rv(NULL)
      committed_tfr_rv(NULL)
      committed_e0_rv(NULL)
      committed_mig_rv(NULL)
      pop_data_source("UN Data")  # Reset to UN Data when country/year changes
      data_source$tfr <- "downloaded"
      data_source$e0  <- "downloaded"
      data_source$mig <- "downloaded"
      cat("[SIGNATURE_DEBUG] Inputs changed; signature DIFFERENT — cleared committed data | curr_sig:", curr_sig,
          "| saved_sig:", saved_sig, "| progressed:", progressed, "\n")
    } else {
      cat("[SIGNATURE_DEBUG] Inputs changed; signature SAME — kept committed data | curr_sig:", curr_sig,
          "| saved_sig:", saved_sig, "| progressed:", progressed, "\n")
    }
  })

  reactive_pop <- reactive({
    user_data <- committed_pop_rv()
    if (!is.null(user_data)) {
      
      # Ensure the data has the expected column names
      # Our standard is age, popM, popF but create_pop_pyramid_plot expects age, popF, popM
      # So we need to reorder for compatibility
      if (all(c("age", "popM", "popF") %in% names(user_data))) {
        # Reorder to what create_pop_pyramid_plot expects
        user_data <- user_data[, c("age", "popF", "popM")]
      } else {
        # Fallback naming if columns are in different order
        names(user_data) <- c("age", "popF", "popM")
      }
      
      
      # Ensure age column is numeric for compatibility with run_forecast
      # Convert character ages like "0", "1", "100+" to numeric
      if (is.character(user_data$age)) {
        user_data$age <- as.numeric(gsub("\\+", "", user_data$age))
      }
      try({
        cat("[POP_SOURCE_DEBUG] Using COMMITTED population data for plot. Head:\n");
        print(utils::head(as.data.frame(user_data[, c("age","popF","popM")]), 5))
      }, silent = TRUE)
      # Convert to data.table as expected by create_pop_pyramid_plot
      return(data.table::as.data.table(user_data))
    } else {
      # Fallback to default WPP data (single ages)
      wpp_data <- safe_get_wpp_pop(input$wpp_country, wpp_starting_year(), n = 1)
      try({
        cat("[POP_SOURCE_DEBUG] Using WPP DEFAULT population data for plot:", input$wpp_country, wpp_starting_year(), "Head:\n")
        print(utils::head(as.data.frame(wpp_data), 5))
      }, silent = TRUE)
      # Convert to data.table
      return(data.table::as.data.table(wpp_data))
    }
  })

  reactive_tfr <- reactive({
    user_data <- committed_tfr_rv()
    if (!is.null(user_data)) {
      names(user_data) <- c("year", "tfr")
      # Ensure correct types
      suppressWarnings({ user_data$year <- as.numeric(user_data$year) })
      suppressWarnings({ user_data$tfr  <- as.numeric(user_data$tfr) })
      data_source$tfr <- "custom" # Indicate data is from modal commit
      try({
        cat("[TFR_SOURCE_DEBUG] Using COMMITTED TFR data for plot. Head:\n");
        print(utils::head(as.data.frame(user_data), 5))
      }, silent = TRUE)
      return(data.table::as.data.table(user_data))
    } else {
      res <- get_wpp_tfr(input$wpp_country)
      data_source$tfr <- "downloaded" # Default WPP data
      try({
        cat("[TFR_SOURCE_DEBUG] Using WPP DEFAULT TFR for plot:", input$wpp_country, "Head:\n");
        print(utils::head(as.data.frame(res), 5))
      }, silent = TRUE)
      return(data.table::as.data.table(res))
    }
  })

  reactive_e0 <- reactive({
    user_data <- committed_e0_rv()
    if (!is.null(user_data)) {
      names(user_data) <- c("year", "e0M", "e0F")
      suppressWarnings({ user_data$year <- as.numeric(user_data$year) })
      suppressWarnings({ user_data$e0M <- as.numeric(user_data$e0M) })
      suppressWarnings({ user_data$e0F <- as.numeric(user_data$e0F) })
      data_source$e0 <- "custom" # Indicate data is from modal commit
      try({
        cat("[E0_SOURCE_DEBUG] Using COMMITTED e0 data for plot. Head:\n");
        print(utils::head(as.data.frame(user_data), 5))
      }, silent = TRUE)
      return(data.table::as.data.table(user_data))
    } else {
      res <- get_wpp_e0(input$wpp_country)
      data_source$e0 <- "downloaded" # Default WPP data
      try({
        cat("[E0_SOURCE_DEBUG] Using WPP DEFAULT e0 for plot:", input$wpp_country, "Head:\n");
        print(utils::head(as.data.frame(res), 5))
      }, silent = TRUE)
      return(data.table::as.data.table(res))
    }
  })

  reactive_mig <- reactive({
    user_data <- committed_mig_rv()
    if (!is.null(user_data)) {
      names(user_data) <- c("year", "mig")
      suppressWarnings({ user_data$year <- as.numeric(user_data$year) })
      suppressWarnings({ user_data$mig  <- as.numeric(user_data$mig) })
      data_source$mig <- "custom" # Indicate data is from modal commit
      try({
        cat("[MIG_SOURCE_DEBUG] Using COMMITTED migration data for plot. Head:\n");
        print(utils::head(as.data.frame(user_data), 5))
      }, silent = TRUE)
      return(data.table::as.data.table(user_data))
    } else {
      res <- get_wpp_mig(input$wpp_country)
      data_source$mig <- "downloaded" # Default WPP data
      try({
        cat("[MIG_SOURCE_DEBUG] Using WPP DEFAULT migration for plot:", input$wpp_country, "Head:\n");
        print(utils::head(as.data.frame(res), 5))
      }, silent = TRUE)
      return(data.table::as.data.table(res))
    }
  })

  # This is a very weird thing. If I use this in the title of the TFR customize
  # tab, weird things start to happen. I think it's because of the circularity
  # that this reactive_tfr is updated from the same modal that uploads a new
  # reactive tfr. The weirdness comes when uploading a new file. I still
  # use this for generating the file name but not in the title of the modal.
  tfr_starting_year <- reactive(min(reactive_tfr()[[1]], na.rm = TRUE))

  # Handle any checks on inputs to make sure everything is correct
  handle_validity_checks(
    wpp_starting_year,
    wpp_ending_year,
    input,
    output,
    i18n,
    has_simulation = reactive({ !is.null(simulations$current) && simulations$current %in% names(simulations$data) })
  )

  # Handle pop/tfr plots/tables before analysis
  handle_before_analysis_plots(
    reactive_pop,
    reactive_tfr,
    reactive_e0,
    reactive_mig,
    wpp_starting_year,
    wpp_ending_year,
    input,
    output,
    i18n,
    sim_base_dir = sim_base_dir
  )

  # Handle navigation between steps
  handle_navigation(
    simulation_results = simulation_results,
    reactive_pop = reactive_pop,
    reactive_tfr = reactive_tfr,
    reactive_e0 = reactive_e0,
    reactive_mig = reactive_mig,
    pop_data_source = pop_data_source,
    wpp_starting_year = wpp_starting_year,
    wpp_ending_year = wpp_ending_year,
    current_tab = current_tab,
    input_next_completed = input_next_clicked,
    input = input,
    output = output,
    i18n = i18n,
    pop_to_commit_rv = committed_pop_rv,
    modal_raw_pop_data = modal_raw_pop_data,
    modal_pop_params = modal_pop_params,
    process_population_data = process_population_data,
    tfr_to_commit_rv = committed_tfr_rv,
    e0_to_commit_rv = committed_e0_rv,
    mig_to_commit_rv = committed_mig_rv,
    restoring_inputs = restoring_inputs,
    save_population_files = save_population_files
  )

  # Handle all customize actions
  handle_customize_data(
    current_pop_reactive = reactive_pop,
    current_tfr_reactive = reactive_tfr,
    current_e0_reactive = reactive_e0,
    current_mig_reactive = reactive_mig,
    pop_to_commit_rv = committed_pop_rv,
    tfr_to_commit_rv = committed_tfr_rv,
    e0_to_commit_rv = committed_e0_rv,
    mig_to_commit_rv = committed_mig_rv,
    pop_data_source = pop_data_source,
    tfr_starting_year,
    wpp_starting_year,
    wpp_ending_year,
    current_tab,
    input,
    output,
    session,
    i18n,
    save_population_files = save_population_files,
    save_tfr_files = save_tfr_files,
    save_e0_files = save_e0_files,
    save_mig_files = save_mig_files,
    modal_raw_pop_data = modal_raw_pop_data,
    modal_pop_params = modal_pop_params,
    process_population_data = process_population_data,
    modal_reset_trigger = modal_reset_trigger,
    custom_data_configs = custom_data_configs,
    last_active_modal_tab = last_active_modal_tab,
    resetting_simulation = resetting_simulation,
    just_restored_data = just_restored_data,
    opening_modal = opening_modal,
    restored_location = restored_location,
    restored_aggregation = restored_aggregation,
    restoring_inputs = restoring_inputs,
    sim_base_dir = sim_base_dir
  )


  # Everything that doesn't fit into other handles is here look tooltip server side code.
  handle_misc(wpp_starting_year, wpp_ending_year, input, output, i18n)

  # Begin simulation on button click
observeEvent(input$begin, {
  hide("mig_page")
  show("forecast_page")
  current_tab("forecast_page") # Assuming current_tab is defined in server

    # Begin forecast. This can take a up to a minute of calculation
    my_sim <- simulations$current
    cat("[BEGIN_BUTTON] User clicked Begin button for sim:", my_sim, "\n")

    # Set up forecast UI with compute mode (this can take up to a minute)
    setup_forecast_ui(my_sim, mode = "compute")

  })

  # Download handler for the report
  output$download_report <- downloadHandler(
    filename = function() {
      paste0(
        "demographic_projection_report_",
        gsub(" ", "_", input$wpp_country),
        "_",
        wpp_starting_year(),
        "-",
        wpp_ending_year(),
        ".pdf"
      )
    },
    content = function(file) {
      showNotification(i18n$t("Generating report... Please wait."), type = "message", duration = NULL, id = "report-gen-notification")

      req(simulation_results()) # Ensure simulation results are available
      results <- simulation_results()

      if (is.null(results)) {
        removeNotification("report-gen-notification")
        showNotification(i18n$t("Please run the projection first to generate a report."), type = "error", duration = 7)
        return(NULL) # Stop execution if results are not available
      }

      # Determine current selections for plot generation
      current_pop_year <- if (!is.null(input$pop_age_sex_years)) input$pop_age_sex_years else (wpp_starting_year() + 1)
      current_age_group <- if (!is.null(input$age_pop_time) && length(unique(results$population_by_time$age)) > 0) input$age_pop_time else unique(results$population_by_time$age)[1]

      # Generate all plots non-reactively
      plots <- tryCatch({
        create_all_report_plots(
          simulation_results = results,
          country = input$wpp_country,
          start_year = wpp_starting_year(),
          end_year = wpp_ending_year(),
          pop_year = current_pop_year,
          age_group = current_age_group,
          i18n = i18n
        )
      }, error = function(e) {
        removeNotification("report-gen-notification")
        showNotification(paste(i18n$t("Error generating plots for report:"), e$message), type = "error", duration = 10)
        return(NULL)
      })

      if (is.null(plots)) {
        removeNotification("report-gen-notification") # Ensure notification is removed if plot generation fails
        return(NULL)
      }

      # Generate the PDF report
      pdf_file_path <- tryCatch({
        generate_demography_report(
          plot_list = plots,
          country = input$wpp_country,
          start_year = wpp_starting_year(),
          end_year = wpp_ending_year(),
          i18n = i18n
        )
      }, error = function(e) {
        removeNotification("report-gen-notification")
        showNotification(paste(i18n$t("Error creating PDF report:"), e$message), type = "error", duration = 10)
        return(NULL)
      })

      removeNotification("report-gen-notification")
      if (!is.null(pdf_file_path) && file.exists(pdf_file_path)) {
        file.copy(pdf_file_path, file, overwrite = TRUE)
        showNotification(i18n$t("Report downloaded successfully!"), type = "message", duration = 5)
        # utils::browseURL(pdf_file_path) # No longer needed, browser handles download
      } else {
        # If pdf_file_path is NULL (error during generation) or file doesn't exist
        showNotification(i18n$t("Report generation failed. PDF could not be created or found."), type = "error", duration = 7)
      }
    }
  )


}

#' Create a translator object for internationalization
#'
#' @return A Translator object initialized with the package's translation file
#' @importFrom shiny.i18n Translator
#' @export
usei18n_local <- function() {
  translation_path <- system.file("extdata", "translation.json", package = "demographydash")
  if (translation_path == "") {
    stop("Could not find translation.json in package. Please ensure the package is installed correctly.")
  }
  i18n <- Translator$new(translation_json_path = translation_path)
  i18n$set_translation_language("en")
  i18n
}
