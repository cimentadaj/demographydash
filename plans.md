# Multi-Simulation Support Implementation Plan

## Overview
Implement a multi-simulation workflow allowing users to create, name, save, and switch between multiple demographic simulations. Each phase will be implemented incrementally with logging and testing.

## Testing Strategy
- Add console logs at each critical point
- Use Playwright to trigger UI interactions
- Monitor logs via BashOutput
- Restart app between phases
- Port will be shown after each phase for manual testing

---

## Phase 1: Add Simulation Name UI (No functionality, just UI) ✅ COMPLETED

**Goal**: Add simulation name field and basic validation without breaking existing functionality.

### Changes Made:
- **app_show.R**: Added simulation name input field to `show_input_ui()` function
- **app_ui.R**: Added sidebar layout structure (initially hidden)
- **app_handles.R**: Modified `handle_validity_checks()` to validate simulation name
- **app_server.R**: Added Phase 1 logging and updated function call
- **translation.json**: Added translations for simulation name field and validation

### Files Modified:
1. `R/app_show.R` - Added simulation name field with required styling
2. `R/app_ui.R` - Wrapped main content in sidebar layout
3. `R/app_handles.R` - Added validation for simulation name before Next button
4. `R/app_server.R` - Added logging and updated function parameters
5. `inst/extdata/translation.json` - Added English/French translations

### Logging Added:
```r
cat("[PHASE1] Simulation name input rendered\n")
cat("[PHASE1] Sidebar structure created\n") 
cat("[PHASE1] Simulation name validation:", !is.null(input$simulation_name), "\n")
```

### Additional Fixes Made:
- **Fixed placeholder text display** - Replaced custom `tags$input()` with standard Shiny `textInput()`
- **Improved placeholder text** - Changed to "Please give a name to your analysis..."
- **Updated translations** - Added proper French translation for the new placeholder text

### Test Results:
- ✅ Simulation name field appears at top of input page
- ✅ Required field styling with asterisk
- ✅ Validation prevents Next button when empty
- ✅ Next button appears when name is entered
- ✅ All logging working correctly
- ✅ No breaking changes to existing functionality
- ✅ Fixed HTML span issue in placeholder text
- ✅ Clean, professional placeholder: "Please give a name to your analysis..."

### Final Port Tested: http://127.0.0.1:5321

---

## Phase 2: Create Sidebar with Current Simulation Display ✅ COMPLETED

**Goal**: Provide a persistent, left sidebar to manage simulations; display the current simulation; allow creating new simulations; and prevent advancing without a saved selection.

### Changes Made:
1. Always‑visible left sidebar with dynamic header and dropdown
   - Header shows `Simulations (N)` where N is the count of saved simulations.
   - Dropdown lists only saved simulations (no drafts); selecting updates the current simulation and URL.
2. Inline “Add a new simulation” flow in the sidebar (no modal)
   - Clicking “+ Add a new simulation” reveals an inline form (name + Create/Cancel).
   - Name must be non-empty and unique; on Create, the simulation is saved and set current; the form hides.
3. Current simulation display
   - Sidebar shows “Current simulation: <name>” for the active saved simulation.
4. Guard “Next” until a saved and selected simulation exists
   - Replaces the former name-on-main-page approach; shows a clear red panel message without ❌ when blocked.
5. Removed the Simulation Name field from the main input page
   - Naming now only happens via the sidebar’s “Add a new simulation” button.

### Files Modified:
- `R/app_ui.R`: Sidebar structure; dynamic `sim_header`, `sim_switcher_ui`, `no_sims_state`, `current_sim_name`, and `new_sim_inline` outputs.
- `R/app_server.R`: Temp directory init; `simulations` reactive store; inline add form handlers; dynamic header/dropdown/labels; URL sync retained.
- `R/app_show.R`: Removed Simulation Name field from the main page UI.
- `R/app_handles.R`: Updated `handle_validity_checks()` to block “Next” until a saved, selected simulation exists; removed ❌ from messages.

### Logging Added:
```r
cat("[PHASE2] Temp directory created:", dir.exists(sim_base_dir), "\n")
cat("[PHASE2] Simulation created via inline form:", nm, "\n")
cat("[PHASE2] Simulations dropdown updated:", length(names(simulations$data)), "\n")
```

### Final Outcome:
- Sidebar is persistent; empty state shown until first simulation is created.
- Users add simulations via the single, obvious entry point in the sidebar.
- “Next” is available only when a saved and selected simulation exists.

---

## Phase 3: Save Basic Metadata (Country/Years) ✅ COMPLETED

**Goal**: Save country, years, and simulation name to disk when clicking Next.

### Changes Made:
1. Created simulation subdirectory structure under `/tmp/hasdaney213/<simulation_name>/` with `inputs/` and `results/`.
2. Save `metadata.json` on every Next click (pop/tfr/e0/mig step transitions). The file includes:
   - `name`, `created_at`, `updated_at`, `last_trigger`
   - `country`, `start_year`, `end_year`
   - `data_sources`: `population`, `tfr`, `e0`, `mig`
3. Continue tracking current simulation in `reactiveValues`.

### Files to Modify:
- `app_server.R`: Add save functions and simulation tracking
- `app_handles.R`: Call save functions on Next click

### Directory Structure:
```
/tmp/hasdaney213/
  ├── [created on app startup]
  ├── simulation_name/
  │   ├── metadata.json (country, years, data sources)
  │   ├── inputs/ (created in later phases)
  │   └── results/ (created in later phases)
```

### Logging Added:
```r
cat("[PHASE3] Saving metadata for simulation:", sim_name, "\n")
cat("[PHASE3] Metadata saved to:", meta_path, "\n")
cat("[PHASE3] Metadata content:\n", json_txt, "\n")
```

### Outcome:
- Temp directory structure created on startup
- Metadata saved/updated on each Next click with full details
- Simulation tracking maintained in memory

---

## Phase 4: Track & Save Population Data ✅ COMPLETED

**Goal**: Save all population parameters and customizations.

### Changes Made:
1. Save population data source (UN/Custom) snapshot alongside data and params.
2. Save population parameters snapshot (aggregation, location, reference year; placeholders for age/open-age/method).
3. Save customizations immediately on population modal "OK" (committed data), and also snapshot on step transitions (Next buttons).
4. Update metadata with population info and last save timestamp.

### Files Modified:
- `R/app_server.R`: Added `save_population_files()` and hooks on modal commit + step transitions.
- `R/app_handles.R`: No signature change; existing modal commit triggers now observed by server to save data.

### Logging Added:
```r
cat("[PHASE4] Population files saved for:", sim_name, "(trigger:", trigger, ")\n")
cat("[PHASE4] Population data source:", pop_data_source(), "\n")
cat("[PHASE4] pop.csv saved to:", pop_path, " rows:", nrow(pop_dt), "\n")
cat("[PHASE4] pop_params.json content:\n", params_json, "\n")
```

### Outcome:
- Population data and parameters saved to `/tmp/hasdaney213/<simulation_name>/inputs/`.
- Custom population data preserved on modal commit; snapshots also taken on step transitions.
- Metadata updated with population info and `last_pop_saved` timestamp.

---

## Phase 5: Track & Save TFR Data ✅ COMPLETED

**Goal**: Save TFR data source, parameters, and customizations.

### Changes Made:
1. **Created save_tfr_files function following population pattern**
2. **Added raw_data_override parameter to capture user input before transformations**
3. **Hooked save_tfr_files to TFR modal Apply button with correct timing**
4. **Added TFR saving to step transitions (Next buttons)**
5. **Updated metadata with TFR info and last_tfr_saved timestamp**

### Files Modified:
- `R/app_server.R`: Added `save_tfr_files()` function and step transition hooks
- `R/app_handles.R`: Modified TFR modal Apply button handler to save raw data

### Logging Added:
```r
cat("[PHASE5] TFR files saved for:", sim_name, "(trigger:", trigger, ")\n")
cat("[PHASE5] TFR data source:", src, "\n")
cat("[PHASE5] tfr.csv saved to:", tfr_path, " rows:", nrow(df_save), "\n")
cat("[PHASE5] tfr_params.json content:\n", params_json, "\n")
```

### Outcome:
- TFR data and parameters saved to `/tmp/hasdaney213/<simulation_name>/inputs/`
- Raw user input preserved before any transformations
- Complete TFR customizations preserved with metadata tracking

---

## Phase 6: Track & Save e0 and Migration Data ✅ COMPLETED

**Goal**: Complete input data saving for e0 and migration.

### Changes Made:
1. **Created save_e0_files and save_mig_files functions following established pattern**
2. **Added raw_data_override parameters to capture user input before transformations**
3. **Hooked both functions to modal Apply buttons with correct timing**
4. **Added e0 and migration saving to step transitions (Next buttons)**
5. **Complete all input data saving with metadata tracking**

### Files Modified:
- `R/app_server.R`: Added `save_e0_files()` and `save_mig_files()` functions and step transition hooks
- `R/app_handles.R`: Modified e0 and migration modal Apply button handlers to save raw data

### Logging Added:
```r
cat("[PHASE6] e0 files saved for:", sim_name, "(trigger:", trigger, ")\n")
cat("[PHASE6] e0 data source:", src, "\n")
cat("[PHASE6] Migration files saved for:", sim_name, "(trigger:", trigger, ")\n")
cat("[PHASE6] Migration data source:", src, "\n")
```

### Outcome:
- All demographic input data saved (pop, tfr, e0, mig) to `/tmp/hasdaney213/<simulation_name>/inputs/`
- Complete simulation state preserved with metadata tracking
- Raw user input preserved for all data types before any transformations

---

## Phase 7: Implement Plus Button for New Simulation ✅ COMPLETED

**Goal**: Allow users to start new simulations while keeping existing ones.

### Changes Made:
1. Plus button creates/selects a new simulation and resets state
2. Input page is reset to defaults (years, region toggle, country)
3. Sidebar hidden on landing; visible elsewhere (no flash)
4. Proper state management and directory scaffolding

### Outcome:
- Multiple simulations with clean starts on the input page; sidebar behavior refined

---

## Phase 8: Implement Simulation Switching ✅ COMPLETED

**Goal**: Allow switching between simulations and restore their complete state.

### Changes Made:
1. Hard reset in-memory state before restore; load pop/TFR/e0/mig per sim
2. UN vs Custom isolation in modal state; rHandsontable re-render on reset
3. UN modal logs the head of the shown data for debugging
4. Interpolation change preserves Custom grid values (no wipe)
5. Save last page on nav; restore page on sim switch

### Outcome:
- Seamless switching, complete state restoration, and return to last page per sim

---

## Phase 9: Save & Restore Forecast Results ⏳ PENDING

**Goal**: Save simulation results and restore them when switching.

### Planned Changes:
1. **Save forecast results when run_forecast completes**
2. **Restore results when switching to completed simulation**
3. **Handle results display for selected simulation**
4. **Proper cleanup and management of result files**

### Files to Modify:
- `app_forecast.R`: Save results to simulation directory
- Result restoration logic
- Results UI to work with selected simulation

### Logging to Add:
```r
cat("[PHASE9] Forecast results saved for:", simulations$current, "\n")
cat("[PHASE9] Results directory:", output_dir, "\n")
cat("[PHASE9] Results restored:", !is.null(simulations$data[[sim_name]]$results), "\n")
```

### Expected Outcome:
- Forecast results preserved per simulation
- Results display correctly when switching
- Efficient result management

---

## Phase 10: Polish & Error Handling ⏳ PENDING

**Goal**: Add loading indicators, handle edge cases, and final cleanup.

### Planned Changes:
1. **Add loading indicators for simulation operations**
2. **Handle edge cases** (simulation name conflicts, disk errors, etc.)
3. **Final cleanup and optimization**
4. **User experience improvements**

### Files to Modify:
- Error handling throughout the application
- Loading indicators
- User feedback improvements
- Final testing and validation

### Logging to Add:
```r
cat("[PHASE10] Error handling active\n")
cat("[PHASE10] Final validation complete\n")
```

### Expected Outcome:
- Robust, production-ready multi-simulation support
- Excellent user experience
- Proper error handling and recovery

---

## Implementation Notes

### Key Data Structures:
```r
simulations <- reactiveValues(
  current = NULL,  # Current simulation name
  data = list()    # List of all simulations
)

# Each simulation structure:
simulations$data[["sim_name"]] <- list(
  metadata = list(
    country = input$wpp_country,
    start_year = wpp_starting_year(),
    end_year = wpp_ending_year(),
    data_sources = list(tfr = "UN", e0 = "custom", etc.)
  ),
  inputs = list(
    pop = reactive_pop(),
    tfr = reactive_tfr(),
    e0 = reactive_e0(),
    mig = reactive_mig()
  ),
  results = forecast_results,
  timestamp = Sys.time()
)
```

### Directory Structure:
```
/tmp/hasdaney213/
  ├── simulation_A/
  │   ├── metadata.json
  │   ├── inputs/
  │   │   ├── pop.csv
  │   │   ├── tfr.csv
  │   │   ├── e0.csv
  │   │   └── mig.csv
  │   └── results/
  │       └── [OPPPserver output files]
```

### Workflow for Each Phase:
1. Implement changes with logging
2. Kill current R process if running
3. Restart app with `R -e "devtools::load_all(); run_app()"`
4. Test with Playwright to trigger logs
5. Monitor logs via BashOutput
6. Stop and report port for manual testing
7. Wait for approval before next phase

---

## Progress Summary

| Phase | Status | Files Modified | Key Features |
|-------|--------|----------------|--------------|
| Phase 1 | ✅ COMPLETED | 5 files | Simulation name UI, validation, sidebar structure |
| Phase 2 | ✅ COMPLETED | 4 files | Sidebar display, inline simulation creation |
| Phase 3 | ✅ COMPLETED | 2 files | Basic metadata saving, directory structure |
| Phase 4 | ✅ COMPLETED | 2 files | Population data saving with raw input preservation |
| Phase 5 | ✅ COMPLETED | 2 files | TFR data saving |
| Phase 6 | ✅ COMPLETED | 2 files | e0 and migration data saving |
| Phase 7 | ✅ COMPLETED | 3 files | New simulation creation, input reset, sidebar behavior |
| Phase 8 | ✅ COMPLETED | 3 files | State restoration + last page restore |
| Phase 9 | ⏳ PENDING | - | Results management |
| Phase 10 | ⏳ PENDING | - | Polish and error handling |

**Current Status**: Phases 1–8 completed. Inputs saved/restored per sim; UI isolation and last-page persistence implemented. Ready for Phase 9.
