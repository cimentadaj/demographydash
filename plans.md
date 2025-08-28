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

## Phase 2: Create Sidebar with Current Simulation Display ⏳ PENDING

**Goal**: Display entered simulation name in sidebar and add plus button for new simulations.

### Planned Changes:
1. **app_server.R**: Initialize temp directory and simulations reactive
2. **app_ui.R**: Add reactive sidebar content that displays current simulation
3. **Show sidebar when simulation name is entered**
4. **Add plus button at bottom of sidebar for new simulations**

### Files to Modify:
- `app_server.R`: Create temp dir, initialize simulations reactiveValues
- `app_ui.R`: Add reactive sidebar content
- `app_handles.R`: Show/hide sidebar based on simulation state

### Logging to Add:
```r
cat("[PHASE2] Temp directory created:", dir.exists("/tmp/hasdaney213"), "\n")
cat("[PHASE2] Simulation name displayed in sidebar:", input$simulation_name, "\n") 
cat("[PHASE2] Simulations list:", names(simulations$data), "\n")
```

### Expected Outcome:
- Sidebar becomes visible when simulation name is entered
- Current simulation name displays in sidebar
- Plus button for creating new simulations

---

## Phase 3: Save Basic Metadata (Country/Years) ⏳ PENDING

**Goal**: Save country, years, and simulation name to disk when clicking Next.

### Planned Changes:
1. **Create simulation subdirectory structure**
2. **Save metadata.json with country/years/name when Next is clicked**
3. **Track current simulation in reactiveValues**

### Files to Modify:
- `app_server.R`: Add save functions and simulation tracking
- `app_handles.R`: Call save functions on Next click

### Directory Structure to Create:
```
/tmp/hasdaney213/
  ├── [created on app startup]
  ├── simulation_name/
  │   ├── metadata.json (country, years, data sources)
  │   ├── inputs/ (created in later phases)
  │   └── results/ (created in later phases)
```

### Logging to Add:
```r
cat("[PHASE3] Saving metadata for simulation:", sim_name, "\n")
cat("[PHASE3] Metadata saved to:", file.path(sim_dir, "metadata.json"), "\n")
cat("[PHASE3] Current simulation set to:", simulations$current, "\n")
```

### Expected Outcome:
- Temp directory structure created
- Basic metadata saved to disk
- Simulation tracking in memory

---

## Phase 4: Track & Save Population Data ⏳ PENDING

**Goal**: Save all population parameters and customizations.

### Planned Changes:
1. **Save population data source (UN/custom)**
2. **Save all population parameters** (age types, open age, reference date, processing methods)
3. **Save customizations from modal**
4. **Update metadata with population info**

### Files to Modify:
- `app_server.R`: Add population data saving logic
- `app_handles.R`: Integrate with existing population handling
- Population modal handlers to save customizations

### Logging to Add:
```r
cat("[PHASE4] Population data source:", pop_source, "\n")
cat("[PHASE4] Population parameters saved for:", simulations$current, "\n")
cat("[PHASE4] Custom pop data saved:", !is.null(custom_pop), "\n")
```

### Expected Outcome:
- Population data and parameters saved
- Custom population data preserved
- Metadata updated with population info

---

## Phase 5: Track & Save TFR Data ⏳ PENDING

**Goal**: Save TFR data source, parameters, and customizations.

### Planned Changes:
1. **Save TFR data source (UN/custom)**
2. **Save TFR parameters and customizations**
3. **Update metadata with TFR info**

### Files to Modify:
- TFR handling functions
- TFR modal customization handlers
- Metadata update functions

### Logging to Add:
```r
cat("[PHASE5] TFR data source:", data_source$tfr, "\n")
cat("[PHASE5] TFR data saved for simulation:", simulations$current, "\n")
```

### Expected Outcome:
- TFR data completely saved
- All TFR customizations preserved

---

## Phase 6: Track & Save e0 and Migration Data ⏳ PENDING

**Goal**: Complete input data saving for e0 and migration.

### Planned Changes:
1. **Save e0 data source and customizations**
2. **Save migration data source and customizations**
3. **Complete all input data saving**

### Files to Modify:
- e0 and migration handling functions
- Modal customization handlers
- Complete metadata structure

### Logging to Add:
```r
cat("[PHASE6] e0 data source:", data_source$e0, "\n")
cat("[PHASE6] Migration data source:", data_source$mig, "\n")
cat("[PHASE6] All input data saved for:", simulations$current, "\n")
```

### Expected Outcome:
- All demographic input data saved
- Complete simulation state preserved

---

## Phase 7: Implement Plus Button for New Simulation ⏳ PENDING

**Goal**: Allow users to start new simulations while keeping existing ones.

### Planned Changes:
1. **Plus button functionality to reset to input page**
2. **Clear reactive values for new simulation**
3. **Keep all simulations listed in sidebar**
4. **Proper state management for multiple simulations**

### Files to Modify:
- Sidebar UI with plus button
- New simulation creation logic
- State reset functions

### Logging to Add:
```r
cat("[PHASE7] Plus button clicked - starting new simulation\n")
cat("[PHASE7] Reactive values cleared\n")
cat("[PHASE7] Total simulations:", length(simulations$data), "\n")
```

### Expected Outcome:
- Users can create multiple simulations
- Each simulation maintains independent state
- Sidebar shows all created simulations

---

## Phase 8: Implement Simulation Switching ⏳ PENDING

**Goal**: Allow switching between simulations and restore their complete state.

### Planned Changes:
1. **Click simulation name to load its data**
2. **Restore all inputs and customizations**
3. **Navigate to appropriate page based on progress**
4. **Complete state restoration from disk**

### Files to Modify:
- Sidebar click handlers
- Data restoration functions
- Navigation logic based on simulation progress
- All reactive expressions to use selected simulation

### Logging to Add:
```r
cat("[PHASE8] Loading simulation:", selected_sim, "\n")
cat("[PHASE8] Data restored from:", sim_dir, "\n")
cat("[PHASE8] Navigation to page:", current_page, "\n")
```

### Expected Outcome:
- Seamless switching between simulations
- Complete state restoration
- Proper navigation to last active page

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
| Phase 2 | ⏳ PENDING | - | Sidebar display, plus button |
| Phase 3 | ⏳ PENDING | - | Basic metadata saving |
| Phase 4 | ⏳ PENDING | - | Population data saving |
| Phase 5 | ⏳ PENDING | - | TFR data saving |
| Phase 6 | ⏳ PENDING | - | e0 and migration saving |
| Phase 7 | ⏳ PENDING | - | New simulation creation |
| Phase 8 | ⏳ PENDING | - | Simulation switching |
| Phase 9 | ⏳ PENDING | - | Results management |
| Phase 10 | ⏳ PENDING | - | Polish and error handling |

**Current Status**: Phase 1 completed successfully. Ready for Phase 2 upon approval.