# DemographyDash Application Overview

This document provides a high-level overview of the DemographyDash Shiny application architecture and flow.

## What This App Does

DemographyDash is a demographic analysis tool that allows users to:
- Select countries/regions and time periods
- View and customize demographic data (population, fertility, life expectancy, migration)
- Run population projections
- Visualize results through interactive charts and tables

## Architecture

This is a **Golem-based Shiny application** with:
- Modular structure following Golem conventions
- Semantic UI components via `shiny.semantic`
- Internationalization support via `shiny.i18n`
- Data sourced from the `OPPPserver` package (UN WPP data)

## Key Files & Their Roles

### Core Application Files
- **`R/app_ui.R`** - Main UI structure and page layouts
- **`R/app_server.R`** - Main server logic, data fetching, and reactive management
- **`R/app_config.R`** - Configuration and Golem setup

### Feature Modules
- **`R/app_handles.R`** - Event handlers for navigation, customization, and user interactions
- **`R/app_show.R`** - UI components for input forms and result displays
- **`R/app_displays.R`** - Visualization functions (plots, charts, tables)
- **`R/app_forecast.R`** - Population projection and simulation logic

### Support Files
- **`R/app_utils.R`** - Utility functions
- **`inst/extdata/translation.json`** - Internationalization strings
- **`inst/app/www/`** - Static assets (CSS, images)

## Application Flow

1. **Landing Page** → User starts here
2. **Input Selection** → Country/region and year selection
3. **Data Review Pages** → Sequential review of:
   - Population pyramid
   - Total Fertility Rate (TFR)
   - Life Expectancy (e0)
   - Migration
4. **Projection** → Run demographic projections
5. **Results** → Interactive visualizations and data export

## Core Functions

### Data Management
- `reactive_pop()`, `reactive_tfr()`, `reactive_e0()`, `reactive_mig()` - Reactive data sources
- `get_wpp_*()` functions - Fetch UN data via OPPPserver

### UI Creation
- `create_modal_ui()` - Reusable modal factory for data customization
- `show_*_results_ui()` - Result display components
- `location_selector_ui()` - Country/region selection

### Visualization
- `create_pop_pyramid_plot()` - Population pyramid charts
- `create_tfr_plot()` - Fertility rate visualizations
- Various plot functions in `app_displays.R`

### Navigation & Flow
- `handle_navigation()` - Page transitions and flow control
- `handle_customize_data()` - Modal interactions and data updates
- `begin_forecast()` - Initiate projection calculations

## Data Flow

1. **Selection** → User selects country and years
2. **Fetch** → Data retrieved from OPPPserver (or custom uploads)
3. **Display** → Data shown in editable tables/charts
4. **Customize** → Optional user modifications via modals
5. **Process** → Projections run on final data
6. **Results** → Visualizations generated and displayed

## Common Patterns

### Reactive Pattern
- Data stored in reactive expressions (`reactive_*`)
- Custom data in reactive values (`committed_*_rv`)
- Observers handle UI updates and navigation

### Modal Pattern
- Customize buttons trigger modals
- Modals contain editable tables (rhandsontable)
- Apply/Cancel pattern for data commits

### Navigation Pattern
- Hidden/shown divs for page transitions
- `current_tab` tracks active page
- Forward/back buttons for sequential flow

## Key Dependencies

- **Golem** - Application framework
- **shiny.semantic** - UI components
- **OPPPserver** - UN demographic data
- **rhandsontable** - Interactive data tables
- **plotly** - Interactive visualizations

---

This overview provides the essential context for understanding the application structure. For specific implementation details, refer to the individual files mentioned above.