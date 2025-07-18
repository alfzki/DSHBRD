# Modular Architecture Documentation

## Overview

The NusaStat Dashboard has been successfully refactored from a monolithic architecture to a modular structure following separation of concerns principles. This document outlines the new architecture and provides guidance for future development.

## Architecture Changes

### Before (Monolithic)
- `R/ui_modules.R` - Single file containing all UI modules (8 modules)
- `R/server_modules.R` - Single file containing main server modules 
- `R/additional_server_modules.R` - Single file containing additional server modules

### After (Modular)
```
R/
├── modules/
│   ├── beranda/
│   │   ├── beranda_ui.R
│   │   └── beranda_server.R
│   ├── manajemen_data/
│   │   ├── manajemen_data_ui.R
│   │   └── manajemen_data_server.R
│   ├── eksplorasi/
│   │   ├── eksplorasi_ui.R
│   │   └── eksplorasi_server.R
│   ├── uji_asumsi/
│   │   ├── uji_asumsi_ui.R
│   │   └── uji_asumsi_server.R
│   ├── uji_rata/
│   │   ├── uji_rata_ui.R
│   │   └── uji_rata_server.R
│   ├── uji_prop_var/
│   │   ├── uji_prop_var_ui.R
│   │   └── uji_prop_var_server.R
│   ├── uji_anova/
│   │   ├── uji_anova_ui.R
│   │   └── uji_anova_server.R
│   └── regresi/
│       ├── regresi_ui.R
│       └── regresi_server.R
├── load_modules.R
└── legacy/
    ├── ui_modules_original.R
    ├── server_modules_original.R
    └── additional_server_modules_original.R
```

## Module Structure

Each module follows a consistent structure:

### UI Module Template
```r
# Module UI
# Description of module functionality

#' Module UI Function
#' 
#' Description
#' 
#' @param id Module ID for namespacing
module_ui <- function(id) {
    ns <- NS(id)
    # UI components here
}
```

### Server Module Template
```r
# Module Server
# Server logic for module functionality

#' Module Server Function
#' 
#' Description
#' 
#' @param id Module ID for namespacing
#' @param values Reactive values object containing shared data
module_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        # Server logic here
    })
}
```

## Benefits of Modular Architecture

1. **Separation of Concerns**: Each module handles specific functionality
2. **Maintainability**: Easier to locate and modify specific features
3. **Scalability**: Easy to add new modules or modify existing ones
4. **Collaboration**: Multiple developers can work on different modules
5. **Testing**: Individual modules can be tested independently
6. **Code Organization**: Clear structure makes codebase more navigable

## Module Descriptions

### beranda
- **Purpose**: Dashboard home page and welcome content
- **Features**: Metadata display, PDF downloads, project information
- **Dependencies**: DT, rmarkdown

### manajemen_data
- **Purpose**: Data management and categorization
- **Features**: Variable categorization, interval/quantile methods
- **Dependencies**: dplyr, DT

### eksplorasi
- **Purpose**: Data exploration and visualization
- **Features**: Summary statistics, interactive plots, leaflet maps
- **Dependencies**: ggplot2, plotly, leaflet

### uji_asumsi
- **Purpose**: Statistical assumption testing
- **Features**: Normality tests, homogeneity tests, Q-Q plots
- **Dependencies**: car (for Levene's test)

### uji_rata
- **Purpose**: Mean difference testing
- **Features**: One-sample and two-sample t-tests
- **Dependencies**: Base R statistical functions

### uji_prop_var
- **Purpose**: Proportion and variance testing
- **Features**: Proportion tests, chi-square variance tests
- **Dependencies**: Base R prop.test

### uji_anova
- **Purpose**: Analysis of variance testing
- **Features**: One-way and two-way ANOVA, post-hoc tests
- **Dependencies**: Base R aov, TukeyHSD

### regresi
- **Purpose**: Multiple linear regression analysis
- **Features**: Model building, assumption testing, diagnostics
- **Dependencies**: car (VIF), lmtest (Breusch-Pagan)

## Module Loading System

The `load_modules.R` file provides:

1. **load_all_modules()**: Sources all UI and server modules

## Usage

### In app.R
```r
# Load modular components
source("R/load_modules.R")
load_all_modules()
```

### Adding New Modules
1. Create new directory in `R/modules/`
2. Create `modulename_ui.R` and `modulename_server.R`
3. Follow the module template structure
4. Add module name to `load_modules.R`
5. Add UI and server calls to `app.R`

## Validation and Testing

Run validation to ensure all modules are properly structured:
```r
source("R/load_modules.R")
validate_all_modules()
```

## Migration Notes

- Original monolithic files are preserved in `R/legacy/` directory
- All functionality has been preserved in the modular structure
- App.R has been updated to use the new modular loading system
- No changes to global.R or other core files were required

## Future Development

- Each module can now be developed independently
- New statistical features can be added as separate modules
- Module-specific tests can be implemented
- Documentation can be maintained per module
- Code reviews can focus on specific functionality

## Maintenance

- Use `validate_all_modules()` to check integrity after changes
- Keep module dependencies documented
- Follow consistent naming conventions
- Maintain module documentation
