<!-- Use this file to provide workspace-specific custom instructions to Copilot. For more details, visit https://code.visualstudio.com/docs/copilot/copilot-customization#_use-a-githubcopilotinstructionsmd-file -->

# NusaStat Dashboard - AI Coding Agent Instructions

## Project Overview

NusaStat is a modular R Shiny dashboard for Indonesian social vulnerability analysis. It features data management, exploration, assumption testing, inferential statistics, and regression, with a focus on maintainability, extensibility, and statistical rigor.

## Architecture & Key Patterns

- **Modular Structure:**
  - All dashboard features are implemented as Shiny modules in `R/modules/`, each with paired `*_ui.R` and `*_server.R` files.
  - Modules are loaded via `R/load_modules.R` using `load_all_modules()` in `app.R`.
  - Separation of concerns: UI and server logic are strictly separated.
- **Core Files:**
  - `app.R`: Main entry, sources modules and defines UI/server.
  - `global.R`: Loads packages, data, and utility functions.
  - `validate_dashboard.R`: Runs integrity and feature tests.
  - `data/`: Contains CSV data files (auto-generated if missing).
  - `www/custom.css`: Custom styling for responsive, accessible UI.
- **Module Template:**
  - UI: `module_ui(id)` with namespacing via `NS(id)`.
  - Server: `module_server(id, values)` using `moduleServer()`.
  - See `MODULAR_ARCHITECTURE.md` for full template and migration notes.

## Developer Workflow

- **Setup:**
  - Install dependencies: `source('global.R')`
  - Launch dashboard: `source('app.R')` (or use VS Code task)
  - Validate modules: `source('validate_dashboard.R')`
- **Development:**
  - Add new features as modules in `R/modules/`.
  - Update `load_modules.R` and `app.R` to register new modules.
  - Use sample data in `data/` for testing; real data integration is supported.
- **Testing:**
  - Each module can be tested independently.
  - Use `validate_all_modules()` for integrity checks.
- **Troubleshooting:**
  - See `TROUBLESHOOTING.md` and `QUICK_REFERENCE.md` for common fixes and commands.

## Conventions & Best Practices

- **R Style:** CamelCase for variables, snake_case for functions.
- **Shiny:** Use reactive programming, validate inputs, handle errors with user notifications.
- **Statistical Analysis:** Always check assumptions before tests; provide automated interpretations.
- **Performance:** Large datasets are auto-sampled; optimize with efficient data handling and reactive invalidation.
- **Security:** Validate all user inputs, use safe file operations, and restrict resource usage.
- **Documentation:** All modules and functions should be documented; see `README.md`, `USER_MANUAL.md`, and `FEATURES.md` for reference.

## Integration Points

- **External Libraries:** Uses `shiny`, `shinydashboard`, `dplyr`, `ggplot2`, `plotly`, `DT`, `car`, `lmtest`, and others as needed per module.
- **Export/Reporting:** Results can be downloaded in PDF, CSV, and Word formats; report generation uses R Markdown.

## Example: Adding a New Module

1. Create `R/modules/newfeature/newfeature_ui.R` and `newfeature_server.R`.
2. Register in `R/load_modules.R`.
3. Add UI/server calls in `app.R`.
4. Document in `FEATURES.md` and update tests in `validate_dashboard.R`.

## Quick Reference

- Launch: `source('app.R')` or VS Code task
- Validate: `source('validate_dashboard.R')`
- Docs: `README.md`, `USER_MANUAL.md`, `FEATURES.md`, `TROUBLESHOOTING.md`

---

For unclear or missing sections, please provide feedback to iterate and improve these instructions.
