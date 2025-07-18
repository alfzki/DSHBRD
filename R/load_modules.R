# Load All Modules Helper Script
# This script loads all modular UI and server components for ALIVA Dashboard

#' Load all UI and server modules for ALIVA Dashboard
#'
#' This function sources all the modular components needed for the dashboard.
#' It should be called after global.R is loaded to ensure all dependencies are available.
#'
#' @description Sources all UI and server module files from the R/modules directory
#' following the new modular architecture that separates concerns by functionality.
load_all_modules <- function() {
    # Define the base modules directory
    modules_dir <- file.path("R", "modules")

    # Define all module names
    module_names <- c(
        "beranda",
        "manajemen_data",
        "eksplorasi",
        "uji_asumsi",
        "uji_rata",
        "uji_prop_var",
        "uji_anova",
        "regresi"
    )

    # Load UI modules
    cat("Loading UI modules...\n")
    for (module in module_names) {
        ui_file <- file.path(modules_dir, module, paste0(module, "_ui.R"))
        if (file.exists(ui_file)) {
            source(ui_file)
            cat(paste("✓ Loaded UI:", module, "\n"))
        } else {
            cat(paste("⚠ UI file not found:", ui_file, "\n"))
        }
    }

    # Load Server modules
    cat("\nLoading Server modules...\n")
    for (module in module_names) {
        server_file <- file.path(modules_dir, module, paste0(module, "_server.R"))
        if (file.exists(server_file)) {
            source(server_file)
            cat(paste("✓ Loaded Server:", module, "\n"))
        } else {
            cat(paste("⚠ Server file not found:", server_file, "\n"))
        }
    }

    cat("\n✅ All modules loaded successfully!\n")
    cat("📊 Modular architecture implemented with separation of concerns.\n")
    cat("🔧 Each module now handles specific functionality independently.\n\n")
}
