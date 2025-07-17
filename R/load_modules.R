# Load All Modules Helper Script
# This script loads all modular UI and server components for NusaStat Dashboard

#' Load all UI and server modules for NusaStat Dashboard
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

#' Get list of available modules
#'
#' @return character vector of available module names
get_available_modules <- function() {
    modules_dir <- file.path("R", "modules")
    if (dir.exists(modules_dir)) {
        return(list.dirs(modules_dir, full.names = FALSE, recursive = FALSE))
    } else {
        return(character(0))
    }
}

#' Check module file integrity
#'
#' @param module_name character name of the module to check
#' @return logical TRUE if both UI and server files exist
check_module_integrity <- function(module_name) {
    modules_dir <- file.path("R", "modules")
    ui_file <- file.path(modules_dir, module_name, paste0(module_name, "_ui.R"))
    server_file <- file.path(modules_dir, module_name, paste0(module_name, "_server.R"))

    ui_exists <- file.exists(ui_file)
    server_exists <- file.exists(server_file)

    if (ui_exists && server_exists) {
        return(TRUE)
    } else {
        cat(paste("❌ Module", module_name, "is incomplete:\n"))
        if (!ui_exists) cat(paste("  Missing UI file:", ui_file, "\n"))
        if (!server_exists) cat(paste("  Missing Server file:", server_file, "\n"))
        return(FALSE)
    }
}

#' Validate all modules
#'
#' Checks that all required modules have both UI and server components
validate_all_modules <- function() {
    modules <- get_available_modules()

    if (length(modules) == 0) {
        cat("❌ No modules found in R/modules directory\n")
        return(FALSE)
    }

    cat("Validating module integrity...\n")
    all_valid <- TRUE

    for (module in modules) {
        if (!check_module_integrity(module)) {
            all_valid <- FALSE
        }
    }

    if (all_valid) {
        cat("✅ All modules are complete and ready to load!\n")
        return(TRUE)
    } else {
        cat("❌ Some modules are incomplete. Please check the missing files.\n")
        return(FALSE)
    }
}
