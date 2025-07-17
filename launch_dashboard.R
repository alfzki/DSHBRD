# NusaStat Dashboard Launcher
# This script ensures all dependencies are installed before launching

cat("NusaStat Dashboard Launcher\n")
cat("===========================\n\n")

# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Check R version
cat("R Version:", R.version.string, "\n")
cat("Working Directory:", getwd(), "\n\n")

# Core packages that must be available
core_packages <- c("shiny", "dplyr", "ggplot2")

cat("Checking core packages...\n")
for (pkg in core_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        cat("Installing core package:", pkg, "\n")
        install.packages(pkg, repos = "https://cran.rstudio.com/")
    } else {
        cat("✓", pkg, "is available\n")
    }
}

# Check if data files exist
cat("\nChecking data files...\n")
if (file.exists("data/sovi_data.csv")) {
    cat("✓ SOVI data file found\n")
} else {
    cat("⚠ SOVI data file not found. Please provide the required data in 'data/sovi_data.csv'.\n")
}

if (file.exists("data/distance.csv")) {
    cat("✓ Distance data file found\n")
} else {
    cat("⚠ Distance data file not found. Please provide the required data in 'data/distance.csv'.\n")
}

# Load global configuration
cat("\nLoading global configuration...\n")
if (file.exists("global.R")) {
    tryCatch(
        {
            source("global.R")
            cat("✓ Global configuration loaded successfully\n")
        },
        error = function(e) {
            cat("✗ Error loading global configuration:", e$message, "\n")
            cat("Attempting to launch with minimal configuration...\n")
            library(shiny)
        }
    )
} else {
    cat("✗ global.R not found\n")
    stop("Global configuration file is required")
}

# Check for development mode argument
args <- commandArgs(trailingOnly = TRUE)
is_dev_mode <- "--dev" %in% args

if (is_dev_mode) {
    cat("\n** DEVELOPMENT MODE ACTIVATED **\n")
    cat("** Shiny autoreload is ON.      **\n\n")
    options(shiny.autoreload = TRUE)
}

# Launch the dashboard
cat("Launching NusaStat Dashboard...\n")
if (!is_dev_mode) {
    cat("Dashboard will be available at: http://127.0.0.1:3838\n")
}
cat("Press Ctrl+C to stop the dashboard\n\n")

# Check if app.R exists
if (!file.exists("app.R")) {
    stop("app.R not found in current directory")
}

# Launch with error handling
tryCatch(
    {
        shiny::runApp("app.R",
            port = 3838,
            host = "127.0.0.1",
            launch.browser = FALSE
        )
    },
    error = function(e) {
        cat("Error launching dashboard:", e$message, "\n")
        cat("Please check the error messages above and try again.\n")
    }
)
