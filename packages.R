# Package Management for ALIVA Dashboard
# ==========================================
# This file should be run LOCALLY before deployment, NOT during app startup
# Run this once locally: source("packages.R")

# Set CRAN mirror for faster downloads
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# List of required packages (optimized for deployment)
required_packages <- c(
    # Core Shiny packages
    "shiny", "shinydashboard", "shinyWidgets", "shinythemes", "shinyjs",

    # Data manipulation (essential)
    "dplyr", "tidyr", "readr", "stringr", "lubridate",

    # Visualization (essential)
    "ggplot2", "plotly", "leaflet", "DT", "htmlwidgets",

    # Statistical analysis (essential)
    "car", "lmtest", "nortest", "broom", "psych",

    # Lightweight report generation (removing heavy packages)
    "rmarkdown", "knitr", "kableExtra",

    # Additional utilities (essential only)
    "here", "glue", "scales", "RColorBrewer", "viridis"
)

# Heavy packages that cause compilation issues - REMOVED for deployment optimization
# These were causing qpdf compilation timeouts:
# "pagedown", "officer", "flextable"

cat("Installing packages locally...\n")
cat("Note: This should be run LOCALLY, not during deployment\n\n")

# Function to install packages with better error handling
install_required_packages <- function(packages) {
    for (pkg in packages) {
        cat("Checking package:", pkg, "\n")
        if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
            cat("Installing:", pkg, "\n")
            try(
                {
                    install.packages(pkg,
                        dependencies = TRUE,
                        repos = "https://cran.rstudio.com/",
                        type = "binary"
                    ) # Prefer binary packages
                },
                silent = FALSE
            )
        } else {
            cat("Already installed:", pkg, "\n")
        }
    }
}

# Install packages
install_required_packages(required_packages)

cat("\n==========================================\n")
cat("Package installation completed!\n")
cat("Now run: renv::snapshot() to create renv.lock\n")
cat("==========================================\n")
