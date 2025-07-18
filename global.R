# Global configurations and functions for NusaStat Dashboard
# This file contains package installations, data loading, and utility functions

# Package Installation and Loading
# =================================

# List of required packages
required_packages <- c(
    # Core Shiny packages
    "shiny", "shinydashboard", "shinyWidgets", "shinythemes", "shinyjs",

    # Data manipulation
    "dplyr", "tidyr", "readr", "stringr", "lubridate",

    # Visualization
    "ggplot2", "plotly", "leaflet", "DT", "htmlwidgets",

    # Statistical analysis
    "car", "lmtest", "nortest", "broom", "psych",

    # Report generation
    "rmarkdown", "knitr", "pagedown", "officer", "flextable",

    # Additional utilities
    "here", "glue", "scales", "RColorBrewer", "viridis",

    # Pandoc installation (Windows only)
    "installr"
)

# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Function to check and install packages
install_if_missing <- function(packages) {
    new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
    if (length(new_packages)) {
        cat("Installing missing packages:", paste(new_packages, collapse = ", "), "\n")
        install.packages(new_packages, dependencies = TRUE, repos = "https://cran.rstudio.com/")
    }
}

# Install missing packages
install_if_missing(required_packages)

# Load all required packages
lapply(required_packages, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
})

# Pandoc Setup and Installation
# =============================

#' Check if pandoc is available and install if missing
#' @return logical indicating if pandoc is available
check_and_install_pandoc <- function() {
    # Check if pandoc is available
    if (rmarkdown::pandoc_available()) {
        pandoc_version <- rmarkdown::pandoc_version()
        cat("✓ Pandoc is available (version:", as.character(pandoc_version), ")\n")
        return(TRUE)
    }

    cat("⚠ Pandoc not found. Attempting to install...\n")

    # Only attempt installation on Windows
    if (.Platform$OS.type == "windows") {
        tryCatch(
            {
                # First try using winget (Windows Package Manager)
                if (system("winget --version", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0) {
                    cat("ℹ Installing pandoc using Windows Package Manager (winget)...\n")
                    install_result <- system("winget install --source winget --exact --id JohnMacFarlane.Pandoc --silent",
                        ignore.stdout = TRUE, ignore.stderr = TRUE
                    )

                    if (install_result == 0) {
                        cat("✓ Pandoc installation completed via winget.\n")

                        # Refresh environment variables
                        system("refreshenv", ignore.stdout = TRUE, ignore.stderr = TRUE)

                        # Check again if pandoc is now available
                        if (rmarkdown::pandoc_available()) {
                            pandoc_version <- rmarkdown::pandoc_version()
                            cat("✓ Pandoc is now available (version:", as.character(pandoc_version), ")\n")
                            return(TRUE)
                        } else {
                            cat("⚠ Pandoc installed but not detected by R. Please restart your R session.\n")
                            return(FALSE)
                        }
                    } else {
                        stop("Winget installation failed")
                    }
                } else {
                    # Fallback to installr if winget is not available
                    cat("ℹ Installing pandoc using installr package...\n")
                    installr::install.pandoc()
                    cat("✓ Pandoc installation completed via installr.\n")

                    # Check again if pandoc is now available
                    if (rmarkdown::pandoc_available()) {
                        pandoc_version <- rmarkdown::pandoc_version()
                        cat("✓ Pandoc is now available (version:", as.character(pandoc_version), ")\n")
                        return(TRUE)
                    } else {
                        cat("⚠ Pandoc installation completed but still not detected by R.\n")
                        cat("ℹ Please restart your R session or try installing pandoc manually.\n")
                        return(FALSE)
                    }
                }
            },
            error = function(e) {
                cat("✗ Failed to install pandoc automatically:", e$message, "\n")
                cat("ℹ Please install pandoc manually using one of these methods:\n")
                cat("  • Download from: https://pandoc.org/installing.html\n")
                cat("  • Use winget: winget install --source winget --exact --id JohnMacFarlane.Pandoc\n")
                cat("  • Use chocolatey: choco install pandoc\n")
                return(FALSE)
            }
        )
    } else {
        cat("ℹ Automatic pandoc installation is only supported on Windows.\n")
        cat("ℹ Please install pandoc manually from: https://pandoc.org/installing.html\n")
        return(FALSE)
    }
}

# Check and install pandoc if needed
check_and_install_pandoc()

# Global Settings
# ===============

# Set options for better performance
options(
    shiny.maxRequestSize = 50 * 1024^2, # 50MB file upload limit
    dplyr.summarise.inform = FALSE,
    warn = -1 # Suppress warnings for cleaner output
)

# Data Loading Functions
# =====================

#' Load SOVI data from CSV file
#' @return data.frame containing SOVI data with added categorical variables
load_sovi_data <- function() {
    file_path <- here::here("data", "sovi_data.csv")

    if (file.exists(file_path)) {
        data <- readr::read_csv(file_path, show_col_types = FALSE)

        # Add district names based on DISTRICTCODE (basic naming for now)
        data <- data %>%
            mutate(
                # Create readable district names
                district = paste("Kabupaten/Kota", sprintf("%04d", DISTRICTCODE)),

                # Create region based on district code patterns
                region = case_when(
                    substr(as.character(DISTRICTCODE), 1, 2) %in% c("11", "12", "13", "14", "15", "16", "17", "18", "19") ~ "Sumatera",
                    substr(as.character(DISTRICTCODE), 1, 2) %in% c("31", "32", "33", "34", "35", "36") ~ "Jawa-Bali",
                    substr(as.character(DISTRICTCODE), 1, 2) %in% c("51", "52", "53", "61", "62", "63", "64") ~ "Kalimantan-Sulawesi",
                    substr(as.character(DISTRICTCODE), 1, 2) %in% c("71", "72", "73", "74", "75", "76", "81", "82", "91", "92", "94") ~ "Indonesia Timur",
                    TRUE ~ "Lainnya"
                ),
                # Create island grouping
                island = case_when(
                    substr(as.character(DISTRICTCODE), 1, 2) %in% c("11", "12", "13", "14", "15", "16", "17", "18", "19") ~ "Sumatera",
                    substr(as.character(DISTRICTCODE), 1, 2) %in% c("31", "32", "33", "34", "35", "36") ~ "Jawa-Bali",
                    substr(as.character(DISTRICTCODE), 1, 2) %in% c("51", "52", "53") ~ "Kalimantan",
                    substr(as.character(DISTRICTCODE), 1, 2) %in% c("61", "62", "63", "64") ~ "Sulawesi",
                    substr(as.character(DISTRICTCODE), 1, 2) %in% c("71", "72", "73", "74", "75", "76") ~ "Nusa Tenggara",
                    substr(as.character(DISTRICTCODE), 1, 2) %in% c("81", "82") ~ "Maluku",
                    substr(as.character(DISTRICTCODE), 1, 2) %in% c("91", "92", "94") ~ "Papua",
                    TRUE ~ "Lainnya"
                ),
                # Create province grouping (simplified version)
                province = case_when(
                    substr(as.character(DISTRICTCODE), 1, 2) == "11" ~ "Aceh",
                    substr(as.character(DISTRICTCODE), 1, 2) == "12" ~ "Sumatera Utara",
                    substr(as.character(DISTRICTCODE), 1, 2) == "13" ~ "Sumatera Barat",
                    substr(as.character(DISTRICTCODE), 1, 2) == "14" ~ "Riau",
                    substr(as.character(DISTRICTCODE), 1, 2) == "15" ~ "Jambi",
                    substr(as.character(DISTRICTCODE), 1, 2) == "16" ~ "Sumatera Selatan",
                    substr(as.character(DISTRICTCODE), 1, 2) == "17" ~ "Bengkulu",
                    substr(as.character(DISTRICTCODE), 1, 2) == "18" ~ "Lampung",
                    substr(as.character(DISTRICTCODE), 1, 2) == "19" ~ "Kepulauan Bangka Belitung",
                    substr(as.character(DISTRICTCODE), 1, 2) == "21" ~ "Kepulauan Riau",
                    substr(as.character(DISTRICTCODE), 1, 2) == "31" ~ "DKI Jakarta",
                    substr(as.character(DISTRICTCODE), 1, 2) == "32" ~ "Jawa Barat",
                    substr(as.character(DISTRICTCODE), 1, 2) == "33" ~ "Jawa Tengah",
                    substr(as.character(DISTRICTCODE), 1, 2) == "34" ~ "DI Yogyakarta",
                    substr(as.character(DISTRICTCODE), 1, 2) == "35" ~ "Jawa Timur",
                    substr(as.character(DISTRICTCODE), 1, 2) == "36" ~ "Banten",
                    substr(as.character(DISTRICTCODE), 1, 2) == "51" ~ "Bali",
                    substr(as.character(DISTRICTCODE), 1, 2) == "52" ~ "Nusa Tenggara Barat",
                    substr(as.character(DISTRICTCODE), 1, 2) == "53" ~ "Nusa Tenggara Timur",
                    substr(as.character(DISTRICTCODE), 1, 2) == "61" ~ "Kalimantan Barat",
                    substr(as.character(DISTRICTCODE), 1, 2) == "62" ~ "Kalimantan Tengah",
                    substr(as.character(DISTRICTCODE), 1, 2) == "63" ~ "Kalimantan Selatan",
                    substr(as.character(DISTRICTCODE), 1, 2) == "64" ~ "Kalimantan Timur",
                    substr(as.character(DISTRICTCODE), 1, 2) == "65" ~ "Kalimantan Utara",
                    substr(as.character(DISTRICTCODE), 1, 2) == "71" ~ "Sulawesi Utara",
                    substr(as.character(DISTRICTCODE), 1, 2) == "72" ~ "Sulawesi Tengah",
                    substr(as.character(DISTRICTCODE), 1, 2) == "73" ~ "Sulawesi Selatan",
                    substr(as.character(DISTRICTCODE), 1, 2) == "74" ~ "Sulawesi Tenggara",
                    substr(as.character(DISTRICTCODE), 1, 2) == "75" ~ "Gorontalo",
                    substr(as.character(DISTRICTCODE), 1, 2) == "76" ~ "Sulawesi Barat",
                    substr(as.character(DISTRICTCODE), 1, 2) == "81" ~ "Maluku",
                    substr(as.character(DISTRICTCODE), 1, 2) == "82" ~ "Maluku Utara",
                    substr(as.character(DISTRICTCODE), 1, 2) == "91" ~ "Papua Barat",
                    substr(as.character(DISTRICTCODE), 1, 2) == "92" ~ "Papua",
                    substr(as.character(DISTRICTCODE), 1, 2) == "94" ~ "Papua Tengah",
                    substr(as.character(DISTRICTCODE), 1, 2) == "95" ~ "Papua Pegunungan",
                    substr(as.character(DISTRICTCODE), 1, 2) == "96" ~ "Papua Selatan",
                    TRUE ~ "Lainnya"
                )
            ) %>%
            # Convert categorical variables to factors
            mutate(
                region = as.factor(region),
                island = as.factor(island),
                province = as.factor(province)
            )

        cat("SOVI data loaded successfully:", nrow(data), "rows,", ncol(data), "columns\n")
        cat("Added categorical variables: region, island, province, district\n")
        return(data)
    } else {
        stop(
            "SOVI data file not found at: ", file_path,
            "\nPlease ensure sovi_data.csv exists in the data/ directory."
        )
    }
}

#' Load distance data from CSV file
#' @return data.frame containing distance data
load_distance_data <- function() {
    file_path <- here::here("data", "distance.csv")

    if (file.exists(file_path)) {
        data <- readr::read_csv(file_path, show_col_types = FALSE)
        cat("Distance data loaded successfully:", nrow(data), "rows,", ncol(data), "columns\n")
        return(data)
    } else {
        stop(
            "Distance data file not found at: ", file_path,
            "\nPlease ensure distance.csv exists in the data/ directory."
        )
    }
}

# Note: Dummy data creation functions removed - app now requires real data files
# The app will load real sovi_data.csv and distance.csv files only
# If these files are missing, the app will show an error message

# Utility Functions
# =================

# Variable labels for better UI display
SOVI_VARIABLE_LABELS <- list(
    # Numeric variables with descriptions
    "CHILDREN" = "Persentase Populasi Balita (CHILDREN)",
    "FEMALE" = "Persentase Populasi Perempuan (FEMALE)",
    "ELDERLY" = "Persentase Populasi Lansia ≥65 tahun (ELDERLY)",
    "FHEAD" = "Persentase Rumah Tangga Kepala Keluarga Perempuan (FHEAD)",
    "FAMILYSIZE" = "Rata-rata Jumlah Anggota Rumah Tangga (FAMILYSIZE)",
    "NOELECTRIC" = "Persentase Rumah Tangga Tanpa Listrik (NOELECTRIC)",
    "LOWEDU" = "Persentase Populasi Berpendidikan Rendah (LOWEDU)",
    "GROWTH" = "Persentase Pertumbuhan Populasi (GROWTH)",
    "POVERTY" = "Persentase Penduduk Miskin (POVERTY)",
    "ILLITERATE" = "Persentase Populasi Buta Huruf (ILLITERATE)",
    "NOTRAINING" = "Persentase RT Tanpa Pelatihan Bencana (NOTRAINING)",
    "DPRONE" = "Persentase RT di Area Rawan Bencana (DPRONE)",
    "RENTED" = "Persentase Rumah Tangga Menyewa (RENTED)",
    "NOSEWER" = "Persentase RT Tanpa Sistem Drainase (NOSEWER)",
    "TAPWATER" = "Persentase RT Menggunakan Air Ledeng (TAPWATER)",
    "POPULATION" = "Jumlah Total Populasi (POPULATION)",
    "DISTRICTCODE" = "Kode Wilayah (DISTRICTCODE)",

    # Categorical variables
    "region" = "Wilayah Regional",
    "island" = "Kelompok Pulau",
    "province" = "Provinsi"
)

#' Get variable choices with labels for UI dropdowns
#' @param data data.frame
#' @param var_type character either "numeric", "categorical", or "all"
#' @return named vector suitable for selectInput choices
get_variable_choices <- function(data, var_type = "all") {
    if (is.null(data)) {
        return(character(0))
    }

    if (var_type == "numeric") {
        vars <- get_numeric_columns(data)
    } else if (var_type == "categorical") {
        vars <- get_categorical_columns(data)
    } else {
        vars <- names(data)
    }

    # Create named vector with labels
    choices <- setNames(vars, sapply(vars, function(x) {
        if (x %in% names(SOVI_VARIABLE_LABELS)) {
            SOVI_VARIABLE_LABELS[[x]]
        } else {
            x
        }
    }))

    return(choices)
}

#' Get numeric columns from a dataframe
#' @param data data.frame
#' @return character vector of numeric column names
get_numeric_columns <- function(data) {
    if (is.null(data)) {
        return(character(0))
    }
    sapply(data, is.numeric) %>%
        .[. == TRUE] %>%
        names()
}

#' Get categorical columns from a dataframe
#' @param data data.frame
#' @return character vector of categorical column names
get_categorical_columns <- function(data) {
    if (is.null(data)) {
        return(character(0))
    }
    sapply(data, function(x) is.character(x) || is.factor(x)) %>%
        .[. == TRUE] %>%
        names()
}

#' Interpret p-value for statistical tests
#' @param p_val numeric p-value
#' @param alpha numeric significance level (default: 0.05)
#' @param h0 character null hypothesis
#' @param h1 character alternative hypothesis
#' @return character interpretation
interpret_p_value <- function(p_val, alpha = 0.05, h0 = "H0", h1 = "H1") {
    if (is.na(p_val)) {
        return("P-value tidak dapat dihitung.")
    }

    if (p_val < alpha) {
        return(glue::glue("Keputusan: Tolak {h0} (p-value = {round(p_val, 4)} < α = {alpha}). {h1}"))
    } else {
        return(glue::glue("Keputusan: Gagal tolak {h0} (p-value = {round(p_val, 4)} ≥ α = {alpha}). {h0}"))
    }
}

#' Format numbers for display
#' @param x numeric vector
#' @param digits integer number of decimal places
#' @return character vector of formatted numbers
format_number <- function(x, digits = 2) {
    if (is.numeric(x)) {
        return(format(round(x, digits), nsmall = digits))
    }
    return(as.character(x))
}

# Data validation functions
# ========================

#' Validate that data is loaded
#' @param data data.frame to validate
#' @param data_name character name of the data for error messages
#' @return logical TRUE if valid, FALSE otherwise
validate_data <- function(data, data_name = "data") {
    if (is.null(data)) {
        showNotification(
            paste("Error:", data_name, "belum dimuat."),
            type = "error",
            duration = 5
        )
        return(FALSE)
    }

    if (nrow(data) == 0) {
        showNotification(
            paste("Error:", data_name, "kosong."),
            type = "error",
            duration = 5
        )
        return(FALSE)
    }

    return(TRUE)
}

# Data validation on startup
validate_data_files <- function() {
    sovi_path <- here::here("data", "sovi_data.csv")
    distance_path <- here::here("data", "distance.csv")

    missing_files <- c()

    if (!file.exists(sovi_path)) {
        missing_files <- c(missing_files, "sovi_data.csv")
    }

    if (!file.exists(distance_path)) {
        missing_files <- c(missing_files, "distance.csv")
    }

    if (length(missing_files) > 0) {
        cat("WARNING: Missing required data files:\n")
        for (file in missing_files) {
            cat("  -", file, "\n")
        }
        cat("Please ensure these files exist in the data/ directory.\n")
        cat("The dashboard may not function correctly without these files.\n")
    } else {
        cat("All required data files found.\n")
    }
}

# Validate data files on startup
validate_data_files()

# Print startup message
cat("==========================================\n")
cat("NusaStat Dashboard initialized!\n")
cat("==========================================\n")
cat("Required packages loaded:", length(required_packages), "\n")
cat("Real data files expected in 'data/' directory\n")
cat("Ready to launch the application!\n")
cat("Application URL: http://127.0.0.1:3838\n")
cat("==========================================\n")
