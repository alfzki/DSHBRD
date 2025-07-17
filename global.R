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
    "here", "glue", "scales", "RColorBrewer", "viridis"
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
#' @return data.frame containing SOVI data
load_sovi_data <- function() {
    file_path <- here::here("data", "sovi_data.csv")

    if (file.exists(file_path)) {
        data <- readr::read_csv(file_path, show_col_types = FALSE)
        cat("SOVI data loaded successfully:", nrow(data), "rows,", ncol(data), "columns\n")
        return(data)
    } else {
        # Create sample data if file doesn't exist
        cat("SOVI data file not found. Creating sample data...\n")
        return(create_sample_sovi_data())
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
        # Create sample data if file doesn't exist
        cat("Distance data file not found. Creating sample data...\n")
        return(create_sample_distance_data())
    }
}

#' Create sample SOVI data for demonstration
#' @return data.frame with sample SOVI data
create_sample_sovi_data <- function() {
    set.seed(123)
    n <- 514 # Number of districts/regencies in Indonesia

    # Generate sample data
    sovi_data <- data.frame(
        kode_wilayah = sprintf("%04d", 1:n),
        nama_wilayah = paste("Kabupaten", 1:n),
        provinsi = rep(paste("Provinsi", 1:34), length.out = n),

        # Social vulnerability indicators (continuous variables)
        kepadatan_penduduk = round(rnorm(n, 500, 200), 0),
        rasio_ketergantungan = round(rnorm(n, 45, 10), 2),
        persentase_lansia = round(rnorm(n, 8, 2), 2),
        persentase_balita = round(rnorm(n, 7, 2), 2),
        persentase_disabilitas = round(rnorm(n, 3, 1), 2),
        persentase_buta_huruf = round(rnorm(n, 5, 2), 2),
        persentase_kemiskinan = round(rnorm(n, 12, 4), 2),
        persentase_pengangguran = round(rnorm(n, 8, 3), 2),
        indeks_kesehatan = round(rnorm(n, 70, 10), 2),
        indeks_pendidikan = round(rnorm(n, 75, 8), 2),
        indeks_ekonomi = round(rnorm(n, 65, 12), 2),
        akses_listrik = round(rnorm(n, 95, 5), 2),
        akses_air_bersih = round(rnorm(n, 85, 10), 2),
        akses_sanitasi = round(rnorm(n, 80, 15), 2),

        # Derived indices
        sovi_score = round(rnorm(n, 50, 15), 2),
        vulnerability_level = sample(c("Rendah", "Sedang", "Tinggi"), n, replace = TRUE, prob = c(0.3, 0.4, 0.3))
    )

    # Ensure realistic ranges
    sovi_data$kepadatan_penduduk <- pmax(50, sovi_data$kepadatan_penduduk)
    sovi_data$rasio_ketergantungan <- pmax(20, pmin(80, sovi_data$rasio_ketergantungan))
    sovi_data$persentase_lansia <- pmax(2, pmin(20, sovi_data$persentase_lansia))
    sovi_data$persentase_balita <- pmax(3, pmin(15, sovi_data$persentase_balita))
    sovi_data$persentase_disabilitas <- pmax(0.5, pmin(10, sovi_data$persentase_disabilitas))
    sovi_data$persentase_buta_huruf <- pmax(0, pmin(20, sovi_data$persentase_buta_huruf))
    sovi_data$persentase_kemiskinan <- pmax(2, pmin(30, sovi_data$persentase_kemiskinan))
    sovi_data$persentase_pengangguran <- pmax(1, pmin(20, sovi_data$persentase_pengangguran))
    sovi_data$indeks_kesehatan <- pmax(40, pmin(90, sovi_data$indeks_kesehatan))
    sovi_data$indeks_pendidikan <- pmax(50, pmin(95, sovi_data$indeks_pendidikan))
    sovi_data$indeks_ekonomi <- pmax(30, pmin(85, sovi_data$indeks_ekonomi))
    sovi_data$akses_listrik <- pmax(70, pmin(100, sovi_data$akses_listrik))
    sovi_data$akses_air_bersih <- pmax(50, pmin(100, sovi_data$akses_air_bersih))
    sovi_data$akses_sanitasi <- pmax(30, pmin(100, sovi_data$akses_sanitasi))
    sovi_data$sovi_score <- pmax(0, pmin(100, sovi_data$sovi_score))

    return(sovi_data)
}

#' Create sample distance data for demonstration
#' @return data.frame with sample distance data
create_sample_distance_data <- function() {
    set.seed(456)
    n <- 514

    distance_data <- data.frame(
        kode_wilayah = sprintf("%04d", 1:n),
        jarak_ke_ibukota_provinsi = round(rnorm(n, 150, 80), 0),
        jarak_ke_pusat_kesehatan = round(rnorm(n, 25, 15), 0),
        jarak_ke_pusat_pendidikan = round(rnorm(n, 20, 10), 0),
        jarak_ke_pusat_ekonomi = round(rnorm(n, 35, 20), 0),
        waktu_tempuh_ibukota = round(rnorm(n, 3, 1.5), 1)
    )

    # Ensure realistic ranges
    distance_data$jarak_ke_ibukota_provinsi <- pmax(10, distance_data$jarak_ke_ibukota_provinsi)
    distance_data$jarak_ke_pusat_kesehatan <- pmax(1, distance_data$jarak_ke_pusat_kesehatan)
    distance_data$jarak_ke_pusat_pendidikan <- pmax(1, distance_data$jarak_ke_pusat_pendidikan)
    distance_data$jarak_ke_pusat_ekonomi <- pmax(1, distance_data$jarak_ke_pusat_ekonomi)
    distance_data$waktu_tempuh_ibukota <- pmax(0.5, distance_data$waktu_tempuh_ibukota)

    return(distance_data)
}

# Utility Functions
# =================

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

#' Create a professional-looking box for UI
#' @param title character box title
#' @param content UI content
#' @param status character box status (default: "primary")
#' @param width integer box width (default: 12)
#' @return UI box element
create_info_box <- function(title, content, status = "primary", width = 12) {
    fluidRow(
        column(
            width = width,
            box(
                title = title,
                status = status,
                solidHeader = TRUE,
                width = NULL,
                content
            )
        )
    )
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

# Initialize sample data files if they don't exist
initialize_sample_data <- function() {
    # Create data directory if it doesn't exist
    if (!dir.exists(here::here("data"))) {
        dir.create(here::here("data"), recursive = TRUE)
    }

    sovi_path <- here::here("data", "sovi_data.csv")
    distance_path <- here::here("data", "distance.csv")

    if (!file.exists(sovi_path)) {
        sample_sovi <- create_sample_sovi_data()
        readr::write_csv(sample_sovi, sovi_path)
        cat("Sample SOVI data created at:", sovi_path, "\n")
    }

    if (!file.exists(distance_path)) {
        sample_distance <- create_sample_distance_data()
        readr::write_csv(sample_distance, distance_path)
        cat("Sample distance data created at:", distance_path, "\n")
    }
}

# Create sample data files on startup
initialize_sample_data()

# Print startup message
cat("==========================================\n")
cat("NusaStat Dashboard initialized successfully!\n")
cat("==========================================\n")
cat("Required packages loaded:", length(required_packages), "\n")
cat("Sample data files created in 'data/' directory\n")
cat("Ready to launch the application!\n")
cat("Application URL: http://127.0.0.1:3838\n")
cat("==========================================\n")
