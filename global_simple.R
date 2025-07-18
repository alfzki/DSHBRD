# Simplified Global Configuration for NusaStat Dashboard
# This version works with basic packages and provides fallbacks

cat("Loading NusaStat Dashboard (Simplified Mode)...\n")

# Basic required packages that are usually pre-installed
basic_packages <- c("utils", "stats", "graphics", "grDevices")

# Try to load shiny - if not available, provide instructions
if (!requireNamespace("shiny", quietly = TRUE)) {
    cat("Error: Shiny package is not installed.\n")
    cat("Please install required packages first:\n")
    cat("install.packages(c('shiny', 'shinydashboard', 'dplyr', 'ggplot2', 'readr'))\n")
    stop("Required packages not available")
}

# Load essential packages with fallbacks
library(shiny)

# Try to load other essential packages
packages_to_try <- c("dplyr", "ggplot2", "readr")
loaded_packages <- c()

for (pkg in packages_to_try) {
    if (requireNamespace(pkg, quietly = TRUE)) {
        library(pkg, character.only = TRUE)
        loaded_packages <- c(loaded_packages, pkg)
        cat("✓ Loaded:", pkg, "\n")
    } else {
        cat("⚠ Package not available:", pkg, "\n")
    }
}

# Create basic data if needed
create_basic_sample_data <- function() {
    if (!dir.exists("data")) {
        dir.create("data")
    }

    # Create minimal sample data
    n <- 100
    basic_data <- data.frame(
        id = 1:n,
        variable1 = rnorm(n, 50, 10),
        variable2 = rnorm(n, 30, 8),
        variable3 = rnorm(n, 70, 15),
        category = sample(c("A", "B", "C"), n, replace = TRUE)
    )

    write.csv(basic_data, "data/sovi_data.csv", row.names = FALSE)
    cat("✓ Created basic sample data\n")
    return(basic_data)
}

# Data loading function with fallbacks
load_data_simple <- function() {
    if (file.exists("data/sovi_data.csv")) {
        if ("readr" %in% loaded_packages) {
            return(readr::read_csv("data/sovi_data.csv", show_col_types = FALSE))
        } else {
            return(read.csv("data/sovi_data.csv"))
        }
    } else {
        cat("Data file not found, creating basic sample data...\n")
        return(create_basic_sample_data())
    }
}

# Variable labels for better UI display (simplified version)
SOVI_VARIABLE_LABELS_SIMPLE <- list(
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

    # Basic sample data variables
    "variable1" = "Variabel 1 (variable1)",
    "variable2" = "Variabel 2 (variable2)",
    "variable3" = "Variabel 3 (variable3)",
    "id" = "ID (id)"
)

# Utility functions
get_variable_choices_simple <- function(data, var_type = "all") {
    if (is.null(data)) {
        return(character(0))
    }

    if (var_type == "numeric") {
        vars <- get_numeric_columns_simple(data)
    } else if (var_type == "categorical") {
        vars <- get_categorical_columns_simple(data)
    } else {
        vars <- names(data)
    }

    # Create named vector with labels
    choices <- setNames(vars, sapply(vars, function(x) {
        if (x %in% names(SOVI_VARIABLE_LABELS_SIMPLE)) {
            SOVI_VARIABLE_LABELS_SIMPLE[[x]]
        } else {
            x
        }
    }))

    return(choices)
}

get_numeric_columns_simple <- function(data) {
    if (is.null(data)) {
        return(character(0))
    }
    numeric_cols <- sapply(data, is.numeric)
    names(numeric_cols)[numeric_cols]
}

get_categorical_columns_simple <- function(data) {
    if (is.null(data)) {
        return(character(0))
    }
    cat_cols <- sapply(data, function(x) is.character(x) || is.factor(x))
    names(cat_cols)[cat_cols]
}

# Simple interpretation function
interpret_p_value_simple <- function(p_val, alpha = 0.05) {
    if (is.na(p_val)) {
        return("P-value tidak dapat dihitung.")
    }

    if (p_val < alpha) {
        return(paste("P-value =", round(p_val, 4), "< α =", alpha, ". Tolak H0."))
    } else {
        return(paste("P-value =", round(p_val, 4), "≥ α =", alpha, ". Gagal tolak H0."))
    }
}

cat("Basic configuration loaded successfully!\n")
cat("Loaded packages:", paste(loaded_packages, collapse = ", "), "\n")
cat("Note: Some advanced features may not be available without all packages.\n")
