# ==============================================================================
# SCRIPT VALIDASI DASHBOARD ALIVA
# ==============================================================================
#
# Tujuan: Script validasi dan pengujian komprehensif untuk modul dashboard
# Penulis: Tim Dashboard ALIVA
# Terakhir Dibuat: Juli 2025
#
# Deskripsi:
# Script ini memvalidasi integritas dan fungsionalitas semua modul dashboard,
# memeriksa pemuatan data, menguji fungsi statistik, dan memverifikasi
# komponen UI.
#
# Penggunaan:
# source("validate_dashboard.R")
# run_full_validation()
#
# Fitur Validasi:
# - Validasi pemuatan data dan integritas
# - Pengujian semua modul UI dan server
# - Verifikasi fungsi statistik
# - Pengujian performa dan komponen UI
# ==============================================================================

# Memuat pustaka yang diperlukan
suppressPackageStartupMessages({
    library(shiny)
    library(shinydashboard)
    library(DT)
    library(plotly)
    library(ggplot2)
    library(leaflet)
    library(dplyr)
    library(here)
})

# Memuat semua modul dan utilitas
source(here("R", "load_modules.R"))
source(here("global.R"))

#' Fungsi Validasi Utama
#'
#' @description Menjalankan validasi komprehensif semua komponen dashboard
#' @return List berisi hasil validasi
#' @export
#' @author Tim Dashboard ALIVA
run_full_validation <- function() {
    cat("=================================================\n")
    cat("[INFO] ALIVA DASHBOARD VALIDATION SCRIPT\n")
    cat("=================================================\n\n")

    # Initialize results
    validation_results <- list(
        timestamp = Sys.time(),
        overall_status = "UNKNOWN",
        data_validation = NULL,
        module_validation = NULL,
        function_validation = NULL,
        ui_validation = NULL,
        performance_metrics = NULL
    )

    # Run validation steps
    tryCatch(
        {
            cat("[DATA] Step 1: Data Loading Validation...\n")
            validation_results$data_validation <- validate_data_loading()

            cat("\n[MODULES] Step 2: Module Validation...\n")
            validation_results$module_validation <- validate_modules()

            cat("\n[STATS] Step 3: Statistical Function Validation...\n")
            validation_results$function_validation <- validate_statistical_functions()

            cat("\n[UI] Step 4: UI Component Validation...\n")
            validation_results$ui_validation <- validate_ui_components()

            cat("\n[PERFORMANCE] Step 5: Performance Testing...\n")
            validation_results$performance_metrics <- run_performance_tests() # Determine overall status
            validation_results$overall_status <- determine_overall_status(validation_results)

            # Print summary
            print_validation_summary(validation_results)
        },
        error = function(e) {
            cat("❌ CRITICAL ERROR in validation:\n")
            cat("Error:", e$message, "\n")
            validation_results$overall_status <- "FAILED"
            validation_results$error <- e$message
        }
    )

    return(validation_results)
}

#' Validate data loading functionality
#'
#' @return List with data validation results
validate_data_loading <- function() {
    results <- list(
        sovi_data_status = "UNKNOWN",
        distance_data_status = "UNKNOWN",
        geojson_status = "UNKNOWN",
        data_integrity = "UNKNOWN"
    )

    # Check SOVI data by loading it
    tryCatch(
        {
            sovi_data <- load_sovi_data()
            if (is.data.frame(sovi_data) && nrow(sovi_data) == 511 && ncol(sovi_data) >= 17) {
                results$sovi_data_status <- "PASSED"
                cat("  [OK] SOVI data loaded correctly (", nrow(sovi_data), " x ", ncol(sovi_data), ")\n")
            } else {
                results$sovi_data_status <- "FAILED"
                cat("  [ERROR] SOVI data dimension mismatch\n")
            }
        },
        error = function(e) {
            results$sovi_data_status <- "FAILED"
            cat("  [ERROR] SOVI data loading failed:", e$message, "\n")
        }
    )

    # Check Distance data by loading it
    tryCatch(
        {
            distance_data <- load_distance_data()
            if (is.data.frame(distance_data) && nrow(distance_data) == 511) {
                results$distance_data_status <- "PASSED"
                cat("  [OK] Distance data loaded correctly (", nrow(distance_data), " x ", ncol(distance_data), ")\n")
            } else {
                results$distance_data_status <- "FAILED"
                cat("  [ERROR] Distance data dimension mismatch\n")
            }
        },
        error = function(e) {
            results$distance_data_status <- "FAILED"
            cat("  [ERROR] Distance data loading failed:", e$message, "\n")
        }
    )

    # Check GeoJSON file
    geojson_path <- here("data", "indonesia_kabkota.geojson")
    if (file.exists(geojson_path)) {
        results$geojson_status <- "PASSED"
        cat("  [OK] GeoJSON file found\n")
    } else {
        results$geojson_status <- "WARNING"
        cat("  [WARNING] GeoJSON file not found (optional)\n")
    }

    # Data integrity checks
    if (results$sovi_data_status == "PASSED" && exists("sovi_data")) {
        # Check for required columns
        required_cols <- c("DISTRICTCODE", "CHILDREN", "FEMALE", "ELDERLY", "POVERTY")
        if (all(required_cols %in% colnames(sovi_data))) {
            # Check for missing values
            missing_pct <- sum(is.na(sovi_data)) / (nrow(sovi_data) * ncol(sovi_data)) * 100
            if (missing_pct < 5) {
                results$data_integrity <- "PASSED"
                cat("  [OK] Data integrity check passed (", round(missing_pct, 2), "% missing)\n")
            } else {
                results$data_integrity <- "WARNING"
                cat("  [WARNING] High missing data percentage (", round(missing_pct, 2), "%)\n")
            }
        } else {
            results$data_integrity <- "FAILED"
            cat("  [ERROR] Required columns missing\n")
        }
    }

    return(results)
}

#' Validate all dashboard modules
#'
#' @return List with module validation results
validate_modules <- function() {
    modules <- c(
        "beranda", "manajemen_data", "eksplorasi", "uji_asumsi",
        "uji_rata", "uji_prop_var", "uji_anova", "regresi"
    )

    results <- list()

    for (module in modules) {
        cat("  Testing module:", module, "...")

        # Check if server and UI files exist
        server_file <- here("R", "modules", module, paste0(module, "_server.R"))
        ui_file <- here("R", "modules", module, paste0(module, "_ui.R"))

        server_exists <- file.exists(server_file)
        ui_exists <- file.exists(ui_file)

        if (server_exists && ui_exists) {
            # Try to source the files
            tryCatch(
                {
                    source(server_file)
                    source(ui_file)
                    results[[module]] <- "PASSED"
                    cat(" ✅\n")
                },
                error = function(e) {
                    results[[module]] <- paste("FAILED:", e$message)
                    cat(" ❌\n")
                }
            )
        } else {
            results[[module]] <- "FAILED: Missing files"
            cat(" ❌\n")
        }
    }

    return(results)
}

#' Validate statistical functions
#'
#' @return List with statistical function validation results
validate_statistical_functions <- function() {
    results <- list()

    # Load data for testing
    tryCatch(
        {
            sovi_data <- load_sovi_data()

            # Test basic statistics
            test_data <- sovi_data$CHILDREN[1:100] # Use first 100 observations

            # Test summary statistics
            summary_result <- summary(test_data)
            results$summary_stats <- if (length(summary_result) == 6) "PASSED" else "FAILED"

            # Test standard deviation
            sd_result <- sd(test_data, na.rm = TRUE)
            results$standard_deviation <- if (is.numeric(sd_result) && !is.na(sd_result)) "PASSED" else "FAILED"

            # Test correlation
            if (ncol(sovi_data) >= 2) {
                test_data2 <- sovi_data$FEMALE[1:100]
                cor_result <- cor(test_data, test_data2, use = "complete.obs")
                results$correlation <- if (is.numeric(cor_result) && !is.na(cor_result)) "PASSED" else "FAILED"
            }

            # Test normality test
            if (length(test_data) >= 3) {
                shapiro_result <- shapiro.test(test_data[1:50]) # Shapiro-Wilk needs <= 5000 obs
                results$normality_test <- if ("statistic" %in% names(shapiro_result)) "PASSED" else "FAILED"
            }

            cat("  ✅ Statistical functions validated\n")
        },
        error = function(e) {
            results$statistical_functions <- paste("FAILED:", e$message)
            cat("  ❌ Statistical function errors:", e$message, "\n")
        }
    )

    return(results)
}

#' Validate UI components
#'
#' @return List with UI validation results
validate_ui_components <- function() {
    results <- list()

    # Test if utility functions exist
    utility_functions <- c(
        "get_numeric_columns", "get_categorical_columns",
        "validate_data", "format_number"
    )

    for (func in utility_functions) {
        if (exists(func)) {
            results[[func]] <- "PASSED"
        } else {
            results[[func]] <- "FAILED"
            cat("  ❌ Missing utility function:", func, "\n")
        }
    }

    # Test CSS file
    css_file <- here("www", "custom.css")
    if (file.exists(css_file)) {
        css_size <- file.info(css_file)$size
        if (css_size > 1000) { # At least 1KB
            results$css_file <- "PASSED"
            cat("  ✅ CSS file found and valid\n")
        } else {
            results$css_file <- "WARNING"
            cat("  ⚠️ CSS file too small\n")
        }
    } else {
        results$css_file <- "FAILED"
        cat("  ❌ CSS file not found\n")
    }

    return(results)
}

#' Run performance tests
#'
#' @return List with performance metrics
run_performance_tests <- function() {
    results <- list()

    tryCatch(
        {
            sovi_data <- load_sovi_data()

            # Test data loading time
            start_time <- Sys.time()
            temp_data <- sovi_data
            end_time <- Sys.time()
            results$data_access_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

            # Test statistical computation time
            start_time <- Sys.time()
            summary(sovi_data$CHILDREN)
            cor(sovi_data$CHILDREN, sovi_data$FEMALE, use = "complete.obs")
            end_time <- Sys.time()
            results$stats_computation_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

            # Memory usage
            results$memory_usage_mb <- as.numeric(object.size(sovi_data)) / 1024^2

            cat("  ✅ Performance metrics collected\n")
            cat("    - Data access time:", round(results$data_access_time, 4), "seconds\n")
            cat("    - Stats computation time:", round(results$stats_computation_time, 4), "seconds\n")
            cat("    - Memory usage:", round(results$memory_usage_mb, 2), "MB\n")
        },
        error = function(e) {
            cat("  ❌ Performance testing failed:", e$message, "\n")
        }
    )

    return(results)
}

#' Determine overall validation status
#'
#' @param results List of all validation results
#' @return Character string with overall status
determine_overall_status <- function(results) {
    # Count failures and warnings
    all_results <- unlist(results, recursive = TRUE)

    failures <- sum(grepl("FAILED", all_results, ignore.case = TRUE))
    warnings <- sum(grepl("WARNING", all_results, ignore.case = TRUE))
    passes <- sum(grepl("PASSED", all_results, ignore.case = TRUE))

    if (failures == 0 && warnings == 0) {
        return("EXCELLENT")
    } else if (failures == 0 && warnings <= 2) {
        return("GOOD")
    } else if (failures <= 2) {
        return("ACCEPTABLE")
    } else {
        return("NEEDS_ATTENTION")
    }
}

#' Print validation summary
#'
#' @param results Complete validation results
print_validation_summary <- function(results) {
    cat("\n" %s% rep("=", 50) %s% "\n")
    cat("📋 VALIDATION SUMMARY\n")
    cat(rep("=", 50) %s% "\n\n")

    cat("🕐 Validation completed at:", format(results$timestamp), "\n")
    cat("🎯 Overall Status:", results$overall_status, "\n\n")

    # Data validation summary
    if (!is.null(results$data_validation)) {
        cat("📊 DATA VALIDATION:\n")
        for (test in names(results$data_validation)) {
            status <- results$data_validation[[test]]
            icon <- if (status == "PASSED") "✅" else if (status == "WARNING") "⚠️" else "❌"
            cat("  ", icon, " ", test, ": ", status, "\n")
        }
        cat("\n")
    }

    # Module validation summary
    if (!is.null(results$module_validation)) {
        cat("🔧 MODULE VALIDATION:\n")
        for (module in names(results$module_validation)) {
            status <- results$module_validation[[module]]
            icon <- if (status == "PASSED") "✅" else "❌"
            cat("  ", icon, " ", module, ": ", status, "\n")
        }
        cat("\n")
    }

    # Performance summary
    if (!is.null(results$performance_metrics)) {
        cat("⚡ PERFORMANCE METRICS:\n")
        metrics <- results$performance_metrics
        if (!is.null(metrics$data_access_time)) {
            cat("  📈 Data Access: ", round(metrics$data_access_time, 4), "s\n")
        }
        if (!is.null(metrics$stats_computation_time)) {
            cat("  🧮 Stats Computation: ", round(metrics$stats_computation_time, 4), "s\n")
        }
        if (!is.null(metrics$memory_usage_mb)) {
            cat("  💾 Memory Usage: ", round(metrics$memory_usage_mb, 2), "MB\n")
        }
        cat("\n")
    }

    # Recommendations
    cat("💡 RECOMMENDATIONS:\n")
    if (results$overall_status == "EXCELLENT") {
        cat("  🎉 All systems are working perfectly! Dashboard is ready for production.\n")
    } else if (results$overall_status == "GOOD") {
        cat("  👍 Dashboard is in good condition with minor warnings that can be addressed later.\n")
    } else if (results$overall_status == "ACCEPTABLE") {
        cat("  ⚡ Dashboard is functional but has some issues that should be addressed.\n")
    } else {
        cat("  🚨 Dashboard requires immediate attention to resolve critical issues.\n")
    }

    cat("\n" %s% rep("=", 50) %s% "\n")
}

# Helper operator for string concatenation
`%s%` <- function(x, y) paste0(x, y)

# Run validation if script is sourced directly
if (sys.nframe() == 0) {
    validation_results <- run_full_validation()
}
