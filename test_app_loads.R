#!/usr/bin/env Rscript
# ==============================================================================
# APPLICATION LOAD TEST
# ==============================================================================
# Test script to verify the application loads correctly after unused code removal

cat("Testing ALIVA Dashboard application loading...\n")
cat("==============================================\n\n")

# Test 1: Check if global.R loads without errors
cat("Test 1: Loading global.R...\n")
tryCatch({
    source("global.R")
    cat("✅ global.R loaded successfully\n")
}, error = function(e) {
    cat("❌ ERROR loading global.R:", e$message, "\n")
    quit(status = 1)
})

# Test 2: Check if modules load correctly
cat("\nTest 2: Loading modules...\n")
tryCatch({
    source("R/load_modules.R")
    load_all_modules()
    cat("✅ All modules loaded successfully\n")
}, error = function(e) {
    cat("❌ ERROR loading modules:", e$message, "\n")
    quit(status = 1)
})

# Test 3: Check if main app.R can be parsed
cat("\nTest 3: Parsing app.R...\n")
tryCatch({
    parse("app.R")
    cat("✅ app.R parsed successfully\n")
}, error = function(e) {
    cat("❌ ERROR parsing app.R:", e$message, "\n")
    quit(status = 1)
})

# Test 4: Verify removed functions are gone
cat("\nTest 4: Verifying unused functions were removed...\n")
removed_functions <- c("generate_clean_report", "interpret_with_recommendations", 
                      "render_pdf_safely", "render_word_safely")

all_removed <- TRUE
for (func in removed_functions) {
    if (exists(func, envir = .GlobalEnv)) {
        cat("❌ Function", func, "still exists - removal failed\n")
        all_removed <- FALSE
    }
}

if (all_removed) {
    cat("✅ All unused functions successfully removed\n")
} else {
    quit(status = 1)
}

# Test 5: Check that used functions still exist
cat("\nTest 5: Verifying essential functions still exist...\n")
essential_functions <- c("load_sovi_data", "load_distance_data", "interpret_ttest", 
                        "interpret_anova", "get_numeric_columns", "format_number")

all_exist <- TRUE
for (func in essential_functions) {
    if (!exists(func, envir = .GlobalEnv)) {
        cat("❌ Essential function", func, "is missing\n")
        all_exist <- FALSE
    }
}

if (all_exist) {
    cat("✅ All essential functions are present\n")
} else {
    quit(status = 1)
}

cat("\n==============================================\n")
cat("✅ ALL TESTS PASSED!\n")
cat("The application should work correctly after unused code removal.\n")
cat("==============================================\n")

quit(status = 0)