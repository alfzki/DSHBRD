# Test script to verify data loading
source("global.R")

cat("Testing data loading...\n")

# Test SOVI data loading
sovi_data <- load_sovi_data()
cat("SOVI data structure:\n")
cat("Columns:", paste(names(sovi_data), collapse = ", "), "\n")
cat("Number of rows:", nrow(sovi_data), "\n")
cat("Number of columns:", ncol(sovi_data), "\n")

# Check categorical variables
cat("\nCategorical variables:\n")
categorical_vars <- get_categorical_columns(sovi_data)
cat("Categorical columns found:", paste(categorical_vars, collapse = ", "), "\n")

# Check numeric variables
cat("\nNumeric variables:\n")
numeric_vars <- get_numeric_columns(sovi_data)
cat("Numeric columns found:", paste(numeric_vars, collapse = ", "), "\n")

# Check region distribution
if ("region" %in% names(sovi_data)) {
    cat("\nRegion distribution:\n")
    print(table(sovi_data$region))
}

# Check island distribution
if ("island" %in% names(sovi_data)) {
    cat("\nIsland distribution:\n")
    print(table(sovi_data$island))
}
