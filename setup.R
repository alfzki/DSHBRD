# Quick Setup for ALIVA Dashboard Deployment
# ==========================================
# Run this script to prepare your app for deployment

cat("===========================================\n")
cat("ALIVA Dashboard - Deployment Setup\n")
cat("===========================================\n\n")

# Step 1: Install packages locally
cat("Step 1: Installing required packages...\n")
source("packages.R")

# Step 2: Initialize renv
cat("\nStep 2: Initializing renv...\n")
source("init_renv.R")

# Step 3: Verify setup
cat("\nStep 3: Verifying setup...\n")

# Check if renv.lock was created
if (file.exists("renv.lock")) {
    cat("✓ renv.lock created successfully\n")
} else {
    cat("✗ renv.lock not found - please check init_renv.R output\n")
}

# Check if .Rprofile was created
if (file.exists(".Rprofile")) {
    cat("✓ .Rprofile created successfully\n")
} else {
    cat("✗ .Rprofile not found - renv may not be properly initialized\n")
}

# Check essential data files
data_files <- c("data/sovi_data.csv", "data/distance.csv", "data/indonesia_kabkota.geojson")
for (file in data_files) {
    if (file.exists(file)) {
        cat("✓", file, "found\n")
    } else {
        cat("⚠", file, "not found - app may not work without this file\n")
    }
}

cat("\n===========================================\n")
cat("Setup completed!\n")
cat("\n")
cat("Your app is now ready for deployment.\n")
cat("To deploy, run: source('deploy.R')\n")
cat("\n")
cat("Or manually deploy with rsconnect:\n")
cat("library(rsconnect)\n")
cat("deployApp()\n")
cat("===========================================\n")
