# System Requirements Check for NusaStat Dashboard

cat("NusaStat Dashboard - System Requirements Check\n")
cat("==============================================\n\n")

# Check R version
r_version <- R.version
cat("R Version Information:\n")
cat("- Version:", r_version$version.string, "\n")
cat("- Platform:", r_version$platform, "\n")
cat("- Architecture:", r_version$arch, "\n\n")

# Minimum R version requirement
min_r_version <- "4.0.0"
current_version <- paste(r_version$major, r_version$minor, sep = ".")

if (compareVersion(current_version, min_r_version) >= 0) {
    cat("✓ R version meets minimum requirements (", min_r_version, ")\n")
} else {
    cat("✗ R version is too old. Please upgrade to R", min_r_version, "or higher\n")
}

# Check essential packages
cat("\nChecking essential packages...\n")
essential_packages <- c("shiny", "dplyr", "ggplot2", "readr")

for (pkg in essential_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
        pkg_version <- packageVersion(pkg)
        cat("✓", pkg, "(version", as.character(pkg_version), ")\n")
    } else {
        cat("✗", pkg, "not installed\n")
    }
}

# Check data directory
cat("\nChecking project structure...\n")
required_dirs <- c("data", "R", "www")
for (dir in required_dirs) {
    if (dir.exists(dir)) {
        cat("✓ Directory", dir, "exists\n")
    } else {
        cat("✗ Directory", dir, "missing\n")
    }
}

# Check essential files
required_files <- c("app.R", "global.R", "README.md")
for (file in required_files) {
    if (file.exists(file)) {
        cat("✓ File", file, "exists\n")
    } else {
        cat("✗ File", file, "missing\n")
    }
}

# Check memory
memory_info <- gc()
cat("\nMemory Information:\n")
cat("- Used memory:", round(sum(memory_info[, 2]), 2), "MB\n")

# Final recommendation
cat("\nRecommendations:\n")
cat("================\n")
if (compareVersion(current_version, min_r_version) >= 0) {
    cat("✓ Your R installation meets the requirements\n")
} else {
    cat("⚠ Please update R to version", min_r_version, "or higher\n")
}

if (all(sapply(essential_packages, requireNamespace, quietly = TRUE))) {
    cat("✓ Essential packages are available\n")
} else {
    cat("⚠ Some essential packages are missing. Run install.packages() for missing packages\n")
}

if (all(dir.exists(required_dirs)) && all(file.exists(required_files))) {
    cat("✓ Project structure is complete\n")
    cat("\nYou can now launch the dashboard using:\n")
    cat("source('launch_dashboard.R')\n")
} else {
    cat("⚠ Some project files are missing. Please ensure all files are in place\n")
}

cat("\n" + rep("=", 50) + "\n")
cat("System check completed!\n")
