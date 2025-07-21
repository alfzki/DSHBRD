# Initialize renv for ALIVA Dashboard
# ====================================
# Run this script LOCALLY to set up renv and create lock file

# Install renv if not already installed
if (!require("renv", quietly = TRUE)) {
    install.packages("renv")
}

library(renv)

cat("Initializing renv for ALIVA Dashboard...\n")

# Initialize renv (creates renv.lock and .Rprofile)
renv::init(
    project = getwd(),
    restart = FALSE,
    bare = FALSE
)

cat("Installing required packages...\n")

# Install packages from packages.R
source("packages.R")

cat("Creating snapshot (renv.lock file)...\n")

# Create snapshot to lock package versions
renv::snapshot(prompt = FALSE)

cat("\n==========================================\n")
cat("renv initialization completed!\n")
cat("Files created:\n")
cat("  - renv.lock (package versions)\n")
cat("  - .Rprofile (renv activation)\n")
cat("  - renv/ folder (renv infrastructure)\n")
cat("\n")
cat("Your app is now ready for deployment!\n")
cat("The renv.lock file will ensure consistent\n")
cat("package versions across environments.\n")
cat("==========================================\n")
