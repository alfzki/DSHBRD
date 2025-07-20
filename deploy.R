# Deploy ALIVA Dashboard to shinyapps.io
# =====================================
# This script handles the deployment process after optimization

library(rsconnect)

cat("Preparing ALIVA Dashboard deployment...\n")

# Check if renv.lock exists
if (!file.exists("renv.lock")) {
    cat("ERROR: renv.lock file not found!\n")
    cat("Please run: source('init_renv.R') first\n")
    stop("Deployment cannot proceed without renv.lock")
}

# Check if essential files exist
essential_files <- c("app.R", "global.R", "data/sovi_data.csv", "data/distance.csv")
missing_files <- c()

for (file in essential_files) {
    if (!file.exists(file)) {
        missing_files <- c(missing_files, file)
    }
}

if (length(missing_files) > 0) {
    cat("ERROR: Missing essential files:\n")
    for (file in missing_files) {
        cat("  -", file, "\n")
    }
    stop("Please ensure all required files exist")
}

cat("All essential files found ✓\n")
cat("renv.lock file found ✓\n")
cat("Ready for deployment!\n\n")

# Deploy to shinyapps.io
cat("Deploying to shinyapps.io...\n")

deployApp(
    appDir = ".",
    appName = "aliva-dashboard",
    appTitle = "ALIVA Dashboard - Statistical Analysis",
    account = NULL, # Will use default account
    forceUpdate = TRUE,
    launch.browser = TRUE,
    lint = FALSE,
    metadata = list(
        asMultiple = FALSE,
        asStatic = FALSE,
        ignoredFiles = ".Rhistory;.Rdata;.DS_Store;.git;.gitignore;init_renv.R;packages.R;DEPLOYMENT_GUIDE.md;deploy.R"
    )
)

cat("\n==========================================\n")
cat("Deployment completed!\n")
cat("If successful, your app should be running at:\n")
cat("https://[your-account].shinyapps.io/aliva-dashboard/\n")
cat("==========================================\n")
