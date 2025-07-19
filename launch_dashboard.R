# ==============================================================================
# LAUNCHER DASHBOARD ALIVA
# ==============================================================================
#
# Tujuan: Script peluncuran yang memastikan semua dependensi terinstal
# Penulis: Tim Dashboard ALIVA
# Terakhir Diperbarui: Juli 2025
#
# Deskripsi:
# Script ini memastikan semua paket yang diperlukan telah terinstal,
# memeriksa keberadaan file data, dan meluncurkan dashboard ALIVA
# dengan konfigurasi yang tepat.
#
# Fitur:
# - Verifikasi versi R dan dependensi
# - Pemeriksaan file data yang diperlukan
# - Mode development dengan autoreload
# - Penanganan kesalahan komprehensif
# ==============================================================================

cat("Launcher Dashboard ALIVA\n")
cat("===========================\n\n")

# Penetapan mirror CRAN
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Memeriksa versi R
cat("R Version:", R.version.string, "\n")
cat("Working Directory:", getwd(), "\n\n")

# Paket inti yang harus tersedia
core_packages <- c("shiny", "dplyr", "ggplot2")

cat("Memeriksa paket inti...\n")
for (pkg in core_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        cat("Menginstal paket inti:", pkg, "\n")
        install.packages(pkg, repos = "https://cran.rstudio.com/")
    } else {
        cat("✓", pkg, "tersedia\n")
    }
}

# Memeriksa keberadaan file data
cat("\nMemeriksa file data...\n")
if (file.exists("data/sovi_data.csv")) {
    cat("✓ File data SOVI ditemukan\n")
} else {
    cat("⚠ File data SOVI tidak ditemukan. Harap sediakan data yang diperlukan di 'data/sovi_data.csv'.\n")
}

if (file.exists("data/distance.csv")) {
    cat("✓ File data jarak ditemukan\n")
} else {
    cat("⚠ File data jarak tidak ditemukan. Harap sediakan data yang diperlukan di 'data/distance.csv'.\n")
}

# Memuat konfigurasi global
cat("\nMemuat konfigurasi global...\n")
if (file.exists("global.R")) {
    tryCatch(
        {
            source("global.R")
            cat("✓ Konfigurasi global berhasil dimuat\n")
        },
        error = function(e) {
            cat("✗ Kesalahan memuat konfigurasi global:", e$message, "\n")
            cat("Mencoba meluncurkan dengan konfigurasi minimal...\n")
            library(shiny)
        }
    )
} else {
    cat("✗ global.R tidak ditemukan\n")
    stop("File konfigurasi global diperlukan")
}

# Memeriksa argumen mode development
args <- commandArgs(trailingOnly = TRUE)
is_dev_mode <- "--dev" %in% args

if (is_dev_mode) {
    cat("\n** MODE PENGEMBANGAN DIAKTIFKAN **\n")
    cat("** Autoreload Shiny MENYALA.     **\n\n")
    options(shiny.autoreload = TRUE)
}

# Meluncurkan dashboard
cat("Meluncurkan Dashboard ALIVA...\n")
if (!is_dev_mode) {
    cat("Dashboard akan tersedia di: http://127.0.0.1:3838\n")
}
cat("Tekan Ctrl+C untuk menghentikan dashboard\n\n")

# Memeriksa keberadaan app.R
if (!file.exists("app.R")) {
    stop("app.R tidak ditemukan di direktori saat ini")
}

# Peluncuran dengan penanganan kesalahan
tryCatch(
    {
        shiny::runApp("app.R",
            port = 3838,
            host = "127.0.0.1",
            launch.browser = FALSE
        )
    },
    error = function(e) {
        cat("Kesalahan meluncurkan dashboard:", e$message, "\n")
        cat("Silakan periksa pesan kesalahan di atas dan coba lagi.\n")
    }
)
