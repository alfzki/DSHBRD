# ==============================================================================
# HELPER PEMUATAN SEMUA MODUL
# ==============================================================================
#
# Tujuan: Script helper untuk memuat semua komponen UI dan server modular
# Penulis: Tim Dashboard ALIVA
# Terakhir Diperbarui: Juli 2025
#
# Deskripsi:
# Script ini memuat semua komponen modular yang diperlukan untuk dashboard
# ALIVA. Harus dipanggil setelah global.R dimuat untuk memastikan semua
# dependensi tersedia.
#
# Arsitektur Modular:
# - Memuat file utilitas terlebih dahulu
# - Memuat modul UI dari setiap fitur
# - Memuat modul server dari setiap fitur
# - Pemisahan concern berdasarkan fungsionalitas
# ==============================================================================

#' Memuat Semua Modul UI dan Server untuk Dashboard ALIVA
#'
#' @description Fungsi ini memuat semua komponen modular yang diperlukan untuk
#'              dashboard. Harus dipanggil setelah global.R dimuat untuk memastikan
#'              semua dependensi tersedia.
#' @details Memuat file dari direktori R/modules mengikuti arsitektur modular
#'          baru yang memisahkan concern berdasarkan fungsionalitas.
#' @return Tidak ada nilai kembali, fungsi ini memuat file ke environment global
#' @author Tim Dashboard ALIVA
load_all_modules <- function() {
    # Memuat fungsi utilitas terlebih dahulu
    utils_dir <- file.path("R", "utils")
    if (dir.exists(utils_dir)) {
        utils_files <- list.files(utils_dir, pattern = "\\.R$", full.names = TRUE)
        for (util_file in utils_files) {
            source(util_file)
            cat(paste("✓ Utilitas dimuat:", basename(util_file), "\n"))
        }
    }

    # Mendefinisikan direktori dasar modul
    modules_dir <- file.path("R", "modules")

    # Mendefinisikan semua nama modul
    module_names <- c(
        "beranda",
        "manajemen_data",
        "eksplorasi",
        "uji_asumsi",
        "uji_rata",
        "uji_prop_var",
        "uji_anova",
        "regresi"
    )

    # Memuat modul UI
    cat("Memuat modul UI...\n")
    for (module in module_names) {
        ui_file <- file.path(modules_dir, module, paste0(module, "_ui.R"))
        if (file.exists(ui_file)) {
            source(ui_file)
            cat(paste("✓ UI dimuat:", module, "\n"))
        } else {
            cat(paste("⚠ File UI tidak ditemukan:", ui_file, "\n"))
        }
    }

    # Memuat modul Server
    cat("\nMemuat modul Server...\n")
    for (module in module_names) {
        server_file <- file.path(modules_dir, module, paste0(module, "_server.R"))
        if (file.exists(server_file)) {
            source(server_file)
            cat(paste("✓ Server dimuat:", module, "\n"))
        } else {
            cat(paste("⚠ File Server tidak ditemukan:", server_file, "\n"))
        }
    }

    cat("\n✅ Semua modul berhasil dimuat!\n")
    cat("📊 Arsitektur modular diimplementasikan dengan pemisahan concern.\n")
    cat("🔧 Setiap modul sekarang menangani fungsionalitas spesifik secara independen.\n\n")
}
