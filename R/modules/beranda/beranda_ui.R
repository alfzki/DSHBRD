# ==============================================================================
# MODUL UI BERANDA
# ==============================================================================
#
# Tujuan: Antarmuka pengguna untuk halaman beranda/landing dashboard
# Penulis: Tim Dashboard ALIVA
# Terakhir Diperbarui: Juli 2025
#
# Deskripsi:
# Modul ini menyediakan antarmuka utama halaman beranda yang mencakup
# sambutan, informasi dataset, metadata variabel, dan navigasi awal.
#
# Komponen UI:
# - Kotak sambutan dengan panduan navigasi
# - Informasi dataset dan statistik
# - Tabel metadata variabel yang komprehensif
# - Akses unduhan informasi dashboard
# ==============================================================================

#' Modul UI Beranda
#'
#' @description Membuat antarmuka pengguna untuk halaman beranda/landing dashboard
#' @param id Character. ID modul untuk namespacing
#' @return Elemen UI untuk tab beranda
#' @author Tim Dashboard ALIVA
beranda_ui <- function(id) {
    ns <- NS(id)
    tabItem(
        tabName = "beranda",
        fluidRow(
            column(
                width = 12,
                box(
                    title = tags$span(icon("home"), "Selamat Datang di ALIVA: Alif's Vulnerability Analytics Dashboard"),
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    uiOutput(ns("welcome_content"))
                )
            )
        ),
        fluidRow(
            column(
                width = 6,
                box(
                    title = tags$span(icon("info-circle"), "Tentang Dataset"),
                    status = "info",
                    solidHeader = TRUE,
                    width = NULL,
                    uiOutput(ns("dataset_info"))
                )
            ),
            column(
                width = 6,
                box(
                    title = tags$span(icon("table"), "Metadata Variabel"),
                    status = "success",
                    solidHeader = TRUE,
                    width = NULL,
                    DT::DTOutput(ns("metadata_table"))
                )
            )
        ),
        fluidRow(
            column(
                width = 12,
                box(
                    title = tags$span(icon("download"), "Unduhan"),
                    status = "warning",
                    solidHeader = TRUE,
                    width = NULL,
                    p("Unduh informasi lengkap tentang ALIVA Dashboard ini:"),
                    fluidRow(
                        column(
                            width = 6,
                            h5("Unduhan Individual:"),
                            downloadButton(ns("download_info"), "Info Dashboard (.pdf)",
                                class = "btn-primary", icon = icon("file-pdf"), width = "100%"
                            )
                        ),
                        column(
                            width = 6,
                            h5("Unduhan Gabungan Semua Menu:"),
                            p(
                                class = "text-info", style = "font-size: 0.9em;",
                                icon("info-circle"), " Kombinasi laporan dari semua menu dashboard"
                            ),
                            downloadButton(ns("download_combined_pdf"), "Laporan Gabungan (PDF)",
                                class = "btn-success", icon = icon("file-pdf"), width = "100%"
                            ),
                            br(), br(),
                            downloadButton(ns("download_combined_word"), "Laporan Gabungan (Word)",
                                class = "btn-info", icon = icon("file-word"), width = "100%"
                            )
                        )
                    )
                )
            )
        )
    )
}
