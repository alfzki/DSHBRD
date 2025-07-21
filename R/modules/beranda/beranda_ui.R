# Beranda UI Module
# Home page user interface for ALIVA: Alif's Vulnerability Analytics Dashboard

#' Beranda UI Module
#'
#' Creates the user interface for the home/landing page of the dashboard
#'
#' @param id Module ID for namespacing
#' @return UI elements for the beranda tab
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
                width = 4,
                box(
                    title = tags$span(icon("info-circle"), "Tentang Dataset"),
                    status = "info",
                    solidHeader = TRUE,
                    width = NULL,
                    uiOutput(ns("dataset_info"))
                )
            ),
            column(
                width = 8,
                box(
                    title = tags$span(icon("map-marker"), "Konteks Geografis Indonesia"),
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    height = "400px",
                    leafletOutput(ns("indonesia_map"), height = "350px")
                )
            )
        ),
        fluidRow(
            column(
                width = 6,
                box(
                    title = tags$span(icon("table"), "Metadata Dataset SOVI"),
                    status = "success",
                    solidHeader = TRUE,
                    width = NULL,
                    DT::DTOutput(ns("sovi_metadata_table"))
                )
            ),
            column(
                width = 6,
                box(
                    title = tags$span(icon("ruler"), "Metadata Dataset Distance"),
                    status = "warning",
                    solidHeader = TRUE,
                    width = NULL,
                    DT::DTOutput(ns("distance_metadata_table"))
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
