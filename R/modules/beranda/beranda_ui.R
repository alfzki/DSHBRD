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
                    downloadButton(ns("download_info"), "Unduh Info ALIVA Dashboard (.pdf)",
                        class = "btn-primary", icon = icon("file-pdf")
                    )
                )
            )
        )
    )
}
