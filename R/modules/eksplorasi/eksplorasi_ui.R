# Eksplorasi Data UI Module
# User interface for data exploration functionality

#' Eksplorasi Data UI Module
#'
#' Creates the user interface for data exploration and visualization features
#'
#' @param id Module ID for namespacing
#' @return UI elements for the eksplorasi tab
eksplorasi_ui <- function(id) {
    ns <- NS(id)
    tabItem(
        tabName = "eksplorasi",
        fluidRow(
            column(
                width = 3,
                box(
                    title = tags$span(icon("sliders-h"), "Pengaturan"),
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    selectInput(ns("select_var"), "Pilih Variabel:",
                        choices = NULL, width = "100%"
                    ),
                    hr(),
                    h5("Unduhan Individual:"),
                    downloadButton(ns("download_summary"), "Unduh Ringkasan (.pdf)",
                        class = "btn-sm btn-outline-primary", icon = icon("file-pdf")
                    ),
                    br(), br(),
                    downloadButton(ns("download_plot"), "Unduh Plot (.png)",
                        class = "btn-sm btn-outline-success", icon = icon("image")
                    ),
                    br(), br(),
                    downloadButton(ns("download_plot_jpg"), "Unduh Plot (.jpg)",
                        class = "btn-sm btn-outline-success", icon = icon("image")
                    ),
                    br(), br(),
                    downloadButton(ns("download_interpretation"), "Unduh Interpretasi (.docx)",
                        class = "btn-sm btn-outline-info", icon = icon("file-word")
                    ),
                    hr(),
                    h5("Laporan Lengkap:"),
                    downloadButton(ns("download_report_pdf"), "Unduh Laporan (PDF)",
                        class = "btn-primary", icon = icon("file-pdf"), width = "100%"
                    ),
                    br(), br(),
                    downloadButton(ns("download_report_word"), "Unduh Laporan (Word)",
                        class = "btn-success", icon = icon("file-word"), width = "100%"
                    )
                )
            ),
            column(
                width = 9,
                tabsetPanel(
                    tabPanel(
                        "Statistik Deskriptif",
                        icon = icon("list-alt"),
                        br(),
                        verbatimTextOutput(ns("summary_stats")),
                        br(),
                        uiOutput(ns("interpretation_stats"))
                    ),
                    tabPanel(
                        "Visualisasi Grafik",
                        icon = icon("chart-bar"),
                        br(),
                        plotly::plotlyOutput(ns("plot_viz"), height = "500px")
                    ),
                    tabPanel(
                        "Visualisasi Peta",
                        icon = icon("map"),
                        br(),
                        div(
                            style = "background-color: #e8f4fd; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                            h5(icon("info-circle"), "Informasi Peta Interaktif"),
                            p("Peta ini menampilkan distribusi spasial variabel yang dipilih across kabupaten/kota di Indonesia."),
                            p("Fitur:", tags$span(style = "font-weight: bold;", "• Hover untuk detail • Klik untuk informasi • Legend warna otomatis"))
                        ),
                        leaflet::leafletOutput(ns("map_viz"), height = "500px")
                    )
                )
            )
        )
    )
}
