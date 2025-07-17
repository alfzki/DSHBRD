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
                    h5("Unduhan:"),
                    downloadButton(ns("download_summary"), "Unduh Ringkasan (.pdf)",
                        class = "btn-sm btn-outline-primary", icon = icon("file-pdf")
                    ),
                    br(), br(),
                    downloadButton(ns("download_plot"), "Unduh Plot (.png)",
                        class = "btn-sm btn-outline-success", icon = icon("image")
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
                        p("Catatan: Fitur peta memerlukan data spasial tambahan."),
                        leaflet::leafletOutput(ns("map_viz"), height = "500px")
                    )
                )
            )
        )
    )
}
