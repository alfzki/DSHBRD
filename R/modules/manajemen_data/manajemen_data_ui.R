# Manajemen Data UI Module
# User interface for data management functionality

#' Manajemen Data UI Module
#'
#' Creates the user interface for data management and categorization features
#'
#' @param id Module ID for namespacing
#' @return UI elements for the manajemen_data tab
manajemen_data_ui <- function(id) {
    ns <- NS(id)
    tabItem(
        tabName = "manajemen_data",
        fluidRow(
            column(
                width = 4,
                box(
                    title = tags$span(icon("cogs"), "Pengaturan Kategorisasi"),
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    selectInput(ns("select_var"), "Pilih Variabel Kontinu:",
                        choices = NULL, width = "100%"
                    ),
                    numericInput(ns("num_kategori"), "Jumlah Kategori:",
                        value = 3, min = 2, max = 10, width = "100%"
                    ),
                    radioButtons(ns("method"), "Metode Kategorisasi:",
                        choices = c(
                            "Interval Sama" = "interval",
                            "Kuantil (Jumlah Sama)" = "quantile"
                        ),
                        selected = "interval"
                    ),
                    actionButton(ns("process_btn"), "Proses Kategorisasi",
                        class = "btn-success", icon = icon("play"),
                        width = "100%"
                    )
                )
            ),
            column(
                width = 8,
                box(
                    title = tags$span(icon("table"), "Hasil Kategorisasi"),
                    status = "success",
                    solidHeader = TRUE,
                    width = NULL,
                    DT::DTOutput(ns("result_table"))
                )
            )
        ),
        fluidRow(
            column(
                width = 12,
                box(
                    title = tags$span(icon("comment"), "Interpretasi"),
                    status = "info",
                    solidHeader = TRUE,
                    width = NULL,
                    verbatimTextOutput(ns("interpretation"))
                )
            )
        ),
        fluidRow(
            column(
                width = 6,
                downloadButton(ns("download_result"), "Unduh Hasil (.csv)",
                    class = "btn-primary", icon = icon("download")
                )
            ),
            column(
                width = 6,
                downloadButton(ns("download_interpretation"), "Unduh Interpretasi (.txt)",
                    class = "btn-info", icon = icon("file-text")
                )
            )
        )
    )
}
