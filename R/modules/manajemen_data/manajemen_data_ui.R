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
                    conditionalPanel(
                        condition = "output.show_save_options",
                        ns = ns,
                        hr(),
                        h5("Simpan Sebagai Variabel Baru:"),
                        textInput(ns("new_var_name"), "Nama Variabel Baru:",
                            placeholder = "contoh: income_kategori", width = "100%"
                        ),
                        actionButton(ns("save_variable"), "Simpan ke Dataset",
                            class = "btn-info", icon = icon("save"),
                            width = "100%"
                        ),
                        hr()
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
        conditionalPanel(
            condition = "output.show_downloads",
            ns = ns,
            fluidRow(
                column(
                    width = 12,
                    box(
                        title = tags$span(icon("download"), "Unduh Hasil"),
                        status = "success",
                        solidHeader = TRUE,
                        width = NULL,
                        collapsible = TRUE,
                        collapsed = FALSE,
                        p(class = "text-info", style = "font-size: 0.9em; margin-bottom: 15px;", 
                          icon("info-circle"), " Fitur unduh tersedia setelah kategorisasi berhasil dilakukan."),
                        fluidRow(
                            column(
                                width = 3,
                                h6("Data Hasil"),
                                downloadButton(ns("download_result"), "Unduh Data (.csv)",
                                    class = "btn-outline-primary", icon = icon("file-csv"), width = "100%"
                                )
                            ),
                            column(
                                width = 3,
                                h6("Interpretasi"),
                                downloadButton(ns("download_interpretation"), "Unduh Interpretasi (.docx)",
                                    class = "btn-outline-info", icon = icon("file-word"), width = "100%"
                                )
                            ),
                            column(
                                width = 3,
                                h6("Laporan PDF"),
                                downloadButton(ns("download_report_pdf"), "Unduh Laporan PDF",
                                    class = "btn-primary", icon = icon("file-pdf"), width = "100%"
                                )
                            ),
                            column(
                                width = 3,
                                h6("Laporan Word"),
                                downloadButton(ns("download_report_word"), "Unduh Laporan Word",
                                    class = "btn-success", icon = icon("file-word"), width = "100%"
                                )
                            )
                        )
                    )
                )
            )
        )
    )
}
