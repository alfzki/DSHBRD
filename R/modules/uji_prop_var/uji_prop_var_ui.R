# Uji Proporsi & Varians UI Module
# User interface for proportion and variance testing

#' Uji Proporsi & Varians UI Module
#'
#' Creates the user interface for proportion and variance tests
#'
#' @param id Module ID for namespacing
#' @return UI elements for the uji_prop_var tab
uji_prop_var_ui <- function(id) {
    ns <- NS(id)
    tabItem(
        tabName = "uji_prop_var",
        fluidRow(
            column(
                width = 3,
                box(
                    title = tags$span(icon("percentage"), "Pengaturan Uji"),
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    radioButtons(ns("test_choice"), "Pilih Jenis Uji:",
                        choices = c(
                            "Uji Proporsi" = "proportion",
                            "Uji Varians" = "variance"
                        ),
                        selected = "proportion"
                    ),
                    conditionalPanel(
                        condition = "input.test_choice == 'proportion'",
                        ns = ns,
                        h5("Uji Proporsi:"),
                        numericInput(ns("x_prop"), "Jumlah Sukses:", value = 50, min = 0),
                        numericInput(ns("n_prop"), "Jumlah Total:", value = 100, min = 1),
                        numericInput(ns("p_test"), "Proporsi Uji (p₀):",
                            value = 0.5,
                            min = 0, max = 1, step = 0.01
                        )
                    ),
                    conditionalPanel(
                        condition = "input.test_choice == 'variance'",
                        ns = ns,
                        h5("Uji Varians:"),
                        selectInput(ns("var_variance"), "Pilih Variabel:",
                            choices = NULL, width = "100%"
                        ),
                        numericInput(ns("var_test"), "Varians Uji (σ₀²):", value = 1, min = 0)
                    ),
                    radioButtons(ns("alternative"), "Hipotesis Alternatif:",
                        choices = c(
                            "Dua arah" = "two.sided",
                            "Lebih besar" = "greater",
                            "Lebih kecil" = "less"
                        ),
                        selected = "two.sided"
                    ),
                    actionButton(ns("run_test"), "Jalankan Uji",
                        class = "btn-success", icon = icon("play"), width = "100%"
                    )
                )
            ),
            column(
                width = 9,
                box(
                    title = tags$span(icon("chart-pie"), "Hasil Uji"),
                    status = "success",
                    solidHeader = TRUE,
                    width = NULL,
                    verbatimTextOutput(ns("test_results")),
                    br(),
                    h4("Interpretasi:"),
                    uiOutput(ns("interpretation")),
                    br(),
                    box(
                        title = tags$span(icon("download"), "Unduh Laporan"),
                        status = "info",
                        solidHeader = TRUE,
                        width = NULL,
                        collapsible = TRUE,
                        collapsed = TRUE,
                        p(class = "text-muted", style = "font-size: 0.9em;", 
                          "Masukkan parameter uji dan klik 'Lakukan Uji' untuk mengaktifkan tombol unduh."),
                        fluidRow(
                            column(
                                width = 4,
                                h6("Interpretasi"),
                                downloadButton(ns("download_interpretation"), "Unduh (.docx)",
                                    class = "btn-outline-info btn-sm", icon = icon("file-word"), width = "100%"
                                )
                            ),
                            column(
                                width = 4,
                                h6("Laporan PDF"),
                                downloadButton(ns("download_report_pdf"), "Unduh PDF",
                                    class = "btn-primary btn-sm", icon = icon("file-pdf"), width = "100%"
                                )
                            ),
                            column(
                                width = 4,
                                h6("Laporan Word"),
                                downloadButton(ns("download_report_word"), "Unduh Word",
                                    class = "btn-success btn-sm", icon = icon("file-word"), width = "100%"
                                )
                            )
                        )
                    )
                )
            )
        )
    )
}
