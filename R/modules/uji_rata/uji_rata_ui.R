# Uji Rata UI Module
# User interface for mean difference testing

#' Uji Rata UI Module
#'
#' Creates the user interface for mean difference tests
#'
#' @param id Module ID for namespacing
#' @return UI elements for the uji_rata tab
uji_rata_ui <- function(id) {
    ns <- NS(id)
    tabItem(
        tabName = "uji_rata",
        fluidRow(
            column(
                width = 3,
                box(
                    title = tags$span(icon("calculator"), "Pengaturan Uji"),
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    radioButtons(ns("test_type"), "Jenis Uji:",
                        choices = c(
                            "Satu Sampel" = "one_sample",
                            "Dua Sampel" = "two_sample"
                        ),
                        selected = "one_sample"
                    ),
                    conditionalPanel(
                        condition = "input.test_type == 'one_sample'",
                        ns = ns,
                        selectInput(ns("var_one"), "Pilih Variabel:",
                            choices = NULL, width = "100%"
                        ),
                        numericInput(ns("mu_test"), "Nilai μ₀:", value = 0, width = "100%")
                    ),
                    conditionalPanel(
                        condition = "input.test_type == 'two_sample'",
                        ns = ns,
                        selectInput(ns("var_two"), "Variabel Dependen:",
                            choices = NULL, width = "100%"
                        ),
                        selectInput(ns("group_two"), "Variabel Grup:",
                            choices = NULL, width = "100%"
                        )
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
                    ),
                    hr(),
                    h5("Download Options:"),
                    downloadButton(ns("download_interpretation"), "Interpretasi (.docx)",
                        class = "btn-sm btn-outline-info", icon = icon("file-word"), width = "100%"
                    ),
                    br(), br(),
                    downloadButton(ns("download_report_pdf"), "Laporan Lengkap (PDF)",
                        class = "btn-primary", icon = icon("file-pdf"), width = "100%"
                    ),
                    br(), br(),
                    downloadButton(ns("download_report_word"), "Laporan Lengkap (Word)",
                        class = "btn-success", icon = icon("file-word"), width = "100%"
                    )
                )
            ),
            column(
                width = 9,
                box(
                    title = tags$span(icon("chart-line"), "Hasil Uji"),
                    status = "success",
                    solidHeader = TRUE,
                    width = NULL,
                    verbatimTextOutput(ns("test_results")),
                    br(),
                    h4("Interpretasi:"),
                    uiOutput(ns("interpretation"))
                )
            )
        )
    )
}
