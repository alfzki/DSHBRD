# Uji Asumsi UI Module
# Comprehensive assumption testing interface

#' Uji Asumsi UI Module
#'
#' Creates the user interface for comprehensive statistical assumption testing
#'
#' @param id Module ID for namespacing
#' @return UI elements for the uji_asumsi tab
uji_asumsi_ui <- function(id) {
    ns <- NS(id)
    tabItem(
        tabName = "uji_asumsi",

        # Main content with tabs for different assumption tests
        fluidRow(
            column(
                width = 12,
                tabsetPanel(
                    id = ns("assumption_tabs"),
                    type = "tabs",

                    # Tab 1: Normality Tests
                    tabPanel(
                        title = tags$span(icon("bell"), "Uji Normalitas"),
                        value = "normality",
                        br(),
                        fluidRow(
                            column(
                                width = 3,
                                box(
                                    title = tags$span(icon("cogs"), "Pengaturan Uji Normalitas"),
                                    status = "primary", solidHeader = TRUE, width = NULL,
                                    selectInput(ns("var_normal"), "Pilih Variabel:",
                                        choices = NULL, width = "100%"
                                    ),
                                    hr(),
                                    h6("Jenis Uji Normalitas:"),
                                    checkboxGroupInput(ns("normality_tests"), "Pilih Uji:",
                                        choices = c(
                                            "Shapiro-Wilk Test" = "shapiro",
                                            "Anderson-Darling Test" = "anderson",
                                            "Kolmogorov-Smirnov Test" = "ks",
                                            "Jarque-Bera Test" = "jarque"
                                        ),
                                        selected = c("shapiro", "ks")
                                    ),
                                    hr(),
                                    numericInput(ns("alpha_normal"), "Alpha Level:",
                                        value = 0.05, min = 0.01, max = 0.1, step = 0.01, width = "100%"
                                    ),
                                    checkboxInput(ns("sample_large"), "Sampling jika N > 5000", value = TRUE),
                                    hr(),
                                    actionButton(ns("run_normality"), "Jalankan Uji",
                                        class = "btn-success", icon = icon("play"), width = "100%"
                                    )
                                )
                            ),
                            column(
                                width = 9,
                                box(
                                    title = tags$span(icon("table"), "Hasil Uji Normalitas"),
                                    status = "success", solidHeader = TRUE, width = NULL,
                                    DT::DTOutput(ns("normality_results_table"))
                                ),
                                box(
                                    title = tags$span(icon("chart-line"), "Visualisasi Normalitas"),
                                    status = "info", solidHeader = TRUE, width = NULL,
                                    plotlyOutput(ns("normality_plots"), height = "400px")
                                ),
                                box(
                                    title = tags$span(icon("comment"), "Interpretasi"),
                                    status = "warning", solidHeader = TRUE, width = NULL,
                                    uiOutput(ns("normality_interpretation"))
                                )
                            )
                        )
                    ),

                    # Tab 2: Homogeneity of Variance Tests
                    tabPanel(
                        title = tags$span(icon("balance-scale"), "Uji Homogenitas Varians"),
                        value = "homogeneity",
                        br(),
                        fluidRow(
                            column(
                                width = 3,
                                box(
                                    title = tags$span(icon("cogs"), "Pengaturan Uji Homogenitas"),
                                    status = "primary", solidHeader = TRUE, width = NULL,
                                    selectInput(ns("var_homogen"), "Variabel Dependen:",
                                        choices = NULL, width = "100%"
                                    ),
                                    selectInput(ns("group_homogen"), "Variabel Pengelompokan:",
                                        choices = NULL, width = "100%"
                                    ),
                                    hr(),
                                    h6("Jenis Uji Homogenitas:"),
                                    checkboxGroupInput(ns("homogeneity_tests"), "Pilih Uji:",
                                        choices = c(
                                            "Levene's Test" = "levene",
                                            "Bartlett's Test" = "bartlett",
                                            "Fligner-Killeen Test" = "fligner",
                                            "Brown-Forsythe Test" = "brown"
                                        ),
                                        selected = c("levene", "bartlett")
                                    ),
                                    hr(),
                                    numericInput(ns("alpha_homogen"), "Alpha Level:",
                                        value = 0.05, min = 0.01, max = 0.1, step = 0.01, width = "100%"
                                    ),
                                    actionButton(ns("run_homogeneity"), "Jalankan Uji",
                                        class = "btn-info", icon = icon("play"), width = "100%"
                                    )
                                )
                            ),
                            column(
                                width = 9,
                                box(
                                    title = tags$span(icon("table"), "Hasil Uji Homogenitas"),
                                    status = "info", solidHeader = TRUE, width = NULL,
                                    DT::DTOutput(ns("homogeneity_results_table"))
                                ),
                                box(
                                    title = tags$span(icon("chart-bar"), "Visualisasi Varians per Grup"),
                                    status = "success", solidHeader = TRUE, width = NULL,
                                    plotlyOutput(ns("homogeneity_plots"), height = "400px")
                                ),
                                box(
                                    title = tags$span(icon("comment"), "Interpretasi"),
                                    status = "warning", solidHeader = TRUE, width = NULL,
                                    uiOutput(ns("homogeneity_interpretation"))
                                )
                            )
                        )
                    ),

                    # Tab 3: Independence Tests
                    tabPanel(
                        title = tags$span(icon("link"), "Uji Independensi"),
                        value = "independence",
                        br(),
                        fluidRow(
                            column(
                                width = 3,
                                box(
                                    title = tags$span(icon("cogs"), "Pengaturan Uji Independensi"),
                                    status = "primary", solidHeader = TRUE, width = NULL,
                                    selectInput(ns("var_indep1"), "Variabel 1:",
                                        choices = NULL, width = "100%"
                                    ),
                                    selectInput(ns("var_indep2"), "Variabel 2:",
                                        choices = NULL, width = "100%"
                                    ),
                                    hr(),
                                    h6("Jenis Uji:"),
                                    radioButtons(ns("independence_test"), "Pilih Uji:",
                                        choices = c(
                                            "Chi-square Test" = "chisq",
                                            "Fisher's Exact Test" = "fisher",
                                            "G-test (Log-likelihood)" = "gtest"
                                        ),
                                        selected = "chisq"
                                    ),
                                    numericInput(ns("alpha_indep"), "Alpha Level:",
                                        value = 0.05, min = 0.01, max = 0.1, step = 0.01, width = "100%"
                                    ),
                                    actionButton(ns("run_independence"), "Jalankan Uji",
                                        class = "btn-warning", icon = icon("play"), width = "100%"
                                    )
                                )
                            ),
                            column(
                                width = 9,
                                box(
                                    title = tags$span(icon("table"), "Tabel Kontingensi"),
                                    status = "warning", solidHeader = TRUE, width = NULL,
                                    DT::DTOutput(ns("contingency_table"))
                                ),
                                box(
                                    title = tags$span(icon("chart-pie"), "Visualisasi Kontingensi"),
                                    status = "info", solidHeader = TRUE, width = NULL,
                                    plotlyOutput(ns("independence_plots"), height = "350px")
                                ),
                                box(
                                    title = tags$span(icon("calculator"), "Hasil Uji & Interpretasi"),
                                    status = "success", solidHeader = TRUE, width = NULL,
                                    verbatimTextOutput(ns("independence_results")),
                                    hr(),
                                    uiOutput(ns("independence_interpretation"))
                                )
                            )
                        )
                    ),

                    # Tab 4: Comprehensive Summary
                    tabPanel(
                        title = tags$span(icon("clipboard-check"), "Ringkasan Asumsi"),
                        value = "summary",
                        br(),
                        fluidRow(
                            column(
                                width = 12,
                                box(
                                    title = tags$span(icon("clipboard-list"), "Status Asumsi Statistik"),
                                    status = "success", solidHeader = TRUE, width = NULL,
                                    DT::DTOutput(ns("assumptions_summary_table"))
                                ),
                                box(
                                    title = tags$span(icon("lightbulb"), "Rekomendasi Analisis"),
                                    status = "info", solidHeader = TRUE, width = NULL,
                                    uiOutput(ns("analysis_recommendations"))
                                )
                            )
                        )
                    )
                )
            )
        ),

        # Download section
        fluidRow(
            column(
                width = 12,
                box(
                    title = tags$span(icon("download"), "Unduh Hasil Uji Asumsi"),
                    status = "warning", solidHeader = TRUE, width = NULL,
                    collapsible = TRUE, collapsed = FALSE,
                    fluidRow(
                        column(
                            width = 2,
                            h6("Plots"),
                            downloadButton(ns("download_plots_png"), "PNG",
                                class = "btn-outline-success", icon = icon("image"), width = "100%"
                            ),
                            br(), br(),
                            downloadButton(ns("download_plots_jpg"), "JPEG",
                                class = "btn-outline-info", icon = icon("image"), width = "100%"
                            )
                        ),
                        column(
                            width = 2,
                            h6("Results"),
                            downloadButton(ns("download_results_csv"), "CSV",
                                class = "btn-outline-primary", icon = icon("file-csv"), width = "100%"
                            )
                        ),
                        column(
                            width = 2,
                            h6("Interpretasi"),
                            downloadButton(ns("download_interpretation"), "DOCX",
                                class = "btn-outline-info", icon = icon("file-word"), width = "100%"
                            )
                        ),
                        column(
                            width = 3,
                            h6("Laporan PDF"),
                            downloadButton(ns("download_report_pdf"), "Assumptions PDF",
                                class = "btn-primary", icon = icon("file-pdf"), width = "100%"
                            )
                        ),
                        column(
                            width = 3,
                            h6("Laporan Word"),
                            downloadButton(ns("download_report_word"), "Assumptions Word",
                                class = "btn-success", icon = icon("file-word"), width = "100%"
                            )
                        )
                    )
                )
            )
        )
    )
}
