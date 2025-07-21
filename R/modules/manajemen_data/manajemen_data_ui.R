# Manajemen Data UI Module
# Comprehensive data management interface

#' Manajemen Data UI Module
#'
#' Creates the user interface for comprehensive data management features
#'
#' @param id Module ID for namespacing
#' @return UI elements for the manajemen_data tab
manajemen_data_ui <- function(id) {
    ns <- NS(id)
    tabItem(
        tabName = "manajemen_data",
        # Tab Panel for different data management functions
        fluidRow(
            column(
                width = 12,
                tabsetPanel(
                    id = ns("data_management_tabs"),
                    type = "tabs",

                    # Tab 1: Variable Transformations
                    tabPanel(
                        title = tags$span(icon("exchange"), "Transformasi Variabel"),
                        value = "transformations",
                        br(),
                        fluidRow(
                            column(
                                width = 4,
                                box(
                                    title = tags$span(icon("calculator"), "Pengaturan Transformasi"),
                                    status = "primary", solidHeader = TRUE, width = NULL,
                                    selectInput(ns("transform_var"), "Pilih Variabel:",
                                        choices = NULL, width = "100%"
                                    ),
                                    selectInput(ns("transform_method"), "Metode Transformasi:",
                                        choices = c(
                                            "Log Natural (ln)" = "log",
                                            "Log Base 10" = "log10",
                                            "Akar Kuadrat" = "sqrt",
                                            "Z-Score (Standardisasi)" = "zscore",
                                            "Min-Max (0-1)" = "minmax",
                                            "Kuadrat" = "square",
                                            "Invers" = "inverse"
                                        ),
                                        selected = "zscore", width = "100%"
                                    ),
                                    conditionalPanel(
                                        condition = "input.transform_method == 'minmax'",
                                        ns = ns,
                                        numericInput(ns("minmax_min"), "Nilai Minimum:", value = 0, width = "100%"),
                                        numericInput(ns("minmax_max"), "Nilai Maksimum:", value = 1, width = "100%")
                                    ),
                                    hr(),
                                    textInput(ns("transform_var_name"), "Nama Variabel Baru:",
                                        placeholder = "contoh: var_log", width = "100%"
                                    ),
                                    actionButton(ns("apply_transform"), "Terapkan Transformasi",
                                        class = "btn-success", icon = icon("play"), width = "100%"
                                    )
                                ),
                                box(
                                    title = tags$span(icon("chart-line"), "Visualisasi Before/After"),
                                    status = "info", solidHeader = TRUE, width = NULL,
                                    plotlyOutput(ns("transform_comparison"), height = "300px")
                                )
                            ),
                            column(
                                width = 8,
                                box(
                                    title = tags$span(icon("table"), "Hasil Transformasi"),
                                    status = "success", solidHeader = TRUE, width = NULL,
                                    DT::DTOutput(ns("transform_result_table"))
                                ),
                                box(
                                    title = tags$span(icon("info"), "Interpretasi Transformasi"),
                                    status = "info", solidHeader = TRUE, width = NULL,
                                    verbatimTextOutput(ns("transform_interpretation"))
                                )
                            )
                        )
                    ),

                    # Tab 2: Variable Categorization
                    tabPanel(
                        title = tags$span(icon("tags"), "Kategorisasi"),
                        value = "categorization",
                        br(),
                        fluidRow(
                            column(
                                width = 4,
                                box(
                                    title = tags$span(icon("cogs"), "Pengaturan Kategorisasi"),
                                    status = "primary", solidHeader = TRUE, width = NULL,
                                    selectInput(ns("select_var"), "Pilih Variabel:",
                                        choices = NULL, width = "100%"
                                    ),
                                    numericInput(ns("num_kategori"), "Jumlah Kategori:",
                                        value = 3, min = 2, max = 10, width = "100%"
                                    ),
                                    radioButtons(ns("method"), "Metode Kategorisasi:",
                                        choices = c(
                                            "Interval Sama" = "interval",
                                            "Kuantil (Jumlah Sama)" = "quantile",
                                            "Custom Breakpoints" = "custom"
                                        ),
                                        selected = "quantile"
                                    ),
                                    conditionalPanel(
                                        condition = "input.method == 'custom'",
                                        ns = ns,
                                        textInput(ns("custom_breaks"), "Breakpoints (pisahkan dengan koma):",
                                            placeholder = "contoh: 10,20,50,100", width = "100%"
                                        )
                                    ),
                                    hr(),
                                    textInput(ns("new_var_name"), "Nama Variabel Baru:",
                                        placeholder = "contoh: var_kategori", width = "100%"
                                    ),
                                    actionButton(ns("process_btn"), "Proses Kategorisasi",
                                        class = "btn-success", icon = icon("play"), width = "100%"
                                    )
                                )
                            ),
                            column(
                                width = 8,
                                box(
                                    title = tags$span(icon("table"), "Hasil Kategorisasi"),
                                    status = "success", solidHeader = TRUE, width = NULL,
                                    DT::DTOutput(ns("result_table"))
                                ),
                                box(
                                    title = tags$span(icon("chart-bar"), "Distribusi Kategori"),
                                    status = "info", solidHeader = TRUE, width = NULL,
                                    plotlyOutput(ns("category_plot"), height = "300px")
                                )
                            )
                        )
                    ),

                    # Tab 3: Derived Variables
                    tabPanel(
                        title = tags$span(icon("plus-circle"), "Variabel Turunan"),
                        value = "derived",
                        br(),
                        fluidRow(
                            column(
                                width = 4,
                                box(
                                    title = tags$span(icon("calculator"), "Buat Variabel Turunan"),
                                    status = "primary", solidHeader = TRUE, width = NULL,
                                    textInput(ns("derived_var_name"), "Nama Variabel Baru:",
                                        placeholder = "contoh: vulnerability_index", width = "100%"
                                    ),
                                    selectInput(ns("derived_operation"), "Operasi:",
                                        choices = c(
                                            "Penjumlahan (A + B)" = "sum",
                                            "Pengurangan (A - B)" = "subtract",
                                            "Perkalian (A * B)" = "multiply",
                                            "Pembagian (A / B)" = "divide",
                                            "Rata-rata (A + B)/2" = "mean",
                                            "Formula Custom" = "formula"
                                        ),
                                        selected = "sum", width = "100%"
                                    ),
                                    conditionalPanel(
                                        condition = "input.derived_operation != 'formula'",
                                        ns = ns,
                                        selectInput(ns("derived_var1"), "Variabel A:",
                                            choices = NULL, width = "100%"
                                        ),
                                        selectInput(ns("derived_var2"), "Variabel B:",
                                            choices = NULL, width = "100%"
                                        )
                                    ),
                                    conditionalPanel(
                                        condition = "input.derived_operation == 'formula'",
                                        ns = ns,
                                        textAreaInput(ns("custom_formula"), "Formula Custom:",
                                            placeholder = "Contoh: POVERTY + ILLITERATE - TAPWATER",
                                            width = "100%", height = "80px"
                                        ),
                                        helpText("Gunakan nama variabel persis seperti di dataset.")
                                    ),
                                    actionButton(ns("create_derived"), "Buat Variabel",
                                        class = "btn-info", icon = icon("plus"), width = "100%"
                                    )
                                )
                            ),
                            column(
                                width = 8,
                                box(
                                    title = tags$span(icon("table"), "Variabel yang Tersedia"),
                                    status = "info", solidHeader = TRUE, width = NULL, height = "200px",
                                    DT::DTOutput(ns("available_vars_table"))
                                ),
                                box(
                                    title = tags$span(icon("chart-area"), "Preview Variabel Baru"),
                                    status = "success", solidHeader = TRUE, width = NULL,
                                    plotlyOutput(ns("derived_preview"), height = "250px")
                                )
                            )
                        )
                    ),

                    # Tab 4: Data Filtering
                    tabPanel(
                        title = tags$span(icon("filter"), "Filter Data"),
                        value = "filtering",
                        br(),
                        fluidRow(
                            column(
                                width = 4,
                                box(
                                    title = tags$span(icon("sliders-h"), "Filter Numerik"),
                                    status = "primary", solidHeader = TRUE, width = NULL,
                                    uiOutput(ns("numeric_filters"))
                                ),
                                box(
                                    title = tags$span(icon("list"), "Filter Kategorik"),
                                    status = "warning", solidHeader = TRUE, width = NULL,
                                    uiOutput(ns("categorical_filters"))
                                ),
                                box(
                                    title = tags$span(icon("save"), "Aksi"),
                                    status = "success", solidHeader = TRUE, width = NULL,
                                    actionButton(ns("apply_filters"), "Terapkan Filter",
                                        class = "btn-success", icon = icon("filter"), width = "100%"
                                    ),
                                    br(), br(),
                                    actionButton(ns("reset_filters"), "Reset Filter",
                                        class = "btn-warning", icon = icon("refresh"), width = "100%"
                                    )
                                )
                            ),
                            column(
                                width = 8,
                                box(
                                    title = tags$span(icon("table"), "Data Terfilter"),
                                    status = "success", solidHeader = TRUE, width = NULL,
                                    div(id = ns("filter_summary"), style = "margin-bottom: 10px;"),
                                    DT::DTOutput(ns("filtered_data_table"))
                                )
                            )
                        )
                    ),

                    # Tab 5: Missing Data & Summary
                    tabPanel(
                        title = tags$span(icon("question-circle"), "Data Missing & Ringkasan"),
                        value = "summary",
                        br(),
                        fluidRow(
                            column(
                                width = 6,
                                box(
                                    title = tags$span(icon("exclamation-triangle"), "Analisis Data Missing"),
                                    status = "warning", solidHeader = TRUE, width = NULL,
                                    DT::DTOutput(ns("missing_data_table")),
                                    hr(),
                                    h5("Opsi Penanganan Missing Data:"),
                                    selectInput(ns("missing_method"), "Metode:",
                                        choices = c(
                                            "Hapus baris dengan missing values" = "remove",
                                            "Isi dengan mean/mode" = "impute_central",
                                            "Isi dengan nilai tertentu" = "impute_value"
                                        ),
                                        selected = "impute_central", width = "100%"
                                    ),
                                    conditionalPanel(
                                        condition = "input.missing_method == 'impute_value'",
                                        ns = ns,
                                        numericInput(ns("impute_value"), "Nilai untuk mengisi:", value = 0, width = "100%")
                                    ),
                                    actionButton(ns("handle_missing"), "Terapkan Penanganan",
                                        class = "btn-warning", icon = icon("tools"), width = "100%"
                                    )
                                )
                            ),
                            column(
                                width = 6,
                                box(
                                    title = tags$span(icon("chart-bar"), "Statistik Ringkasan"),
                                    status = "info", solidHeader = TRUE, width = NULL,
                                    DT::DTOutput(ns("summary_stats_table"))
                                ),
                                box(
                                    title = tags$span(icon("info-circle"), "Informasi Dataset"),
                                    status = "success", solidHeader = TRUE, width = NULL,
                                    verbatimTextOutput(ns("data_info"))
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
                    title = tags$span(icon("download"), "Unduh Hasil"),
                    status = "success", solidHeader = TRUE, width = NULL,
                    collapsible = TRUE, collapsed = FALSE,
                    p(
                        class = "text-info", style = "font-size: 0.9em; margin-bottom: 15px;",
                        icon("info-circle"), " Unduh hasil transformasi, kategorisasi, atau dataset yang telah difilter."
                    ),
                    fluidRow(
                        column(
                            width = 3,
                            h6("Data CSV"),
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
}
