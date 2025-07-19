# Eksplorasi Data UI Module
# Comprehensive data exploration interface

#' Eksplorasi Data UI Module
#'
#' Creates the user interface for comprehensive data exploration features
#'
#' @param id Module ID for namespacing
#' @return UI elements for the eksplorasi tab
eksplorasi_ui <- function(id) {
    ns <- NS(id)
    tabItem(
        tabName = "eksplorasi",

        # Main content with tabs for different exploration types
        fluidRow(
            column(
                width = 12,
                tabsetPanel(
                    id = ns("exploration_tabs"),
                    type = "tabs",

                    # Tab 1: Univariate Analysis
                    tabPanel(
                        title = tags$span(icon("chart-line"), "Analisis Univariat"),
                        value = "univariate",
                        br(),
                        fluidRow(
                            column(
                                width = 3,
                                box(
                                    title = tags$span(icon("sliders-h"), "Pengaturan"),
                                    status = "primary", solidHeader = TRUE, width = NULL,
                                    selectInput(ns("univar_var"), "Pilih Variabel:",
                                        choices = NULL, width = "100%"
                                    ),
                                    radioButtons(ns("univar_plot_type"), "Jenis Visualisasi:",
                                        choices = c(
                                            "Histogram" = "histogram",
                                            "Density Plot" = "density",
                                            "Boxplot" = "boxplot",
                                            "Q-Q Plot" = "qqplot"
                                        ),
                                        selected = "histogram"
                                    ),
                                    conditionalPanel(
                                        condition = "input.univar_plot_type == 'histogram'",
                                        ns = ns,
                                        numericInput(ns("hist_bins"), "Jumlah Bins:",
                                            value = 20, min = 5, max = 50, width = "100%"
                                        )
                                    ),
                                    hr(),
                                    actionButton(ns("generate_univar"), "Generate Plot",
                                        class = "btn-success", icon = icon("play"), width = "100%"
                                    )
                                )
                            ),
                            column(
                                width = 9,
                                box(
                                    title = tags$span(icon("chart-bar"), "Visualisasi"),
                                    status = "success", solidHeader = TRUE, width = NULL,
                                    plotlyOutput(ns("univar_plot"), height = "400px")
                                ),
                                box(
                                    title = tags$span(icon("table"), "Statistik Deskriptif"),
                                    status = "info", solidHeader = TRUE, width = NULL,
                                    verbatimTextOutput(ns("univar_summary"))
                                )
                            )
                        )
                    ),

                    # Tab 2: Bivariate Analysis
                    tabPanel(
                        title = tags$span(icon("sitemap"), "Analisis Bivariat"),
                        value = "bivariate",
                        br(),
                        fluidRow(
                            column(
                                width = 3,
                                box(
                                    title = tags$span(icon("cogs"), "Pengaturan"),
                                    status = "primary", solidHeader = TRUE, width = NULL,
                                    selectInput(ns("bivar_x"), "Variabel X (Horizontal):",
                                        choices = NULL, width = "100%"
                                    ),
                                    selectInput(ns("bivar_y"), "Variabel Y (Vertikal):",
                                        choices = NULL, width = "100%"
                                    ),
                                    radioButtons(ns("bivar_plot_type"), "Jenis Plot:",
                                        choices = c(
                                            "Scatter Plot" = "scatter",
                                            "Box Plot (X categorical)" = "boxplot",
                                            "Bar Chart (X categorical)" = "bar",
                                            "Line Plot" = "line"
                                        ),
                                        selected = "scatter"
                                    ),
                                    checkboxInput(ns("add_smooth"), "Tambah Trend Line", value = TRUE),
                                    hr(),
                                    actionButton(ns("generate_bivar"), "Generate Plot",
                                        class = "btn-info", icon = icon("play"), width = "100%"
                                    )
                                )
                            ),
                            column(
                                width = 9,
                                box(
                                    title = tags$span(icon("chart-area"), "Plot Bivariat"),
                                    status = "info", solidHeader = TRUE, width = NULL,
                                    plotlyOutput(ns("bivar_plot"), height = "400px")
                                ),
                                box(
                                    title = tags$span(icon("calculator"), "Statistik Korelasi"),
                                    status = "warning", solidHeader = TRUE, width = NULL,
                                    verbatimTextOutput(ns("correlation_stats"))
                                )
                            )
                        )
                    ),

                    # Tab 3: Correlation Matrix & Heatmap
                    tabPanel(
                        title = tags$span(icon("th"), "Matriks Korelasi"),
                        value = "correlation",
                        br(),
                        fluidRow(
                            column(
                                width = 3,
                                box(
                                    title = tags$span(icon("list"), "Pilih Variabel"),
                                    status = "primary", solidHeader = TRUE, width = NULL,
                                    checkboxGroupInput(ns("corr_vars"), "Variabel untuk Korelasi:",
                                        choices = NULL
                                    ),
                                    hr(),
                                    selectInput(ns("corr_method"), "Metode Korelasi:",
                                        choices = c(
                                            "Pearson" = "pearson",
                                            "Spearman" = "spearman",
                                            "Kendall" = "kendall"
                                        ),
                                        selected = "pearson", width = "100%"
                                    ),
                                    numericInput(ns("corr_threshold"), "Threshold Signifikansi:",
                                        value = 0.05, min = 0.01, max = 0.1, step = 0.01, width = "100%"
                                    ),
                                    actionButton(ns("generate_corr"), "Generate Heatmap",
                                        class = "btn-warning", icon = icon("fire"), width = "100%"
                                    )
                                )
                            ),
                            column(
                                width = 9,
                                box(
                                    title = tags$span(icon("fire"), "Correlation Heatmap"),
                                    status = "warning", solidHeader = TRUE, width = NULL,
                                    plotlyOutput(ns("correlation_heatmap"), height = "500px")
                                ),
                                box(
                                    title = tags$span(icon("table"), "Matriks Korelasi"),
                                    status = "info", solidHeader = TRUE, width = NULL,
                                    DT::DTOutput(ns("correlation_table"))
                                )
                            )
                        )
                    ),

                    # Tab 4: Group Analysis
                    tabPanel(
                        title = tags$span(icon("users"), "Analisis Kelompok"),
                        value = "group",
                        br(),
                        fluidRow(
                            column(
                                width = 3,
                                box(
                                    title = tags$span(icon("layer-group"), "Pengaturan Grup"),
                                    status = "primary", solidHeader = TRUE, width = NULL,
                                    selectInput(ns("group_var"), "Variabel Pengelompokan:",
                                        choices = NULL, width = "100%"
                                    ),
                                    selectInput(ns("group_target"), "Variabel Target:",
                                        choices = NULL, width = "100%"
                                    ),
                                    radioButtons(ns("group_analysis_type"), "Jenis Analisis:",
                                        choices = c(
                                            "Statistik Deskriptif" = "descriptive",
                                            "Box Plot per Group" = "boxplot",
                                            "Bar Chart (Mean)" = "barchart",
                                            "Violin Plot" = "violin"
                                        ),
                                        selected = "descriptive"
                                    ),
                                    actionButton(ns("generate_group"), "Analyze Groups",
                                        class = "btn-primary", icon = icon("users"), width = "100%"
                                    )
                                )
                            ),
                            column(
                                width = 9,
                                box(
                                    title = tags$span(icon("chart-bar"), "Visualisasi Kelompok"),
                                    status = "primary", solidHeader = TRUE, width = NULL,
                                    plotlyOutput(ns("group_plot"), height = "400px")
                                ),
                                box(
                                    title = tags$span(icon("table"), "Statistik per Kelompok"),
                                    status = "success", solidHeader = TRUE, width = NULL,
                                    DT::DTOutput(ns("group_stats_table"))
                                )
                            )
                        )
                    ),

                    # Tab 5: Interactive Data Tables
                    tabPanel(
                        title = tags$span(icon("table"), "Tabel Interaktif"),
                        value = "tables",
                        br(),
                        fluidRow(
                            column(
                                width = 3,
                                box(
                                    title = tags$span(icon("filter"), "Filter Data"),
                                    status = "primary", solidHeader = TRUE, width = NULL,
                                    selectInput(ns("table_columns"), "Pilih Kolom:",
                                        choices = NULL, multiple = TRUE, width = "100%"
                                    ),
                                    numericInput(ns("table_rows"), "Jumlah Baris per Halaman:",
                                        value = 25, min = 10, max = 100, width = "100%"
                                    ),
                                    checkboxInput(ns("show_only_numeric"), "Hanya Numerik", value = FALSE),
                                    hr(),
                                    actionButton(ns("update_table"), "Update Table",
                                        class = "btn-info", icon = icon("refresh"), width = "100%"
                                    )
                                ),
                                box(
                                    title = tags$span(icon("download"), "Export Data"),
                                    status = "success", solidHeader = TRUE, width = NULL,
                                    downloadButton(ns("download_filtered_csv"), "CSV",
                                        class = "btn-outline-primary", width = "100%"
                                    ),
                                    br(), br(),
                                    downloadButton(ns("download_filtered_excel"), "Excel",
                                        class = "btn-outline-success", width = "100%"
                                    )
                                )
                            ),
                            column(
                                width = 9,
                                box(
                                    title = tags$span(icon("database"), "Dataset Lengkap"),
                                    status = "info", solidHeader = TRUE, width = NULL,
                                    DT::DTOutput(ns("interactive_table"))
                                ),
                                box(
                                    title = tags$span(icon("info"), "Informasi Dataset"),
                                    status = "success", solidHeader = TRUE, width = NULL,
                                    verbatimTextOutput(ns("table_info"))
                                )
                            )
                        )
                    ),

                    # Tab 6: Thematic Maps
                    tabPanel(
                        title = tags$span(icon("map-marked"), "Peta Tematik"),
                        value = "maps",
                        br(),
                        fluidRow(
                            column(
                                width = 3,
                                box(
                                    title = tags$span(icon("map"), "Pengaturan Peta"),
                                    status = "primary", solidHeader = TRUE, width = NULL,
                                    selectInput(ns("map_var"), "Variabel untuk Mapping:",
                                        choices = NULL, width = "100%"
                                    ),
                                    selectInput(ns("map_color_scheme"), "Skema Warna:",
                                        choices = c(
                                            "Viridis" = "viridis",
                                            "Plasma" = "plasma",
                                            "Inferno" = "inferno",
                                            "Blue-Red" = "RdYlBu",
                                            "Red-Yellow-Green" = "RdYlGn"
                                        ),
                                        selected = "viridis", width = "100%"
                                    ),
                                    numericInput(ns("map_bins"), "Jumlah Kelas Warna:",
                                        value = 5, min = 3, max = 10, width = "100%"
                                    ),
                                    checkboxInput(ns("show_markers"), "Tampilkan Markers", value = TRUE),
                                    hr(),
                                    actionButton(ns("generate_map"), "Generate Map",
                                        class = "btn-success", icon = icon("map"), width = "100%"
                                    )
                                ),
                                box(
                                    title = tags$span(icon("info-circle"), "Info Peta"),
                                    status = "info", solidHeader = TRUE, width = NULL,
                                    verbatimTextOutput(ns("map_summary"))
                                )
                            ),
                            column(
                                width = 9,
                                box(
                                    title = tags$span(icon("globe"), "Peta Indonesia Interaktif"),
                                    status = "success", solidHeader = TRUE, width = NULL,
                                    leafletOutput(ns("thematic_map"), height = "500px")
                                )
                            )
                        )
                    )
                )
            )
        ),

        # Download section (always visible)
        fluidRow(
            column(
                width = 12,
                box(
                    title = tags$span(icon("download"), "Unduh Hasil Eksplorasi"),
                    status = "warning", solidHeader = TRUE, width = NULL,
                    collapsible = TRUE, collapsed = FALSE,
                    fluidRow(
                        column(
                            width = 2,
                            h6("Plot/Chart"),
                            downloadButton(ns("download_plot_png"), "PNG",
                                class = "btn-outline-success", icon = icon("image"), width = "100%"
                            ),
                            br(), br(),
                            downloadButton(ns("download_plot_jpg"), "JPEG",
                                class = "btn-outline-info", icon = icon("image"), width = "100%"
                            )
                        ),
                        column(
                            width = 2,
                            h6("Data"),
                            downloadButton(ns("download_summary_csv"), "Summary CSV",
                                class = "btn-outline-primary", icon = icon("file-csv"), width = "100%"
                            ),
                            br(), br(),
                            downloadButton(ns("download_data_excel"), "Excel",
                                class = "btn-outline-success", icon = icon("file-excel"), width = "100%"
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
                            downloadButton(ns("download_report_pdf"), "Eksplorasi PDF",
                                class = "btn-primary", icon = icon("file-pdf"), width = "100%"
                            )
                        ),
                        column(
                            width = 3,
                            h6("Laporan Word"),
                            downloadButton(ns("download_report_word"), "Eksplorasi Word",
                                class = "btn-success", icon = icon("file-word"), width = "100%"
                            )
                        )
                    )
                )
            )
        )
    )
}
