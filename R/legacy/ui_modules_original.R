# UI Modules for NusaStat Dashboard
# This file contains all UI module definitions

# Beranda UI Module
# =================
beranda_ui <- function(id) {
    ns <- NS(id)
    tabItem(
        tabName = "beranda",
        fluidRow(
            column(
                width = 12,
                box(
                    title = tags$span(icon("home"), "Selamat Datang di NusaStat Dashboard"),
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
                    p("Unduh informasi lengkap tentang dashboard ini:"),
                    downloadButton(ns("download_info"), "Unduh Info Dashboard (.pdf)",
                        class = "btn-primary", icon = icon("file-pdf")
                    )
                )
            )
        )
    )
}

# Manajemen Data UI Module
# ========================
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

# Eksplorasi Data UI Module
# =========================
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

# Uji Asumsi UI Module
# ====================
uji_asumsi_ui <- function(id) {
    ns <- NS(id)
    tabItem(
        tabName = "uji_asumsi",
        fluidRow(
            column(
                width = 3,
                box(
                    title = tags$span(icon("cog"), "Pengaturan Uji"),
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    h5("Uji Normalitas:"),
                    selectInput(ns("var_normal"), "Pilih Variabel:",
                        choices = NULL, width = "100%"
                    ),
                    hr(),
                    h5("Uji Homogenitas:"),
                    selectInput(ns("var_homogen"), "Variabel Dependen:",
                        choices = NULL, width = "100%"
                    ),
                    selectInput(ns("group_homogen"), "Variabel Grup:",
                        choices = NULL, width = "100%"
                    ),
                    hr(),
                    downloadButton(ns("download_results"), "Unduh Hasil (.pdf)",
                        class = "btn-primary", icon = icon("download")
                    )
                )
            ),
            column(
                width = 9,
                tabsetPanel(
                    tabPanel(
                        "Uji Normalitas",
                        icon = icon("bell"),
                        br(),
                        verbatimTextOutput(ns("normality_test")),
                        plotOutput(ns("normality_plot"), height = "400px"),
                        br(),
                        uiOutput(ns("interpretation_normal"))
                    ),
                    tabPanel(
                        "Uji Homogenitas",
                        icon = icon("balance-scale"),
                        br(),
                        verbatimTextOutput(ns("homogeneity_test")),
                        br(),
                        uiOutput(ns("interpretation_homogen"))
                    )
                )
            )
        )
    )
}

# Uji Beda Rata-Rata UI Module
# =============================
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
                    uiOutput(ns("interpretation")),
                    br(),
                    downloadButton(ns("download_results"), "Unduh Hasil (.pdf)",
                        class = "btn-primary", icon = icon("download")
                    )
                )
            )
        )
    )
}

# Uji Proporsi & Varians UI Module
# ================================
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
                    downloadButton(ns("download_results"), "Unduh Hasil (.pdf)",
                        class = "btn-primary", icon = icon("download")
                    )
                )
            )
        )
    )
}

# Uji ANOVA UI Module
# ===================
uji_anova_ui <- function(id) {
    ns <- NS(id)
    tabItem(
        tabName = "uji_anova",
        fluidRow(
            column(
                width = 3,
                box(
                    title = tags$span(icon("layer-group"), "Pengaturan ANOVA"),
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    radioButtons(ns("anova_type"), "Jenis ANOVA:",
                        choices = c(
                            "Satu Arah" = "one_way",
                            "Dua Arah" = "two_way"
                        ),
                        selected = "one_way"
                    ),
                    selectInput(ns("dep_var"), "Variabel Dependen:",
                        choices = NULL, width = "100%"
                    ),
                    selectInput(ns("factor1"), "Faktor 1:",
                        choices = NULL, width = "100%"
                    ),
                    conditionalPanel(
                        condition = "input.anova_type == 'two_way'",
                        ns = ns,
                        selectInput(ns("factor2"), "Faktor 2:",
                            choices = NULL, width = "100%"
                        ),
                        checkboxInput(ns("interaction"), "Sertakan Interaksi", value = TRUE)
                    ),
                    actionButton(ns("run_anova"), "Jalankan ANOVA",
                        class = "btn-success", icon = icon("play"), width = "100%"
                    )
                )
            ),
            column(
                width = 9,
                tabsetPanel(
                    tabPanel(
                        "Hasil ANOVA",
                        icon = icon("table"),
                        br(),
                        verbatimTextOutput(ns("anova_results")),
                        br(),
                        h4("Interpretasi:"),
                        uiOutput(ns("interpretation"))
                    ),
                    tabPanel(
                        "Post-Hoc Test",
                        icon = icon("search"),
                        br(),
                        verbatimTextOutput(ns("posthoc_results")),
                        br(),
                        uiOutput(ns("posthoc_interpretation"))
                    ),
                    tabPanel(
                        "Visualisasi",
                        icon = icon("chart-bar"),
                        br(),
                        plotly::plotlyOutput(ns("anova_plot"), height = "500px")
                    )
                ),
                br(),
                downloadButton(ns("download_results"), "Unduh Hasil (.pdf)",
                    class = "btn-primary", icon = icon("download")
                )
            )
        )
    )
}

# Regresi Linear Berganda UI Module
# =================================
regresi_ui <- function(id) {
    ns <- NS(id)
    tabItem(
        tabName = "regresi",
        fluidRow(
            column(
                width = 3,
                box(
                    title = tags$span(icon("line-chart"), "Pengaturan Model"),
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    selectInput(ns("dep_var"), "Variabel Dependen (Y):",
                        choices = NULL, width = "100%"
                    ),
                    checkboxGroupInput(ns("indep_vars"), "Variabel Independen (X):",
                        choices = NULL
                    ),
                    actionButton(ns("run_model"), "Jalankan Model",
                        class = "btn-success", icon = icon("play"), width = "100%"
                    ),
                    hr(),
                    h5("Unduhan:"),
                    downloadButton(ns("download_model"), "Unduh Model (.pdf)",
                        class = "btn-sm btn-primary", icon = icon("download")
                    )
                )
            ),
            column(
                width = 9,
                tabsetPanel(
                    tabPanel(
                        "Hasil Model",
                        icon = icon("chart-line"),
                        br(),
                        verbatimTextOutput(ns("model_summary")),
                        br(),
                        h4("Persamaan Model:"),
                        uiOutput(ns("model_equation"))
                    ),
                    tabPanel(
                        "Uji Asumsi",
                        icon = icon("check-circle"),
                        br(),
                        h4("Uji Multikolinearitas (VIF):"),
                        verbatimTextOutput(ns("vif_test")),
                        br(),
                        h4("Uji Heteroskedastisitas (Breusch-Pagan):"),
                        verbatimTextOutput(ns("bp_test")),
                        br(),
                        h4("Uji Normalitas Residual:"),
                        verbatimTextOutput(ns("normality_test")),
                        plotOutput(ns("residual_plots"), height = "400px")
                    ),
                    tabPanel(
                        "Interpretasi",
                        icon = icon("comment"),
                        br(),
                        uiOutput(ns("comprehensive_interpretation"))
                    )
                )
            )
        )
    )
}
