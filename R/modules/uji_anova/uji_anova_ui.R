# Uji ANOVA UI Module
# User interface for ANOVA testing

#' Uji ANOVA UI Module
#'
#' Creates the user interface for ANOVA tests
#'
#' @param id Module ID for namespacing
#' @return UI elements for the uji_anova tab
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
                box(
                    title = tags$span(icon("download"), "Unduh Laporan"),
                    status = "info",
                    solidHeader = TRUE,
                    width = NULL,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    h5("Unduhan Individual"),
                    downloadButton(ns("download_interpretation"), "Unduh Interpretasi (.docx)",
                        class = "btn-outline-info btn-sm", icon = icon("file-word"), width = "100%"
                    ),
                    br(), br(),
                    downloadButton(ns("download_plot_jpg"), "Unduh Plot (.jpg)",
                        class = "btn-outline-success btn-sm", icon = icon("image"), width = "100%"
                    ),
                    br(), br(),
                    h5("Laporan Lengkap"),
                    downloadButton(ns("download_report_pdf"), "Unduh Laporan PDF",
                        class = "btn-primary", icon = icon("file-pdf"), width = "100%"
                    ),
                    br(), br(),
                    downloadButton(ns("download_report_word"), "Unduh Laporan Word",
                        class = "btn-success", icon = icon("file-word"), width = "100%"
                    )
                )
            )
        )
    )
}
