# Regresi UI Module
# User interface for regression analysis

#' Regresi UI Module
#'
#' UI for regression analysis
#'
#' @param id Module ID for namespacing
regresi_ui <- function(id) {
    ns <- NS(id)

    tabItem(
        tabName = "regresi",
        fluidRow(
            box(
                title = "Analisis Regresi Linear Berganda",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                fluidRow(
                    column(
                        width = 4,
                        h4("Pengaturan Model"),
                        selectInput(
                            ns("dep_var"),
                            "Variabel Terikat (Y):",
                            choices = NULL
                        ),
                        selectizeInput(
                            ns("indep_vars"),
                            "Variabel Bebas (X):",
                            choices = NULL,
                            multiple = TRUE
                        ),
                        br(),
                        actionButton(
                            ns("run_regression"),
                            "Jalankan Regresi",
                            icon = icon("calculator"),
                            class = "btn-primary"
                        ),
                        br(), br(),
                        h4("Uji Asumsi"),
                        p("Uji asumsi akan dilakukan otomatis:"),
                        tags$ul(
                            tags$li("Uji Multikolinearitas (VIF)"),
                            tags$li("Uji Heteroskedastisitas (Breusch-Pagan)"),
                            tags$li("Uji Normalitas Residual (Shapiro-Wilk)")
                        )
                    ),
                    column(
                        width = 8,
                        tabsetPanel(
                            id = ns("results_tabs"),
                            tabPanel(
                                "Hasil Regresi",
                                br(),
                                verbatimTextOutput(ns("regression_summary"))
                            ),
                            tabPanel(
                                "Interpretasi",
                                br(),
                                uiOutput(ns("interpretation"))
                            ),
                            tabPanel(
                                "Uji Asumsi",
                                br(),
                                verbatimTextOutput(ns("assumption_tests"))
                            ),
                            tabPanel(
                                "Plot Diagnostik",
                                br(),
                                plotly::plotlyOutput(ns("diagnostic_plots"))
                            )
                        )
                    )
                )
            )
        ),
        fluidRow(
            box(
                title = "Download Hasil",
                status = "success",
                solidHeader = TRUE,
                width = 12,
                p("Download hasil analisis regresi dalam format PDF:"),
                downloadButton(
                    ns("download_results"),
                    "Download Hasil Regresi",
                    icon = icon("download"),
                    class = "btn-success"
                )
            )
        )
    )
}
