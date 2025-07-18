# Uji Asumsi UI Module
# User interface for assumption testing functionality

#' Uji Asumsi UI Module
#'
#' Creates the user interface for statistical assumption testing
#'
#' @param id Module ID for namespacing
#' @return UI elements for the uji_asumsi tab
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
                    box(
                        title = tags$span(icon("download"), "Unduh Laporan"),
                        status = "info",
                        solidHeader = TRUE,
                        width = NULL,
                        collapsible = TRUE,
                        collapsed = TRUE,
                        h6("Interpretasi Individual"),
                        downloadButton(ns("download_interpretation"), "Unduh Interpretasi (.docx)",
                            class = "btn-outline-info btn-sm", icon = icon("file-word"), width = "100%"
                        ),
                        br(), br(),
                        h6("Laporan Lengkap"),
                        downloadButton(ns("download_report_pdf"), "Unduh Laporan PDF",
                            class = "btn-primary", icon = icon("file-pdf"), width = "100%"
                        ),
                        br(), br(),
                        downloadButton(ns("download_report_word"), "Unduh Laporan Word",
                            class = "btn-success", icon = icon("file-word"), width = "100%"
                        )
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
