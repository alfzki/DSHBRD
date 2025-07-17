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
