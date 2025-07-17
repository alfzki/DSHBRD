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
                    downloadButton(ns("download_results"), "Unduh Hasil (.pdf)",
                        class = "btn-primary", icon = icon("download")
                    )
                )
            )
        )
    )
}
