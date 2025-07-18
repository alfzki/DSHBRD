# NusaStat Dashboard - Simplified Version
# This version works with basic packages and core Shiny functionality

# Load basic configuration
source("global_simple.R")

# Simple UI
ui <- fluidPage(
    titlePanel("NusaStat Dashboard - Basic Version"),
    navbarPage(
        "NusaStat",

        # Home tab
        tabPanel(
            "Beranda",
            fluidRow(
                column(
                    12,
                    h2("Selamat Datang di NusaStat Dashboard"),
                    p("Dashboard ini dirancang untuk analisis statistik data kerentanan sosial Indonesia."),
                    hr(),
                    h3("Fitur yang Tersedia:"),
                    tags$ul(
                        tags$li("Eksplorasi data dasar"),
                        tags$li("Statistik deskriptif"),
                        tags$li("Uji statistik sederhana"),
                        tags$li("Visualisasi data")
                    ),
                    hr(),
                    h4("Status Sistem:"),
                    verbatimTextOutput("system_status")
                )
            )
        ),

        # Data exploration tab
        tabPanel(
            "Eksplorasi Data",
            fluidRow(
                column(
                    3,
                    h4("Pengaturan"),
                    selectInput("variable", "Pilih Variabel:", choices = NULL),
                    hr(),
                    actionButton("load_data", "Muat Data", class = "btn-primary")
                ),
                column(
                    9,
                    h4("Hasil Analisis"),
                    tabsetPanel(
                        tabPanel("Statistik Deskriptif", verbatimTextOutput("descriptive_stats")),
                        tabPanel("Histogram", plotOutput("histogram")),
                        tabPanel("Summary", verbatimTextOutput("data_summary"))
                    )
                )
            )
        ),

        # Simple statistics tab
        tabPanel(
            "Uji Statistik",
            fluidRow(
                column(
                    3,
                    h4("Pengaturan Uji"),
                    selectInput("test_variable", "Variabel untuk Uji:", choices = NULL),
                    numericInput("test_value", "Nilai Uji (μ₀):", value = 0),
                    actionButton("run_test", "Jalankan Uji t", class = "btn-success")
                ),
                column(
                    9,
                    h4("Hasil Uji t Satu Sampel"),
                    verbatimTextOutput("t_test_results"),
                    br(),
                    h5("Interpretasi:"),
                    textOutput("t_test_interpretation")
                )
            )
        )
    )
)

# Server logic
server <- function(input, output, session) {
    # Reactive values
    values <- reactiveValues(
        data = NULL
    )

    # System status
    output$system_status <- renderText({
        paste(
            "R Version:", R.version.string, "\n",
            "Shiny Version:", packageVersion("shiny"), "\n",
            "Working Directory:", getwd(), "\n",
            "Data Status:", ifelse(is.null(values$data), "Belum dimuat", "Tersedia")
        )
    })

    # Load data
    observeEvent(input$load_data, {
        tryCatch(
            {
                values$data <- load_data_simple()

                # Update variable choices
                numeric_choices <- get_variable_choices_simple(values$data, "numeric")
                updateSelectInput(session, "variable", choices = numeric_choices)
                updateSelectInput(session, "test_variable", choices = numeric_choices)

                showNotification("Data berhasil dimuat!", type = "message")
            },
            error = function(e) {
                showNotification(paste("Error:", e$message), type = "error")
            }
        )
    })

    # Load data on startup
    observe({
        values$data <- load_data_simple()
        if (!is.null(values$data)) {
            numeric_choices <- get_variable_choices_simple(values$data, "numeric")
            updateSelectInput(session, "variable", choices = numeric_choices)
            updateSelectInput(session, "test_variable", choices = numeric_choices)
        }
    })

    # Descriptive statistics
    output$descriptive_stats <- renderPrint({
        req(input$variable, values$data)

        var_data <- values$data[[input$variable]]
        if (is.numeric(var_data)) {
            cat("Statistik Deskriptif untuk:", input$variable, "\n")
            cat("=" %+% rep("=", 30) %+% "\n\n")
            print(summary(var_data))
            cat("\nStandar Deviasi:", round(sd(var_data, na.rm = TRUE), 4), "\n")
            cat("Varians:", round(var(var_data, na.rm = TRUE), 4), "\n")
        } else {
            cat("Variabel bukan numerik")
        }
    })

    # Histogram
    output$histogram <- renderPlot({
        req(input$variable, values$data)

        var_data <- values$data[[input$variable]]
        if (is.numeric(var_data)) {
            hist(var_data,
                main = paste("Histogram -", input$variable),
                xlab = input$variable,
                ylab = "Frekuensi",
                col = "lightblue",
                border = "white"
            )
        }
    })

    # Data summary
    output$data_summary <- renderPrint({
        req(values$data)

        cat("Ringkasan Dataset\n")
        cat("=================\n\n")
        cat("Jumlah observasi:", nrow(values$data), "\n")
        cat("Jumlah variabel:", ncol(values$data), "\n")
        cat("Variabel numerik:", length(get_numeric_columns_simple(values$data)), "\n")
        cat("Variabel kategorikal:", length(get_categorical_columns_simple(values$data)), "\n\n")

        cat("Nama variabel:\n")
        cat(paste(names(values$data), collapse = ", "), "\n")
    })

    # T-test
    t_test_result <- reactiveVal(NULL)

    observeEvent(input$run_test, {
        req(input$test_variable, values$data)

        var_data <- values$data[[input$test_variable]]
        if (is.numeric(var_data)) {
            result <- t.test(var_data, mu = input$test_value)
            t_test_result(result)
            showNotification("Uji t berhasil dilakukan!", type = "message")
        } else {
            showNotification("Variabel harus numerik!", type = "error")
        }
    })

    # T-test results
    output$t_test_results <- renderPrint({
        req(t_test_result())
        print(t_test_result())
    })

    # T-test interpretation
    output$t_test_interpretation <- renderText({
        req(t_test_result())

        result <- t_test_result()
        interpretation <- interpret_p_value_simple(result$p.value)

        paste(
            "Uji t satu sampel untuk variabel", input$test_variable,
            "dengan nilai uji μ₀ =", input$test_value, ".",
            interpretation,
            "Rata-rata sampel:", round(result$estimate, 4)
        )
    })
}

# Helper function for string concatenation
`%+%` <- function(a, b) paste0(a, b)

# Run the application
shinyApp(ui = ui, server = server)
