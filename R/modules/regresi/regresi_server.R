# Regresi Server Module
# Server logic for regression analysis

#' Regresi Server Module
#'
#' Server logic for multiple linear regression
#'
#' @param id Module ID for namespacing
#' @param data A reactive expression returning the dataset.
#' @param update_trigger A reactive expression that invalidates when data is updated.
regresi_server <- function(id, data, update_trigger) {
    moduleServer(id, function(input, output, session) {
        # Update variable choices - reactive to data structure changes
        observe({
            # Create reactive dependency on data and data update counter
            req(data())
            update_trigger() # Create a reactive dependency on the trigger
            
            numeric_choices <- get_variable_choices(data(), "numeric")

            updateSelectInput(session, "dep_var", choices = numeric_choices)
            updateSelectizeInput(session, "indep_vars", choices = numeric_choices)
        })

        # Reactive values
        model_result <- reactiveVal(NULL)
        assumption_tests <- reactiveVal(NULL)

        # Run regression
        observeEvent(input$run_regression, {
            req(input$dep_var, input$indep_vars)
            if (!validate_data(data(), "Data SOVI")) {
                return()
            }

            if (length(input$indep_vars) < 1) {
                showNotification("Pilih minimal 1 variabel bebas.", type = "error")
                return()
            }

            # Build formula
            formula_str <- paste(input$dep_var, "~", paste(input$indep_vars, collapse = " + "))
            formula_obj <- as.formula(formula_str)

            tryCatch(
                {
                    # Run regression
                    model <- lm(formula_obj, data = data())
                    model_result(model)

                    # Run assumption tests
                    assumptions <- list()

                    # VIF test (only if more than 1 independent variable)
                    if (length(input$indep_vars) > 1) {
                        vif_values <- vif(model)
                        assumptions$vif <- vif_values
                    }

                    # Breusch-Pagan test for heteroscedasticity
                    bp_test <- bptest(model)
                    assumptions$breusch_pagan <- bp_test

                    # Shapiro-Wilk test for normality of residuals
                    residuals <- residuals(model)
                    if (length(residuals) <= 5000) { # Shapiro test limit
                        shapiro_test <- shapiro.test(residuals)
                        assumptions$shapiro <- shapiro_test
                    }

                    assumption_tests(assumptions)

                    showNotification("Regresi berhasil dilakukan!", type = "message")
                },
                error = function(e) {
                    showNotification(paste("Error dalam regresi:", e$message), type = "error")
                    return()
                }
            )
        })

        # Display regression results
        output$regression_summary <- renderPrint({
            req(model_result())

            model <- model_result()

            cat("Multiple Linear Regression Results\n")
            cat("==================================\n\n")

            print(summary(model))
        })

        # Refactored interpretation UI to use the centralized report renderer
        output$interpretation <- renderUI({
            req(model_result())

            # Define a temporary file path for the HTML fragment
            temp_html_file <- tempfile(fileext = ".html")

            # Render only the interpretation part of the report to an HTML fragment
            render_regression_report(
                model = model_result(),
                assumption_tests = assumption_tests(),
                dep_var = input$dep_var,
                indep_vars = input$indep_vars,
                output_format = "html_fragment",
                output_file = temp_html_file,
                output_type = "interpretation_only"
            )

            # Include the generated HTML fragment in the UI
            includeHTML(temp_html_file)
        })

        # Display assumption tests
        output$assumption_tests <- renderPrint({
            req(assumption_tests())

            tests <- assumption_tests()

            cat("Assumption Tests\n")
            cat("================\n\n")

            # VIF test
            if (!is.null(tests$vif)) {
                cat("1. Multicollinearity Test (VIF)\n")
                cat("-------------------------------\n")
                print(tests$vif)
                cat("Interpretation: VIF > 10 indicates severe multicollinearity\n\n")
            }

            # Breusch-Pagan test
            if (!is.null(tests$breusch_pagan)) {
                cat("2. Heteroscedasticity Test (Breusch-Pagan)\n")
                cat("------------------------------------------\n")
                print(tests$breusch_pagan)
                cat("H0: Homoscedasticity (constant variance)\n")
                cat("H1: Heteroscedasticity (non-constant variance)\n\n")
            }

            # Shapiro-Wilk test
            if (!is.null(tests$shapiro)) {
                cat("3. Normality Test (Shapiro-Wilk)\n")
                cat("--------------------------------\n")
                print(tests$shapiro)
                cat("H0: Residuals are normally distributed\n")
                cat("H1: Residuals are not normally distributed\n\n")
            }
        })

        # Diagnostic plots
        output$diagnostic_plots <- plotly::renderPlotly({
            req(model_result())

            model <- model_result()

            # Create diagnostic plots
            residuals <- residuals(model)
            fitted_values <- fitted(model)

            # Residuals vs Fitted
            p1 <- ggplot(
                data.frame(fitted = fitted_values, residuals = residuals),
                aes(x = fitted, y = residuals)
            ) +
                geom_point(alpha = 0.6) +
                geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
                geom_smooth(se = FALSE, color = "blue") +
                labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
                theme_minimal()

            plotly::ggplotly(p1)
        })

        # Refactored Download Handlers using the centralized report helper

        # 1. Download Word Report (replaces old interpretation and word download)
        output$download_report_word <- downloadHandler(
            filename = function() {
                paste0("laporan_regresi_", Sys.Date(), ".docx")
            },
            content = function(file) {
                req(model_result())
                tryCatch({
                    render_regression_report(
                        model = model_result(),
                        assumption_tests = assumption_tests(),
                        dep_var = input$dep_var,
                        indep_vars = input$indep_vars,
                        output_format = "word_document",
                        output_file = file
                    )
                }, error = function(e) {
                    showNotification(paste("Error generating Word report:", e$message), type = "error")
                })
            }
        )

        # Keep the old download_interpretation button, but have it download the full Word report
        output$download_interpretation <- output$download_report_word

        # 2. Download PDF Report
        output$download_report_pdf <- downloadHandler(
            filename = function() {
                paste0("laporan_regresi_", Sys.Date(), ".pdf")
            },
            content = function(file) {
                req(model_result())
                tryCatch({
                    render_regression_report(
                        model = model_result(),
                        assumption_tests = assumption_tests(),
                        dep_var = input$dep_var,
                        indep_vars = input$indep_vars,
                        output_format = "pdf_document",
                        output_file = file
                    )
                }, error = function(e) {
                    showNotification(paste("Error generating PDF report:", e$message), type = "error")
                })
            }
        )
    })
}
