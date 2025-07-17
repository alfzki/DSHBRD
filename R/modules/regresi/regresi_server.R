# Regresi Server Module
# Server logic for regression analysis

#' Regresi Server Module
#'
#' Server logic for multiple linear regression
#'
#' @param id Module ID for namespacing
#' @param values Reactive values object containing shared data
regresi_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        # Update variable choices
        observe({
            if (!is.null(values$sovi_data)) {
                numeric_vars <- get_numeric_columns(values$sovi_data)

                updateSelectInput(session, "dep_var", choices = numeric_vars)
                updateSelectizeInput(session, "indep_vars", choices = numeric_vars)
            }
        })

        # Reactive values
        model_result <- reactiveVal(NULL)
        assumption_tests <- reactiveVal(NULL)

        # Run regression
        observeEvent(input$run_regression, {
            req(input$dep_var, input$indep_vars)
            if (!validate_data(values$sovi_data, "Data SOVI")) {
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
                    model <- lm(formula_obj, data = values$sovi_data)
                    model_result(model)

                    # Run assumption tests
                    assumptions <- list()

                    # VIF test (only if more than 1 independent variable)
                    if (length(input$indep_vars) > 1) {
                        library(car)
                        vif_values <- vif(model)
                        assumptions$vif <- vif_values
                    }

                    # Breusch-Pagan test for heteroscedasticity
                    library(lmtest)
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

        # Regression interpretation
        output$interpretation <- renderUI({
            req(model_result())

            model <- model_result()
            summary_model <- summary(model)

            interpretations <- list(
                h4("Interpretasi Model Regresi:"),
                br(),
                h5("R-squared:"),
                p(paste("Nilai R² =", format_number(summary_model$r.squared, 4))),
                p(paste(
                    "Artinya", format_number(summary_model$r.squared * 100, 2),
                    "% variabilitas variabel terikat dapat dijelaskan oleh model."
                )),
                br(),
                h5("F-statistic:"),
                p(paste("F-statistic =", format_number(summary_model$fstatistic[1], 4))),
                p(paste("p-value =", format_number(pf(summary_model$fstatistic[1],
                    summary_model$fstatistic[2],
                    summary_model$fstatistic[3],
                    lower.tail = FALSE
                ), 6))),
                p(interpret_p_value(
                    pf(summary_model$fstatistic[1],
                        summary_model$fstatistic[2],
                        summary_model$fstatistic[3],
                        lower.tail = FALSE
                    ),
                    h0 = "Model tidak signifikan (semua koefisien = 0)",
                    h1 = "Model signifikan (minimal ada satu koefisien ≠ 0)"
                )),
                br(),
                h5("Koefisien:"),
                p("Interpretasi koefisien regresi:")
            )

            # Add coefficient interpretations
            coeffs <- summary_model$coefficients
            for (i in 2:nrow(coeffs)) { # Skip intercept
                var_name <- rownames(coeffs)[i]
                coeff_value <- coeffs[i, 1]
                p_value <- coeffs[i, 4]

                interpretation <- paste0(
                    "• ", var_name, ": β = ", format_number(coeff_value, 4),
                    " (p = ", format_number(p_value, 4), ")"
                )

                if (p_value < 0.05) {
                    interpretation <- paste(interpretation, "- Signifikan")
                } else {
                    interpretation <- paste(interpretation, "- Tidak signifikan")
                }

                interpretations <- append(interpretations, list(p(interpretation)))
            }

            do.call(tagList, interpretations)
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

        # Download handler
        output$download_results <- downloadHandler(
            filename = function() {
                paste0("regression_results_", Sys.Date(), ".pdf")
            },
            content = function(file) {
                temp_rmd <- tempfile(fileext = ".Rmd")

                dep_var <- input$dep_var
                indep_vars <- paste(input$indep_vars, collapse = ", ")

                rmd_content <- paste0('---
title: "Hasil Analisis Regresi Linear Berganda"
output: pdf_document
date: "`r Sys.Date()`"
---

# Analisis Regresi Linear Berganda

Hasil analisis regresi untuk menguji hubungan antara variabel bebas dan terikat.

## Variabel

- Variabel Terikat: ', dep_var, "
- Variabel Bebas: ", indep_vars, "

## Interpretasi

Berdasarkan hasil regresi, dapat disimpulkan hubungan dan signifikansi variabel dalam model.
")

                writeLines(rmd_content, temp_rmd)
                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )
    })
}
