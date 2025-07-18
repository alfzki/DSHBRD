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
        # Update variable choices - reactive to data structure changes
        observe({
            # Create reactive dependency on data and data update counter
            req(values$sovi_data)
            data_counter <- values$data_update_counter  # This creates a reactive dependency
            
            numeric_choices <- get_variable_choices(values$sovi_data, "numeric")

            updateSelectInput(session, "dep_var", choices = numeric_choices)
            updateSelectizeInput(session, "indep_vars", choices = numeric_choices)
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

        # Enhanced regression interpretation using helper function
        output$interpretation <- renderUI({
            req(model_result())

            model <- model_result()
            
            # Use the interpretation helper function
            interpretation_text <- interpret_regression(
                lm_result = model,
                alpha = 0.05
            )

            # Convert to HTML with proper formatting
            interpretation_html <- gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", interpretation_text)
            interpretation_html <- gsub("\\n", "<br>", interpretation_html)

            HTML(paste0("<div style='padding: 15px; background-color: #f8f9fa; border-left: 4px solid #007bff; margin: 10px 0;'>",
                       interpretation_html,
                       "</div>"))
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

        # Enhanced Download Handlers

        # 1. Individual interpretation download (Word)
        output$download_interpretation <- downloadHandler(
            filename = function() {
                paste0("interpretasi_regresi_", Sys.Date(), ".docx")
            },
            content = function(file) {
                req(model_result())

                model <- model_result()
                summary_model <- summary(model)

                # Extract key statistics
                r_squared <- summary_model$r.squared
                adj_r_squared <- summary_model$adj.r.squared
                f_statistic <- summary_model$fstatistic[1]
                f_p_value <- pf(f_statistic, summary_model$fstatistic[2],
                    summary_model$fstatistic[3],
                    lower.tail = FALSE
                )

                model_quality <- if (r_squared >= 0.7) {
                    "sangat baik (R² ≥ 0.7)"
                } else if (r_squared >= 0.5) {
                    "baik (R² ≥ 0.5)"
                } else if (r_squared >= 0.3) {
                    "cukup (R² ≥ 0.3)"
                } else {
                    "rendah (R² < 0.3)"
                }

                practical_recommendation <- if (f_p_value < 0.05) {
                    if (r_squared > 0.5) {
                        "Model memiliki kekuatan prediktif yang baik dan dapat digunakan untuk inferensi statistik."
                    } else {
                        "Model secara statistik signifikan namun memiliki kekuatan prediktif yang terbatas. Pertimbangkan untuk menambah variabel prediktor."
                    }
                } else {
                    "Model tidak menunjukkan hubungan yang signifikan. Pertimbangkan untuk menggunakan variabel prediktor yang berbeda atau memeriksa asumsi model."
                }

                # Create Word document
                doc <- officer::read_docx()

                doc <- doc %>%
                    officer::body_add_par("INTERPRETASI HASIL REGRESI LINEAR BERGANDA", style = "heading 1") %>%
                    officer::body_add_par(paste("Tanggal Analisis:", Sys.Date()), style = "Normal") %>%
                    officer::body_add_par("", style = "Normal") %>%
                    officer::body_add_par("RINGKASAN MODEL", style = "heading 2") %>%
                    officer::body_add_par(paste(
                        "Model memiliki kualitas", model_quality,
                        "dalam menjelaskan variabilitas data."
                    ), style = "Normal") %>%
                    officer::body_add_par("", style = "Normal") %>%
                    officer::body_add_par("KEBAIKAN MODEL", style = "heading 2") %>%
                    officer::body_add_par(paste(
                        "• R² =", format_number(r_squared, 4),
                        paste0("(", format_number(r_squared * 100, 1), "% varians dijelaskan)")
                    ), style = "Normal") %>%
                    officer::body_add_par(paste("• Adjusted R² =", format_number(adj_r_squared, 4)), style = "Normal") %>%
                    officer::body_add_par(paste("• F-statistic =", format_number(f_statistic, 4)), style = "Normal") %>%
                    officer::body_add_par(paste("• Model p-value =", format_number(f_p_value, 6)), style = "Normal") %>%
                    officer::body_add_par("", style = "Normal") %>%
                    officer::body_add_par("KESIMPULAN MODEL", style = "heading 2") %>%
                    officer::body_add_par(interpret_p_value(
                        f_p_value,
                        h0 = "Model tidak signifikan (semua koefisien = 0)",
                        h1 = "Model signifikan (minimal ada satu koefisien ≠ 0)"
                    ), style = "Normal") %>%
                    officer::body_add_par("", style = "Normal") %>%
                    officer::body_add_par("ANALISIS KOEFISIEN", style = "heading 2")

                # Add coefficient interpretations
                coeffs <- summary_model$coefficients
                for (i in 2:nrow(coeffs)) {
                    var_name <- rownames(coeffs)[i]
                    coeff_value <- coeffs[i, 1]
                    std_error <- coeffs[i, 2]
                    p_value <- coeffs[i, 4]

                    ci_lower <- coeff_value - 1.96 * std_error
                    ci_upper <- coeff_value + 1.96 * std_error

                    significance_text <- if (p_value < 0.001) {
                        "sangat signifikan (p < 0.001)"
                    } else if (p_value < 0.01) {
                        "signifikan (p < 0.01)"
                    } else if (p_value < 0.05) {
                        "signifikan (p < 0.05)"
                    } else {
                        "tidak signifikan (p ≥ 0.05)"
                    }

                    doc <- doc %>%
                        officer::body_add_par(paste("Variabel:", var_name), style = "heading 3") %>%
                        officer::body_add_par(paste("• Koefisien:", format_number(coeff_value, 4)), style = "Normal") %>%
                        officer::body_add_par(paste(
                            "• 95% CI: [", format_number(ci_lower, 4), ",",
                            format_number(ci_upper, 4), "]"
                        ), style = "Normal") %>%
                        officer::body_add_par(paste("• Status:", significance_text), style = "Normal")

                    if (p_value < 0.05) {
                        doc <- doc %>%
                            officer::body_add_par(paste(
                                "Untuk setiap kenaikan 1 unit", var_name,
                                ", variabel", input$dep_var,
                                ifelse(coeff_value > 0, "meningkat", "menurun"),
                                "sebesar", abs(format_number(coeff_value, 4)), "unit."
                            ), style = "Normal")
                    }
                    doc <- doc %>% officer::body_add_par("", style = "Normal")
                }

                doc <- doc %>%
                    officer::body_add_par("REKOMENDASI", style = "heading 2") %>%
                    officer::body_add_par(practical_recommendation, style = "Normal")

                print(doc, target = file)
            }
        )

        # 2. Comprehensive PDF report
        output$download_report_pdf <- downloadHandler(
            filename = function() {
                paste0("laporan_regresi_", Sys.Date(), ".pdf")
            },
            content = function(file) {
                req(model_result())

                # Prepare parameters for R Markdown template
                params <- list(
                    data = values$sovi_data,
                    model = model_result(),
                    summary_model = summary(model_result()),
                    assumption_tests = assumption_tests(),
                    dep_var = input$dep_var,
                    indep_vars = input$indep_vars,
                    analysis_date = Sys.Date(),
                    interpretation = interpret_regression(model_result(), alpha = 0.05)
                )

                # Render the R Markdown template
                tryCatch({
                    rmarkdown::render(
                        input = "reports/laporan_regresi.Rmd",
                        output_file = file,
                        output_format = "pdf_document",
                        params = params,
                        envir = new.env(parent = globalenv()),
                        quiet = TRUE
                    )
                }, error = function(e) {
                    # If PDF generation fails, create a simple error document
                    writeLines(paste("Error generating PDF report:", e$message), file)
                    showNotification("PDF generation failed. Please try the Word format.", type = "error")
                })
            }
        )

        # 3. Comprehensive Word report
        output$download_report_word <- downloadHandler(
            filename = function() {
                paste0("laporan_regresi_", Sys.Date(), ".docx")
            },
            content = function(file) {
                req(model_result())

                # Prepare parameters for R Markdown template
                params <- list(
                    data = values$sovi_data,
                    model = model_result(),
                    summary_model = summary(model_result()),
                    assumption_tests = assumption_tests(),
                    dep_var = input$dep_var,
                    indep_vars = input$indep_vars,
                    analysis_date = Sys.Date(),
                    interpretation = interpret_regression(model_result(), alpha = 0.05)
                )

                # Create temporary Rmd file for Word output
                temp_rmd <- tempfile(fileext = ".Rmd")
                file.copy("reports/laporan_regresi.Rmd", temp_rmd)

                # Modify YAML header for Word output
                rmd_content <- readLines(temp_rmd)
                yaml_end <- which(rmd_content == "---")[2]
                rmd_content[2] <- "output: word_document"
                writeLines(rmd_content, temp_rmd)

                # Render the R Markdown template
                rmarkdown::render(
                    input = temp_rmd,
                    output_file = file,
                    params = params,
                    envir = new.env(parent = globalenv()),
                    quiet = TRUE
                )
            }
        )
    })
}
