# Additional Server Modules for NusaStat Dashboard
# This file contains the remaining server module implementations

# Uji Proporsi & Varians Server Module
# ====================================
uji_prop_var_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        # Update variable choices
        observe({
            if (!is.null(values$sovi_data)) {
                numeric_vars <- get_numeric_columns(values$sovi_data)
                updateSelectInput(session, "var_variance", choices = numeric_vars)
            }
        })

        # Reactive values
        test_result <- reactiveVal(NULL)

        # Run test
        observeEvent(input$run_test, {
            if (!validate_data(values$sovi_data, "Data SOVI")) {
                return()
            }

            if (input$test_choice == "proportion") {
                req(input$x_prop, input$n_prop, input$p_test)

                if (input$x_prop > input$n_prop) {
                    showNotification("Jumlah sukses tidak boleh lebih dari jumlah total.", type = "error")
                    return()
                }

                if (input$n_prop <= 0) {
                    showNotification("Jumlah total harus lebih dari 0.", type = "error")
                    return()
                }

                result <- prop.test(
                    x = input$x_prop, n = input$n_prop, p = input$p_test,
                    alternative = input$alternative
                )
                test_result(result)
            } else if (input$test_choice == "variance") {
                req(input$var_variance, input$var_test)

                var_data <- values$sovi_data[[input$var_variance]]
                var_data <- var_data[!is.na(var_data)]

                if (length(var_data) < 2) {
                    showNotification("Data tidak cukup untuk uji varians.", type = "error")
                    return()
                }

                if (input$var_test <= 0) {
                    showNotification("Nilai varians uji harus lebih dari 0.", type = "error")
                    return()
                }

                # Chi-square test for variance
                n <- length(var_data)
                sample_var <- var(var_data)
                chi_stat <- (n - 1) * sample_var / input$var_test

                # Calculate p-value based on alternative hypothesis
                if (input$alternative == "two.sided") {
                    p_value <- 2 * min(pchisq(chi_stat, df = n - 1), 1 - pchisq(chi_stat, df = n - 1))
                } else if (input$alternative == "greater") {
                    p_value <- 1 - pchisq(chi_stat, df = n - 1)
                } else {
                    p_value <- pchisq(chi_stat, df = n - 1)
                }

                # Create result object similar to other tests
                result <- list(
                    statistic = chi_stat,
                    p.value = p_value,
                    parameter = n - 1,
                    method = "Chi-square test for variance",
                    data.name = input$var_variance,
                    sample.var = sample_var,
                    null.value = input$var_test,
                    alternative = input$alternative
                )

                test_result(result)
            }

            showNotification("Uji berhasil dilakukan!", type = "message")
        })

        # Display test results
        output$test_results <- renderPrint({
            req(test_result())

            result <- test_result()

            if (input$test_choice == "proportion") {
                cat("Uji Proporsi Satu Sampel\n")
                cat("========================\n\n")
                cat("Data:", input$x_prop, "sukses dari", input$n_prop, "percobaan\n")
                cat("Proporsi sampel:", round(input$x_prop / input$n_prop, 4), "\n")
                cat("Hipotesis nol: p =", input$p_test, "\n")
                cat(
                    "Hipotesis alternatif: p",
                    switch(input$alternative,
                        "two.sided" = "≠",
                        "greater" = ">",
                        "less" = "<"
                    ),
                    input$p_test, "\n\n"
                )
                cat("Statistik uji (Chi-square):", round(result$statistic, 4), "\n")
                cat("Derajat kebebasan:", result$parameter, "\n")
                cat("P-value:", format(result$p.value, scientific = TRUE), "\n")
                cat("Interval kepercayaan 95%:", round(result$conf.int[1], 4), "-", round(result$conf.int[2], 4), "\n")
            } else {
                cat("Uji Varians Satu Sampel\n")
                cat("=======================\n\n")
                cat("Data:", result$data.name, "\n")
                cat("Varians sampel:", round(result$sample.var, 4), "\n")
                cat("Hipotesis nol: σ² =", result$null.value, "\n")
                cat(
                    "Hipotesis alternatif: σ²",
                    switch(result$alternative,
                        "two.sided" = "≠",
                        "greater" = ">",
                        "less" = "<"
                    ),
                    result$null.value, "\n\n"
                )
                cat("Statistik uji (Chi-square):", round(result$statistic, 4), "\n")
                cat("Derajat kebebasan:", result$parameter, "\n")
                cat("P-value:", format(result$p.value, scientific = TRUE), "\n")
            }
        })

        # Generate interpretation
        output$interpretation <- renderUI({
            req(test_result())

            result <- test_result()
            p_value <- result$p.value

            if (input$test_choice == "proportion") {
                h0 <- paste("Proporsi populasi =", input$p_test)
                h1 <- switch(input$alternative,
                    "two.sided" = paste("Proporsi populasi ≠", input$p_test),
                    "greater" = paste("Proporsi populasi >", input$p_test),
                    "less" = paste("Proporsi populasi <", input$p_test)
                )

                sample_prop <- input$x_prop / input$n_prop

                additional_info <- tagList(
                    p(paste("Proporsi sampel:", format_number(sample_prop, 4))),
                    p(paste(
                        "Interval kepercayaan 95%:",
                        format_number(result$conf.int[1], 4), "hingga",
                        format_number(result$conf.int[2], 4)
                    ))
                )
            } else {
                h0 <- paste("Varians populasi =", input$var_test)
                h1 <- switch(input$alternative,
                    "two.sided" = paste("Varians populasi ≠", input$var_test),
                    "greater" = paste("Varians populasi >", input$var_test),
                    "less" = paste("Varians populasi <", input$var_test)
                )

                additional_info <- tagList(
                    p(paste("Varians sampel:", format_number(result$sample.var, 4))),
                    p(paste("Statistik Chi-square:", format_number(result$statistic, 4)))
                )
            }

            interpretation <- interpret_p_value(p_value, h0 = h0, h1 = h1)

            tagList(
                h4("Interpretasi Hasil Uji:"),
                p(interpretation),
                additional_info
            )
        })

        # Download handler
        output$download_results <- downloadHandler(
            filename = function() {
                test_name <- ifelse(input$test_choice == "proportion", "uji_proporsi", "uji_varians")
                paste0(test_name, "_", Sys.Date(), ".pdf")
            },
            content = function(file) {
                temp_rmd <- tempfile(fileext = ".Rmd")

                test_title <- ifelse(input$test_choice == "proportion",
                    "Uji Proporsi Satu Sampel",
                    "Uji Varians Satu Sampel"
                )

                rmd_content <- paste0('---
title: "', test_title, '"
output: pdf_document
date: "`r Sys.Date()`"
---

# ', test_title, "

Hasil uji statistik untuk ", tolower(test_title), ".

## Interpretasi

Berdasarkan hasil uji, dapat disimpulkan apakah hipotesis nol ditolak atau tidak.
")

                writeLines(rmd_content, temp_rmd)
                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )
    })
}

# Uji ANOVA Server Module
# =======================
uji_anova_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        # Update variable choices
        observe({
            if (!is.null(values$sovi_data)) {
                numeric_vars <- get_numeric_columns(values$sovi_data)
                categorical_vars <- get_categorical_columns(values$sovi_data)

                updateSelectInput(session, "dep_var", choices = numeric_vars)
                updateSelectInput(session, "factor1", choices = categorical_vars)
                updateSelectInput(session, "factor2", choices = categorical_vars)
            }
        })

        # Reactive values
        anova_result <- reactiveVal(NULL)
        posthoc_result <- reactiveVal(NULL)

        # Run ANOVA
        observeEvent(input$run_anova, {
            req(input$dep_var, input$factor1)
            if (!validate_data(values$sovi_data, "Data SOVI")) {
                return()
            }

            # Check if factors have enough levels
            factor1_levels <- unique(values$sovi_data[[input$factor1]])
            if (length(factor1_levels) < 2) {
                showNotification("Faktor 1 harus memiliki minimal 2 level.", type = "error")
                return()
            }

            if (input$anova_type == "one_way") {
                # One-way ANOVA
                formula_str <- paste(input$dep_var, "~", input$factor1)
                formula_obj <- as.formula(formula_str)

                tryCatch(
                    {
                        model <- aov(formula_obj, data = values$sovi_data)
                        result <- summary(model)
                        anova_result(list(model = model, summary = result, type = "one_way"))

                        # Post-hoc test (Tukey HSD)
                        if (length(factor1_levels) > 2) {
                            posthoc <- TukeyHSD(model)
                            posthoc_result(posthoc)
                        }
                    },
                    error = function(e) {
                        showNotification(paste("Error dalam ANOVA:", e$message), type = "error")
                        return()
                    }
                )
            } else if (input$anova_type == "two_way") {
                req(input$factor2)

                factor2_levels <- unique(values$sovi_data[[input$factor2]])
                if (length(factor2_levels) < 2) {
                    showNotification("Faktor 2 harus memiliki minimal 2 level.", type = "error")
                    return()
                }

                # Two-way ANOVA
                if (input$interaction) {
                    formula_str <- paste(input$dep_var, "~", input$factor1, "*", input$factor2)
                } else {
                    formula_str <- paste(input$dep_var, "~", input$factor1, "+", input$factor2)
                }

                formula_obj <- as.formula(formula_str)

                tryCatch(
                    {
                        model <- aov(formula_obj, data = values$sovi_data)
                        result <- summary(model)
                        anova_result(list(model = model, summary = result, type = "two_way"))

                        # Post-hoc test for main effects
                        posthoc1 <- TukeyHSD(model, which = input$factor1)
                        posthoc2 <- TukeyHSD(model, which = input$factor2)
                        posthoc_result(list(factor1 = posthoc1, factor2 = posthoc2))
                    },
                    error = function(e) {
                        showNotification(paste("Error dalam ANOVA:", e$message), type = "error")
                        return()
                    }
                )
            }

            showNotification("ANOVA berhasil dilakukan!", type = "message")
        })

        # Display ANOVA results
        output$anova_results <- renderPrint({
            req(anova_result())

            result <- anova_result()

            cat("ANOVA Results\n")
            cat("=============\n\n")

            if (result$type == "one_way") {
                cat("One-Way ANOVA\n")
                cat("Dependent Variable:", input$dep_var, "\n")
                cat("Factor:", input$factor1, "\n\n")
            } else {
                cat("Two-Way ANOVA\n")
                cat("Dependent Variable:", input$dep_var, "\n")
                cat("Factor 1:", input$factor1, "\n")
                cat("Factor 2:", input$factor2, "\n")
                cat("Interaction:", ifelse(input$interaction, "Yes", "No"), "\n\n")
            }

            print(result$summary)
        })

        # ANOVA interpretation
        output$interpretation <- renderUI({
            req(anova_result())

            result <- anova_result()
            anova_table <- result$summary[[1]]

            interpretations <- list()

            if (result$type == "one_way") {
                p_value <- anova_table$`Pr(>F)`[1]
                f_stat <- anova_table$`F value`[1]

                interpretation <- interpret_p_value(
                    p_value,
                    h0 = paste("Semua rata-rata grup", input$factor1, "sama"),
                    h1 = paste("Minimal ada satu rata-rata grup", input$factor1, "yang berbeda")
                )

                interpretations <- list(
                    h4("Interpretasi ANOVA Satu Arah:"),
                    p(interpretation),
                    p(paste("F-statistic:", format_number(f_stat, 4))),
                    p(paste("Derajat kebebasan:", anova_table$Df[1], "dan", anova_table$Df[2]))
                )
            } else {
                # Two-way ANOVA interpretations
                interpretations <- list(h4("Interpretasi ANOVA Dua Arah:"))

                # Main effect 1
                if (nrow(anova_table) >= 1) {
                    p_value1 <- anova_table$`Pr(>F)`[1]
                    interpretation1 <- interpret_p_value(
                        p_value1,
                        h0 = paste("Tidak ada efek utama", input$factor1),
                        h1 = paste("Ada efek utama", input$factor1)
                    )
                    interpretations <- append(interpretations, list(
                        h5(paste("Efek Utama", input$factor1, ":")),
                        p(interpretation1)
                    ))
                }

                # Main effect 2
                if (nrow(anova_table) >= 2) {
                    p_value2 <- anova_table$`Pr(>F)`[2]
                    interpretation2 <- interpret_p_value(
                        p_value2,
                        h0 = paste("Tidak ada efek utama", input$factor2),
                        h1 = paste("Ada efek utama", input$factor2)
                    )
                    interpretations <- append(interpretations, list(
                        h5(paste("Efek Utama", input$factor2, ":")),
                        p(interpretation2)
                    ))
                }

                # Interaction effect
                if (input$interaction && nrow(anova_table) >= 3) {
                    p_value_int <- anova_table$`Pr(>F)`[3]
                    interpretation_int <- interpret_p_value(
                        p_value_int,
                        h0 = paste("Tidak ada efek interaksi", input$factor1, "×", input$factor2),
                        h1 = paste("Ada efek interaksi", input$factor1, "×", input$factor2)
                    )
                    interpretations <- append(interpretations, list(
                        h5(paste("Efek Interaksi", input$factor1, "×", input$factor2, ":")),
                        p(interpretation_int)
                    ))
                }
            }

            do.call(tagList, interpretations)
        })

        # Post-hoc test results
        output$posthoc_results <- renderPrint({
            req(posthoc_result())

            result <- posthoc_result()

            cat("Post-Hoc Test Results (Tukey HSD)\n")
            cat("=================================\n\n")

            if (is.list(result) && "factor1" %in% names(result)) {
                cat("Post-hoc for", input$factor1, ":\n")
                print(result$factor1)
                cat("\n")
                cat("Post-hoc for", input$factor2, ":\n")
                print(result$factor2)
            } else {
                print(result)
            }
        })

        # Post-hoc interpretation
        output$posthoc_interpretation <- renderUI({
            req(posthoc_result())

            tagList(
                h4("Interpretasi Post-Hoc Test:"),
                p("Uji post-hoc menunjukkan perbedaan spesifik antara pasangan grup."),
                p("Nilai 'p adj' yang kurang dari 0.05 menunjukkan perbedaan yang signifikan antara dua grup."),
                p("Interval kepercayaan yang tidak mengandung 0 juga menunjukkan perbedaan yang signifikan.")
            )
        })

        # ANOVA plot
        output$anova_plot <- plotly::renderPlotly({
            req(anova_result())

            if (anova_result()$type == "one_way") {
                # Box plot for one-way ANOVA
                p <- ggplot(values$sovi_data, aes_string(x = input$factor1, y = input$dep_var)) +
                    geom_boxplot(fill = "lightblue", alpha = 0.7) +
                    geom_jitter(width = 0.2, alpha = 0.5) +
                    labs(
                        title = paste("Box Plot:", input$dep_var, "by", input$factor1),
                        x = input$factor1,
                        y = input$dep_var
                    ) +
                    theme_minimal()
            } else {
                # Interaction plot for two-way ANOVA
                p <- ggplot(values$sovi_data, aes_string(
                    x = input$factor1, y = input$dep_var,
                    color = input$factor2
                )) +
                    geom_boxplot(position = position_dodge(width = 0.8)) +
                    labs(
                        title = paste("Interaction Plot:", input$dep_var, "by", input$factor1, "and", input$factor2),
                        x = input$factor1,
                        y = input$dep_var,
                        color = input$factor2
                    ) +
                    theme_minimal()
            }

            plotly::ggplotly(p, tooltip = c("x", "y"))
        })

        # Download handler
        output$download_results <- downloadHandler(
            filename = function() {
                paste0("anova_results_", Sys.Date(), ".pdf")
            },
            content = function(file) {
                temp_rmd <- tempfile(fileext = ".Rmd")

                anova_type <- ifelse(input$anova_type == "one_way", "Satu Arah", "Dua Arah")

                rmd_content <- paste0('---
title: "Hasil ANOVA ', anova_type, '"
output: pdf_document
date: "`r Sys.Date()`"
---

# ANOVA ', anova_type, "

Hasil analisis varians untuk menguji perbedaan rata-rata antar grup.

## Interpretasi

Berdasarkan hasil ANOVA, dapat disimpulkan apakah terdapat perbedaan signifikan antar grup.
")

                writeLines(rmd_content, temp_rmd)
                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )
    })
}

# Regresi Linear Berganda Server Module
# =====================================
regresi_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        # Update variable choices
        observe({
            if (!is.null(values$sovi_data)) {
                numeric_vars <- get_numeric_columns(values$sovi_data)
                updateSelectInput(session, "dep_var", choices = numeric_vars)
                updateCheckboxGroupInput(session, "indep_vars", choices = numeric_vars)
            }
        })

        # Reactive values
        regression_model <- reactiveVal(NULL)

        # Run regression model
        observeEvent(input$run_model, {
            req(input$dep_var, input$indep_vars)

            if (!validate_data(values$sovi_data, "Data SOVI")) {
                return()
            }

            if (length(input$indep_vars) < 1) {
                showNotification("Pilih minimal satu variabel independen.", type = "error")
                return()
            }

            if (input$dep_var %in% input$indep_vars) {
                showNotification("Variabel dependen tidak boleh sama dengan variabel independen.", type = "error")
                return()
            }

            # Create formula
            formula_str <- paste(input$dep_var, "~", paste(input$indep_vars, collapse = " + "))
            formula_obj <- as.formula(formula_str)

            tryCatch(
                {
                    # Fit the model
                    model <- lm(formula_obj, data = values$sovi_data)
                    regression_model(model)

                    showNotification("Model regresi berhasil dibuat!", type = "message")
                },
                error = function(e) {
                    showNotification(paste("Error dalam model regresi:", e$message), type = "error")
                    return()
                }
            )
        })

        # Display model summary
        output$model_summary <- renderPrint({
            req(regression_model())

            model <- regression_model()
            summary(model)
        })

        # Model equation
        output$model_equation <- renderUI({
            req(regression_model())

            model <- regression_model()
            coefs <- coef(model)

            # Build equation string
            equation_parts <- c()
            equation_parts <- c(equation_parts, paste(input$dep_var, "="))
            equation_parts <- c(equation_parts, format_number(coefs[1], 4))

            for (i in 2:length(coefs)) {
                coef_val <- coefs[i]
                var_name <- names(coefs)[i]

                if (coef_val >= 0) {
                    equation_parts <- c(equation_parts, "+", format_number(coef_val, 4), "×", var_name)
                } else {
                    equation_parts <- c(equation_parts, "-", format_number(abs(coef_val), 4), "×", var_name)
                }
            }

            equation_str <- paste(equation_parts, collapse = " ")

            tagList(
                h4("Persamaan Regresi:"),
                p(equation_str, style = "font-size: 16px; font-weight: bold; color: #2c3e50;"),
                br(),
                h5("Keterangan:"),
                p(paste("•", input$dep_var, ": variabel dependen")),
                lapply(input$indep_vars, function(var) {
                    p(paste("•", var, ": variabel independen"))
                })
            )
        })

        # VIF test for multicollinearity
        output$vif_test <- renderPrint({
            req(regression_model())

            model <- regression_model()

            if (length(input$indep_vars) < 2) {
                cat("VIF test memerlukan minimal 2 variabel independen.\n")
                return()
            }

            tryCatch(
                {
                    vif_values <- car::vif(model)
                    cat("Variance Inflation Factor (VIF)\n")
                    cat("===============================\n\n")

                    if (is.matrix(vif_values)) {
                        print(vif_values)
                    } else {
                        for (i in 1:length(vif_values)) {
                            cat(names(vif_values)[i], ":", format_number(vif_values[i], 4), "\n")
                        }
                    }

                    cat("\nInterpretasi VIF:\n")
                    cat("• VIF < 5: Tidak ada multikolinearitas\n")
                    cat("• VIF 5-10: Multikolinearitas sedang\n")
                    cat("• VIF > 10: Multikolinearitas tinggi\n")
                },
                error = function(e) {
                    cat("Error dalam uji VIF:", e$message, "\n")
                }
            )
        })

        # Breusch-Pagan test for heteroscedasticity
        output$bp_test <- renderPrint({
            req(regression_model())

            model <- regression_model()

            tryCatch(
                {
                    bp_result <- lmtest::bptest(model)
                    cat("Breusch-Pagan Test for Heteroscedasticity\n")
                    cat("=========================================\n\n")
                    cat("Statistik uji:", format_number(bp_result$statistic, 4), "\n")
                    cat("Derajat kebebasan:", bp_result$parameter, "\n")
                    cat("P-value:", format(bp_result$p.value, scientific = TRUE), "\n\n")

                    if (bp_result$p.value < 0.05) {
                        cat("Keputusan: Tolak H0 - Ada heteroskedastisitas\n")
                    } else {
                        cat("Keputusan: Gagal tolak H0 - Tidak ada heteroskedastisitas\n")
                    }
                },
                error = function(e) {
                    cat("Error dalam uji Breusch-Pagan:", e$message, "\n")
                }
            )
        })

        # Normality test for residuals
        output$normality_test <- renderPrint({
            req(regression_model())

            model <- regression_model()
            residuals <- residuals(model)

            tryCatch(
                {
                    # Use sample if too many observations
                    if (length(residuals) > 5000) {
                        residuals <- sample(residuals, 5000)
                        cat("Catatan: Menggunakan sampel 5000 residual untuk uji normalitas\n\n")
                    }

                    shapiro_result <- shapiro.test(residuals)
                    cat("Shapiro-Wilk Test for Normality of Residuals\n")
                    cat("============================================\n\n")
                    cat("Statistik uji:", format_number(shapiro_result$statistic, 4), "\n")
                    cat("P-value:", format(shapiro_result$p.value, scientific = TRUE), "\n\n")

                    if (shapiro_result$p.value < 0.05) {
                        cat("Keputusan: Tolak H0 - Residual tidak berdistribusi normal\n")
                    } else {
                        cat("Keputusan: Gagal tolak H0 - Residual berdistribusi normal\n")
                    }
                },
                error = function(e) {
                    cat("Error dalam uji normalitas residual:", e$message, "\n")
                }
            )
        })

        # Residual plots
        output$residual_plots <- renderPlot({
            req(regression_model())

            model <- regression_model()

            par(mfrow = c(2, 2))

            # Residuals vs Fitted
            plot(model, which = 1, main = "Residuals vs Fitted")

            # Normal Q-Q
            plot(model, which = 2, main = "Normal Q-Q Plot")

            # Scale-Location
            plot(model, which = 3, main = "Scale-Location")

            # Residuals vs Leverage
            plot(model, which = 5, main = "Residuals vs Leverage")
        })

        # Comprehensive interpretation
        output$comprehensive_interpretation <- renderUI({
            req(regression_model())

            model <- regression_model()
            model_summary <- summary(model)

            # Extract key statistics
            r_squared <- model_summary$r.squared
            adj_r_squared <- model_summary$adj.r.squared
            f_statistic <- model_summary$fstatistic[1]
            f_p_value <- pf(f_statistic,
                model_summary$fstatistic[2],
                model_summary$fstatistic[3],
                lower.tail = FALSE
            )

            # Build interpretation
            interpretations <- list(
                h4("Interpretasi Komprehensif Model Regresi:"),
                h5("1. Kebaikan Model (Goodness of Fit):"),
                p(paste(
                    "• R-squared:", format_number(r_squared, 4),
                    "- Model mampu menjelaskan", format_number(r_squared * 100, 1),
                    "% variasi dari variabel", input$dep_var
                )),
                p(paste(
                    "• Adjusted R-squared:", format_number(adj_r_squared, 4),
                    "- R-squared yang disesuaikan dengan jumlah variabel"
                )),
                h5("2. Uji Signifikansi Model (Uji F):"),
                p(paste("• F-statistic:", format_number(f_statistic, 4))),
                p(paste("• P-value:", format(f_p_value, scientific = TRUE))),
                if (f_p_value < 0.05) {
                    p("• Keputusan: Model regresi signifikan secara statistik")
                } else {
                    p("• Keputusan: Model regresi tidak signifikan secara statistik")
                },
                h5("3. Uji Signifikansi Koefisien (Uji t):"),
                lapply(2:length(coef(model)), function(i) {
                    var_name <- names(coef(model))[i]
                    coef_val <- coef(model)[i]
                    p_val <- model_summary$coefficients[i, 4]

                    tagList(
                        p(paste("• Variabel", var_name, ":")),
                        p(paste("  - Koefisien:", format_number(coef_val, 4))),
                        p(paste("  - P-value:", format(p_val, scientific = TRUE))),
                        if (p_val < 0.05) {
                            p(paste("  - Keputusan: Berpengaruh signifikan terhadap", input$dep_var))
                        } else {
                            p(paste("  - Keputusan: Tidak berpengaruh signifikan terhadap", input$dep_var))
                        }
                    )
                })
            )

            do.call(tagList, interpretations)
        })

        # Download handler
        output$download_model <- downloadHandler(
            filename = function() {
                paste0("model_regresi_", Sys.Date(), ".pdf")
            },
            content = function(file) {
                temp_rmd <- tempfile(fileext = ".Rmd")

                rmd_content <- paste0(
                    '---
title: "Model Regresi Linear Berganda"
output: pdf_document
date: "`r Sys.Date()`"
---

# Model Regresi Linear Berganda

Variabel Dependen: ', input$dep_var, "
Variabel Independen: ", paste(input$indep_vars, collapse = ", "), "

## Ringkasan Model

Model regresi linear berganda telah dibuat untuk memprediksi ", input$dep_var,
                    " berdasarkan variabel independen yang dipilih.

## Interpretasi

Berdasarkan hasil analisis regresi, dapat disimpulkan hubungan antar variabel dan signifikansi model.
"
                )

                writeLines(rmd_content, temp_rmd)
                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )
    })
}
