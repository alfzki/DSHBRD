# Uji Asumsi Server Module
# Comprehensive assumption testing functionality

#' Uji Asumsi Server Module
#'
#' Provides comprehensive statistical assumption testing
#'
#' @param id Module ID for namespacing
#' @param values Reactive values from parent
uji_asumsi_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        # Reactive values for storing results
        assumption_results <- reactiveValues(
            normality_results = NULL,
            homogeneity_results = NULL,
            independence_results = NULL,
            current_plots = list(),
            data_used = NULL
        )

        # Helper function to check if packages are available
        check_required_packages <- function() {
            required_packages <- c("nortest", "car", "broom", "DescTools")
            missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
            if (length(missing) > 0) {
                showNotification(
                    paste(
                        "Packages missing:", paste(missing, collapse = ", "),
                        "Please install them for full functionality."
                    ),
                    type = "warning", duration = 5
                )
            }
        }

        # Update variable choices when data changes
        observe({
            req(values$current_data)
            check_required_packages()

            # Get numeric variables
            numeric_vars <- names(values$current_data)[sapply(values$current_data, function(x) {
                is.numeric(x) && length(unique(x)) > 2
            })]

            # Get categorical variables
            categorical_vars <- names(values$current_data)[sapply(values$current_data, function(x) {
                is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x)) <= 10)
            })]

            # Update choices
            updateSelectInput(session, "var_normal",
                choices = numeric_vars,
                selected = if (length(numeric_vars) > 0) numeric_vars[1] else NULL
            )

            updateSelectInput(session, "var_homogen",
                choices = numeric_vars,
                selected = if (length(numeric_vars) > 0) numeric_vars[1] else NULL
            )

            updateSelectInput(session, "group_homogen",
                choices = categorical_vars,
                selected = if (length(categorical_vars) > 0) categorical_vars[1] else NULL
            )

            updateSelectInput(session, "var_indep1",
                choices = categorical_vars,
                selected = if (length(categorical_vars) > 0) categorical_vars[1] else NULL
            )

            updateSelectInput(session, "var_indep2",
                choices = categorical_vars,
                selected = if (length(categorical_vars) > 1) categorical_vars[2] else NULL
            )
        })

        # ========== NORMALITY TESTS ==========

        observeEvent(input$run_normality, {
            req(values$current_data, input$var_normal)

            tryCatch(
                {
                    data_to_use <- values$current_data
                    var_data <- data_to_use[[input$var_normal]]

                    # Remove missing values
                    var_data <- var_data[!is.na(var_data)]

                    if (length(var_data) < 3) {
                        showNotification("Insufficient data for normality testing.", type = "error")
                        return()
                    }

                    # Sample if data is too large
                    if (input$sample_large && length(var_data) > 5000) {
                        set.seed(123)
                        var_data <- sample(var_data, 5000)
                        showNotification("Large dataset detected. Using random sample of 5000 observations.",
                            type = "message"
                        )
                    }

                    # Store data for visualization
                    assumption_results$data_used <- var_data

                    # Run selected normality tests
                    test_results <- list()

                    if ("shapiro" %in% input$normality_tests) {
                        if (length(var_data) <= 5000) {
                            shapiro_result <- shapiro.test(var_data)
                            test_results$"Shapiro-Wilk" <- data.frame(
                                Test = "Shapiro-Wilk",
                                Statistic = shapiro_result$statistic,
                                P_Value = shapiro_result$p.value,
                                Normal = ifelse(shapiro_result$p.value > input$alpha_normal, "Ya", "Tidak"),
                                stringsAsFactors = FALSE
                            )
                        }
                    }

                    if ("anderson" %in% input$normality_tests) {
                        if (requireNamespace("nortest", quietly = TRUE)) {
                            anderson_result <- nortest::ad.test(var_data)
                            test_results$"Anderson-Darling" <- data.frame(
                                Test = "Anderson-Darling",
                                Statistic = anderson_result$statistic,
                                P_Value = anderson_result$p.value,
                                Normal = ifelse(anderson_result$p.value > input$alpha_normal, "Ya", "Tidak"),
                                stringsAsFactors = FALSE
                            )
                        }
                    }

                    if ("ks" %in% input$normality_tests) {
                        ks_result <- ks.test(var_data, "pnorm", mean(var_data), sd(var_data))
                        test_results$"Kolmogorov-Smirnov" <- data.frame(
                            Test = "Kolmogorov-Smirnov",
                            Statistic = ks_result$statistic,
                            P_Value = ks_result$p.value,
                            Normal = ifelse(ks_result$p.value > input$alpha_normal, "Ya", "Tidak"),
                            stringsAsFactors = FALSE
                        )
                    }

                    if ("jarque" %in% input$normality_tests) {
                        if (requireNamespace("DescTools", quietly = TRUE)) {
                            jarque_result <- DescTools::JarqueBeraTest(var_data)
                            test_results$"Jarque-Bera" <- data.frame(
                                Test = "Jarque-Bera",
                                Statistic = jarque_result$statistic,
                                P_Value = jarque_result$p.value,
                                Normal = ifelse(jarque_result$p.value > input$alpha_normal, "Ya", "Tidak"),
                                stringsAsFactors = FALSE
                            )
                        }
                    }

                    # Combine results
                    if (length(test_results) > 0) {
                        final_results <- do.call(rbind, test_results)
                        rownames(final_results) <- NULL
                        assumption_results$normality_results <- final_results
                    } else {
                        showNotification("No normality tests were successfully completed.", type = "warning")
                    }
                },
                error = function(e) {
                    showNotification(paste("Error in normality testing:", e$message), type = "error")
                }
            )
        })

        # Normality results table
        output$normality_results_table <- DT::renderDT({
            req(assumption_results$normality_results)

            DT::datatable(
                assumption_results$normality_results,
                options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    dom = "ft"
                ),
                rownames = FALSE
            ) %>%
                DT::formatRound(columns = c("Statistic", "P_Value"), digits = 5) %>%
                DT::formatStyle(
                    "Normal",
                    backgroundColor = DT::styleEqual(c("Ya", "Tidak"), c("lightgreen", "lightcoral"))
                )
        })

        # Normality plots
        output$normality_plots <- renderPlotly({
            req(assumption_results$data_used)

            var_data <- assumption_results$data_used
            var_name <- input$var_normal

            # Create histogram with normal overlay
            p1 <- plot_ly() %>%
                add_histogram(
                    x = var_data, name = "Data", opacity = 0.7,
                    nbinsx = 30, histnorm = "probability density"
                ) %>%
                add_lines(
                    x = seq(min(var_data), max(var_data), length.out = 100),
                    y = dnorm(
                        seq(min(var_data), max(var_data), length.out = 100),
                        mean(var_data), sd(var_data)
                    ),
                    name = "Normal Distribution", line = list(color = "red", width = 2)
                ) %>%
                layout(
                    title = paste("Histogram vs Normal Distribution:", var_name),
                    xaxis = list(title = var_name),
                    yaxis = list(title = "Density"),
                    showlegend = TRUE
                )

            # Create Q-Q plot
            qqplot_data <- qqnorm(var_data, plot.it = FALSE)
            qqline_data <- qqline(var_data, plot.it = FALSE)

            p2 <- plot_ly() %>%
                add_markers(
                    x = qqplot_data$x, y = qqplot_data$y,
                    name = "Sample Quantiles", marker = list(size = 4)
                ) %>%
                add_lines(
                    x = range(qqplot_data$x),
                    y = range(qqplot_data$x) * sd(var_data) + mean(var_data),
                    name = "Theoretical Line", line = list(color = "red", width = 2)
                ) %>%
                layout(
                    title = paste("Q-Q Plot:", var_name),
                    xaxis = list(title = "Theoretical Quantiles"),
                    yaxis = list(title = "Sample Quantiles"),
                    showlegend = TRUE
                )

            # Combine plots
            subplot(p1, p2, nrows = 1, shareY = FALSE, titleX = TRUE) %>%
                layout(title = "Normality Assessment Plots")
        })

        # Normality interpretation
        output$normality_interpretation <- renderUI({
            req(assumption_results$normality_results)

            results <- assumption_results$normality_results
            alpha <- input$alpha_normal
            var_name <- input$var_normal

            # Count normal results
            normal_count <- sum(results$Normal == "Ya", na.rm = TRUE)
            total_tests <- nrow(results)

            interpretation <- if (normal_count == total_tests) {
                tags$div(
                    class = "alert alert-success",
                    tags$h5(icon("check-circle"), "Kesimpulan: Data Berdistribusi Normal"),
                    tags$p(paste(
                        "Semua", total_tests, "uji normalitas menunjukkan bahwa variabel",
                        var_name, "berdistribusi normal (p-value >", alpha, ")."
                    )),
                    tags$p("Asumsi normalitas TERPENUHI. Anda dapat melanjutkan dengan uji statistik parametrik.")
                )
            } else if (normal_count > total_tests / 2) {
                tags$div(
                    class = "alert alert-warning",
                    tags$h5(icon("exclamation-triangle"), "Kesimpulan: Normalitas Sebagian Terpenuhi"),
                    tags$p(paste(normal_count, "dari", total_tests, "uji menunjukkan normalitas.")),
                    tags$p("Pertimbangkan transformasi data atau uji non-parametrik sebagai alternatif.")
                )
            } else {
                tags$div(
                    class = "alert alert-danger",
                    tags$h5(icon("times-circle"), "Kesimpulan: Data TIDAK Berdistribusi Normal"),
                    tags$p(paste("Mayoritas uji menunjukkan bahwa variabel", var_name, "tidak berdistribusi normal.")),
                    tags$p("Pertimbangkan: transformasi data, uji non-parametrik, atau metode robust.")
                )
            }

            # Add detailed results
            detailed_results <- tags$div(
                tags$hr(),
                tags$h6("Detail Hasil Uji:"),
                tags$ul(
                    lapply(1:nrow(results), function(i) {
                        test_name <- results$Test[i]
                        p_val <- results$P_Value[i]
                        conclusion <- results$Normal[i]

                        tags$li(
                            tags$strong(test_name), ": ",
                            "p-value = ", formatC(p_val, format = "e", digits = 3),
                            " → ", if (conclusion == "Ya") "Normal" else "Tidak Normal"
                        )
                    })
                )
            )

            tagList(interpretation, detailed_results)
        })

        # ========== HOMOGENEITY TESTS ==========

        observeEvent(input$run_homogeneity, {
            req(values$current_data, input$var_homogen, input$group_homogen)

            tryCatch(
                {
                    data_to_use <- values$current_data
                    var_data <- data_to_use[[input$var_homogen]]
                    group_data <- data_to_use[[input$group_homogen]]

                    # Remove missing values
                    complete_cases <- complete.cases(var_data, group_data)
                    var_data <- var_data[complete_cases]
                    group_data <- group_data[complete_cases]

                    if (length(unique(group_data)) < 2) {
                        showNotification("Need at least 2 groups for homogeneity testing.", type = "error")
                        return()
                    }

                    # Run selected homogeneity tests
                    test_results <- list()

                    if ("levene" %in% input$homogeneity_tests) {
                        if (requireNamespace("car", quietly = TRUE)) {
                            levene_result <- car::leveneTest(var_data ~ as.factor(group_data))
                            test_results$"Levene" <- data.frame(
                                Test = "Levene's Test",
                                Statistic = levene_result$`F value`[1],
                                DF = levene_result$`Df`[1],
                                P_Value = levene_result$`Pr(>F)`[1],
                                Homogeneous = ifelse(levene_result$`Pr(>F)`[1] > input$alpha_homogen, "Ya", "Tidak"),
                                stringsAsFactors = FALSE
                            )
                        }
                    }

                    if ("bartlett" %in% input$homogeneity_tests) {
                        bartlett_result <- bartlett.test(var_data ~ as.factor(group_data))
                        test_results$"Bartlett" <- data.frame(
                            Test = "Bartlett's Test",
                            Statistic = bartlett_result$statistic,
                            DF = bartlett_result$parameter,
                            P_Value = bartlett_result$p.value,
                            Homogeneous = ifelse(bartlett_result$p.value > input$alpha_homogen, "Ya", "Tidak"),
                            stringsAsFactors = FALSE
                        )
                    }

                    if ("fligner" %in% input$homogeneity_tests) {
                        fligner_result <- fligner.test(var_data ~ as.factor(group_data))
                        test_results$"Fligner" <- data.frame(
                            Test = "Fligner-Killeen Test",
                            Statistic = fligner_result$statistic,
                            DF = fligner_result$parameter,
                            P_Value = fligner_result$p.value,
                            Homogeneous = ifelse(fligner_result$p.value > input$alpha_homogen, "Ya", "Tidak"),
                            stringsAsFactors = FALSE
                        )
                    }

                    if ("brown" %in% input$homogeneity_tests) {
                        # Brown-Forsythe is Levene test with median
                        if (requireNamespace("car", quietly = TRUE)) {
                            brown_result <- car::leveneTest(var_data ~ as.factor(group_data), center = median)
                            test_results$"Brown-Forsythe" <- data.frame(
                                Test = "Brown-Forsythe Test",
                                Statistic = brown_result$`F value`[1],
                                DF = brown_result$`Df`[1],
                                P_Value = brown_result$`Pr(>F)`[1],
                                Homogeneous = ifelse(brown_result$`Pr(>F)`[1] > input$alpha_homogen, "Ya", "Tidak"),
                                stringsAsFactors = FALSE
                            )
                        }
                    }

                    # Combine results
                    if (length(test_results) > 0) {
                        final_results <- do.call(rbind, test_results)
                        rownames(final_results) <- NULL
                        assumption_results$homogeneity_results <- final_results

                        # Store data for visualization
                        assumption_results$homogeneity_data <- data.frame(
                            value = var_data,
                            group = as.factor(group_data)
                        )
                    } else {
                        showNotification("No homogeneity tests were successfully completed.", type = "warning")
                    }
                },
                error = function(e) {
                    showNotification(paste("Error in homogeneity testing:", e$message), type = "error")
                }
            )
        })

        # Homogeneity results table
        output$homogeneity_results_table <- DT::renderDT({
            req(assumption_results$homogeneity_results)

            DT::datatable(
                assumption_results$homogeneity_results,
                options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    dom = "ft"
                ),
                rownames = FALSE
            ) %>%
                DT::formatRound(columns = c("Statistic", "P_Value"), digits = 5) %>%
                DT::formatStyle(
                    "Homogeneous",
                    backgroundColor = DT::styleEqual(c("Ya", "Tidak"), c("lightgreen", "lightcoral"))
                )
        })

        # Homogeneity plots
        output$homogeneity_plots <- renderPlotly({
            req(assumption_results$homogeneity_data)

            homog_data <- assumption_results$homogeneity_data

            # Create boxplot
            p1 <- plot_ly(homog_data,
                y = ~value, x = ~group, type = "box",
                name = "Distribution by Group"
            ) %>%
                layout(
                    title = paste("Variance Distribution:", input$var_homogen, "by", input$group_homogen),
                    xaxis = list(title = input$group_homogen),
                    yaxis = list(title = input$var_homogen)
                )

            # Create variance plot
            variance_by_group <- aggregate(homog_data$value, by = list(homog_data$group), FUN = var, na.rm = TRUE)
            names(variance_by_group) <- c("Group", "Variance")

            p2 <- plot_ly(variance_by_group,
                x = ~Group, y = ~Variance, type = "bar",
                name = "Variance by Group", marker = list(color = "lightblue")
            ) %>%
                layout(
                    title = "Variance by Group",
                    xaxis = list(title = input$group_homogen),
                    yaxis = list(title = "Variance")
                )

            subplot(p1, p2, nrows = 1, shareY = FALSE, titleX = TRUE) %>%
                layout(title = "Homogeneity of Variance Assessment")
        })

        # Homogeneity interpretation
        output$homogeneity_interpretation <- renderUI({
            req(assumption_results$homogeneity_results)

            results <- assumption_results$homogeneity_results
            alpha <- input$alpha_homogen

            # Count homogeneous results
            homog_count <- sum(results$Homogeneous == "Ya", na.rm = TRUE)
            total_tests <- nrow(results)

            interpretation <- if (homog_count == total_tests) {
                tags$div(
                    class = "alert alert-success",
                    tags$h5(icon("check-circle"), "Kesimpulan: Varians Homogen"),
                    tags$p(paste("Semua", total_tests, "uji homogenitas menunjukkan varians yang homogen antar kelompok.")),
                    tags$p("Asumsi homoskedastisitas TERPENUHI. Anda dapat menggunakan ANOVA atau uji-t.")
                )
            } else if (homog_count > total_tests / 2) {
                tags$div(
                    class = "alert alert-warning",
                    tags$h5(icon("exclamation-triangle"), "Kesimpulan: Homogenitas Sebagian Terpenuhi"),
                    tags$p(paste(homog_count, "dari", total_tests, "uji menunjukkan homogenitas.")),
                    tags$p("Pertimbangkan transformasi data atau gunakan uji yang robust terhadap heteroskedastisitas.")
                )
            } else {
                tags$div(
                    class = "alert alert-danger",
                    tags$h5(icon("times-circle"), "Kesimpulan: Varians TIDAK Homogen"),
                    tags$p("Mayoritas uji menunjukkan varians yang tidak homogen antar kelompok."),
                    tags$p("Gunakan: Welch's t-test, transformasi data, atau metode non-parametrik.")
                )
            }

            # Add variance summary
            if (!is.null(assumption_results$homogeneity_data)) {
                var_summary <- aggregate(assumption_results$homogeneity_data$value,
                    by = list(assumption_results$homogeneity_data$group),
                    FUN = function(x) {
                        c(
                            mean = mean(x, na.rm = TRUE),
                            var = var(x, na.rm = TRUE),
                            n = length(x[!is.na(x)])
                        )
                    }
                )

                var_table <- do.call(rbind, var_summary$x)
                var_table <- as.data.frame(var_table)
                var_table$Group <- var_summary$Group.1
                var_table <- var_table[, c("Group", "mean", "var", "n")]

                detailed_results <- tags$div(
                    tags$hr(),
                    tags$h6("Ringkasan Statistik per Kelompok:"),
                    DT::DTOutput(session$ns("variance_summary_table"))
                )

                output$variance_summary_table <- DT::renderDT({
                    DT::datatable(
                        var_table,
                        options = list(pageLength = 5, dom = "t"),
                        rownames = FALSE,
                        colnames = c("Kelompok", "Rata-rata", "Varians", "N")
                    ) %>%
                        DT::formatRound(columns = c("mean", "var"), digits = 4)
                })
            } else {
                detailed_results <- tags$div()
            }

            tagList(interpretation, detailed_results)
        })

        # ========== INDEPENDENCE TESTS ==========

        observeEvent(input$run_independence, {
            req(values$current_data, input$var_indep1, input$var_indep2)

            tryCatch(
                {
                    data_to_use <- values$current_data
                    var1_data <- data_to_use[[input$var_indep1]]
                    var2_data <- data_to_use[[input$var_indep2]]

                    # Remove missing values
                    complete_cases <- complete.cases(var1_data, var2_data)
                    var1_data <- var1_data[complete_cases]
                    var2_data <- var2_data[complete_cases]

                    # Create contingency table
                    cont_table <- table(var1_data, var2_data)
                    assumption_results$contingency_table <- cont_table

                    # Run selected independence test
                    test_result <- switch(input$independence_test,
                        "chisq" = {
                            if (any(cont_table < 5)) {
                                showNotification("Warning: Some expected frequencies < 5. Consider Fisher's exact test.",
                                    type = "warning"
                                )
                            }
                            chisq.test(cont_table)
                        },
                        "fisher" = fisher.test(cont_table, simulate.p.value = TRUE),
                        "gtest" = {
                            # G-test implementation
                            observed <- as.vector(cont_table)
                            expected <- outer(rowSums(cont_table), colSums(cont_table), "*") / sum(cont_table)
                            expected <- as.vector(expected)
                            g_stat <- 2 * sum(observed * log(observed / expected), na.rm = TRUE)
                            df <- (nrow(cont_table) - 1) * (ncol(cont_table) - 1)
                            p_value <- 1 - pchisq(g_stat, df)

                            list(statistic = g_stat, parameter = df, p.value = p_value, method = "G-test")
                        }
                    )

                    assumption_results$independence_results <- test_result
                },
                error = function(e) {
                    showNotification(paste("Error in independence testing:", e$message), type = "error")
                }
            )
        })

        # Contingency table output
        output$contingency_table <- DT::renderDT({
            req(assumption_results$contingency_table)

            cont_table <- assumption_results$contingency_table
            cont_df <- as.data.frame.matrix(cont_table)
            cont_df$Variable1 <- rownames(cont_df)
            cont_df <- cont_df[, c(ncol(cont_df), 1:(ncol(cont_df) - 1))]

            DT::datatable(
                cont_df,
                options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    dom = "ft"
                ),
                rownames = FALSE,
                colnames = c(input$var_indep1, colnames(cont_table))
            )
        })

        # Independence plots
        output$independence_plots <- renderPlotly({
            req(assumption_results$contingency_table)

            cont_table <- assumption_results$contingency_table

            # Convert to data frame for plotting
            cont_df <- as.data.frame(cont_table)
            names(cont_df) <- c("Var1", "Var2", "Freq")

            # Create grouped bar chart
            plot_ly(cont_df, x = ~Var1, y = ~Freq, color = ~Var2, type = "bar") %>%
                layout(
                    title = paste("Contingency Table:", input$var_indep1, "vs", input$var_indep2),
                    xaxis = list(title = input$var_indep1),
                    yaxis = list(title = "Frequency"),
                    barmode = "group"
                )
        })

        # Independence results and interpretation
        output$independence_results <- renderText({
            req(assumption_results$independence_results)

            result <- assumption_results$independence_results

            paste(
                "Test:", result$method,
                "\nTest Statistic:", formatC(result$statistic, format = "f", digits = 4),
                "\nDegrees of Freedom:", result$parameter,
                "\nP-value:", formatC(result$p.value, format = "e", digits = 4)
            )
        })

        output$independence_interpretation <- renderUI({
            req(assumption_results$independence_results)

            result <- assumption_results$independence_results
            alpha <- input$alpha_indep
            is_independent <- result$p.value > alpha

            interpretation <- if (is_independent) {
                tags$div(
                    class = "alert alert-success",
                    tags$h5(icon("check-circle"), "Kesimpulan: Variabel Independen"),
                    tags$p(paste(
                        "P-value =", formatC(result$p.value, format = "e", digits = 3),
                        "> α =", alpha
                    )),
                    tags$p(paste("Tidak ada bukti ketergantungan antara", input$var_indep1, "dan", input$var_indep2, "."))
                )
            } else {
                tags$div(
                    class = "alert alert-danger",
                    tags$h5(icon("times-circle"), "Kesimpulan: Variabel TIDAK Independen"),
                    tags$p(paste(
                        "P-value =", formatC(result$p.value, format = "e", digits = 3),
                        "≤ α =", alpha
                    )),
                    tags$p(paste("Terdapat ketergantungan signifikan antara", input$var_indep1, "dan", input$var_indep2, "."))
                )
            }

            interpretation
        })

        # ========== COMPREHENSIVE SUMMARY ==========

        output$assumptions_summary_table <- DT::renderDT({
            # Create summary of all assumption tests
            summary_data <- data.frame(
                Assumption = character(0),
                Variable = character(0),
                Test_Method = character(0),
                Status = character(0),
                P_Value = numeric(0),
                Conclusion = character(0),
                stringsAsFactors = FALSE
            )

            # Add normality results
            if (!is.null(assumption_results$normality_results)) {
                normality_summary <- data.frame(
                    Assumption = "Normalitas",
                    Variable = input$var_normal,
                    Test_Method = assumption_results$normality_results$Test,
                    Status = assumption_results$normality_results$Normal,
                    P_Value = assumption_results$normality_results$P_Value,
                    Conclusion = ifelse(assumption_results$normality_results$Normal == "Ya",
                        "Terpenuhi", "Tidak Terpenuhi"
                    ),
                    stringsAsFactors = FALSE
                )
                summary_data <- rbind(summary_data, normality_summary)
            }

            # Add homogeneity results
            if (!is.null(assumption_results$homogeneity_results)) {
                homogeneity_summary <- data.frame(
                    Assumption = "Homogenitas",
                    Variable = paste(input$var_homogen, "by", input$group_homogen),
                    Test_Method = assumption_results$homogeneity_results$Test,
                    Status = assumption_results$homogeneity_results$Homogeneous,
                    P_Value = assumption_results$homogeneity_results$P_Value,
                    Conclusion = ifelse(assumption_results$homogeneity_results$Homogeneous == "Ya",
                        "Terpenuhi", "Tidak Terpenuhi"
                    ),
                    stringsAsFactors = FALSE
                )
                summary_data <- rbind(summary_data, homogeneity_summary)
            }

            # Add independence results
            if (!is.null(assumption_results$independence_results)) {
                independence_summary <- data.frame(
                    Assumption = "Independensi",
                    Variable = paste(input$var_indep1, "vs", input$var_indep2),
                    Test_Method = assumption_results$independence_results$method,
                    Status = ifelse(assumption_results$independence_results$p.value > input$alpha_indep, "Ya", "Tidak"),
                    P_Value = assumption_results$independence_results$p.value,
                    Conclusion = ifelse(assumption_results$independence_results$p.value > input$alpha_indep,
                        "Terpenuhi", "Tidak Terpenuhi"
                    ),
                    stringsAsFactors = FALSE
                )
                summary_data <- rbind(summary_data, independence_summary)
            }

            if (nrow(summary_data) == 0) {
                summary_data <- data.frame(
                    Assumption = "Belum ada uji yang dijalankan",
                    Variable = "-",
                    Test_Method = "-",
                    Status = "-",
                    P_Value = NA,
                    Conclusion = "Jalankan uji pada tab-tab sebelumnya",
                    stringsAsFactors = FALSE
                )
            }

            DT::datatable(
                summary_data,
                options = list(
                    pageLength = 15,
                    scrollX = TRUE,
                    dom = "ft"
                ),
                rownames = FALSE,
                colnames = c("Asumsi", "Variabel", "Metode Uji", "Status", "P-Value", "Kesimpulan")
            ) %>%
                DT::formatRound(columns = "P_Value", digits = 5) %>%
                DT::formatStyle(
                    "Conclusion",
                    backgroundColor = DT::styleEqual(
                        c("Terpenuhi", "Tidak Terpenuhi"),
                        c("lightgreen", "lightcoral")
                    )
                )
        })

        output$analysis_recommendations <- renderUI({
            recommendations <- list()

            # Check normality status
            if (!is.null(assumption_results$normality_results)) {
                normal_status <- assumption_results$normality_results$Normal
                if (all(normal_status == "Ya")) {
                    recommendations <- append(
                        recommendations,
                        "✅ Data berdistribusi normal → Gunakan uji parametrik (t-test, ANOVA, regresi linear)"
                    )
                } else {
                    recommendations <- append(
                        recommendations,
                        "⚠️ Data tidak berdistribusi normal → Pertimbangkan transformasi atau uji non-parametrik"
                    )
                }
            }

            # Check homogeneity status
            if (!is.null(assumption_results$homogeneity_results)) {
                homog_status <- assumption_results$homogeneity_results$Homogeneous
                if (all(homog_status == "Ya")) {
                    recommendations <- append(
                        recommendations,
                        "✅ Varians homogen → ANOVA klasik dan pooled t-test dapat digunakan"
                    )
                } else {
                    recommendations <- append(
                        recommendations,
                        "⚠️ Varians tidak homogen → Gunakan Welch's test atau transformasi data"
                    )
                }
            }

            # Check independence status
            if (!is.null(assumption_results$independence_results)) {
                indep_status <- assumption_results$independence_results$p.value > input$alpha_indep
                if (indep_status) {
                    recommendations <- append(
                        recommendations,
                        "✅ Variabel independen → Asumsi independensi terpenuhi"
                    )
                } else {
                    recommendations <- append(
                        recommendations,
                        "⚠️ Variabel tidak independen → Ada asosiasi antara variabel"
                    )
                }
            }

            if (length(recommendations) == 0) {
                recommendations <- "Jalankan uji asumsi terlebih dahulu untuk mendapatkan rekomendasi analisis."
            }

            tags$div(
                class = "alert alert-info",
                tags$h5(icon("lightbulb"), "Rekomendasi Analisis:"),
                tags$ul(
                    lapply(recommendations, function(rec) tags$li(rec))
                )
            )
        })

        # ========== DOWNLOAD HANDLERS ==========

        output$download_plots_png <- downloadHandler(
            filename = function() {
                paste0("uji_asumsi_plots_", Sys.Date(), ".png")
            },
            content = function(file) {
                # This is a placeholder - in reality you'd combine all plots
                showNotification("PNG download functionality will be implemented.", type = "message")
            }
        )

        output$download_plots_jpg <- downloadHandler(
            filename = function() {
                paste0("uji_asumsi_plots_", Sys.Date(), ".jpg")
            },
            content = function(file) {
                showNotification("JPEG download functionality will be implemented.", type = "message")
            }
        )

        output$download_results_csv <- downloadHandler(
            filename = function() {
                paste0("uji_asumsi_results_", Sys.Date(), ".csv")
            },
            content = function(file) {
                # Combine all results into one CSV
                all_results <- list()

                if (!is.null(assumption_results$normality_results)) {
                    all_results$normality <- assumption_results$normality_results
                }

                if (!is.null(assumption_results$homogeneity_results)) {
                    all_results$homogeneity <- assumption_results$homogeneity_results
                }

                if (length(all_results) > 0) {
                    # For simplicity, save the first available result
                    write.csv(all_results[[1]], file, row.names = FALSE)
                } else {
                    # Create empty file with message
                    write.csv(data.frame(Message = "No results available"), file, row.names = FALSE)
                }
            }
        )

        output$download_interpretation <- downloadHandler(
            filename = function() {
                paste0("interpretasi_asumsi_", Sys.Date(), ".docx")
            },
            content = function(file) {
                showNotification("DOCX interpretation download will be implemented.", type = "message")
            }
        )

        output$download_report_pdf <- downloadHandler(
            filename = function() {
                paste0("laporan_uji_asumsi_", Sys.Date(), ".pdf")
            },
            content = function(file) {
                showNotification("PDF report download will be implemented.", type = "message")
            }
        )

        output$download_report_word <- downloadHandler(
            filename = function() {
                paste0("laporan_uji_asumsi_", Sys.Date(), ".docx")
            },
            content = function(file) {
                showNotification("Word report download will be implemented.", type = "message")
            }
        )

        # Return reactive values for other modules
        return(assumption_results)
    })
}
