# Uji ANOVA Server Module
# Server logic for ANOVA testing

#' Uji ANOVA Server Module
#'
#' Server logic for ANOVA tests
#'
#' @param id Module ID for namespacing
#' @param values Reactive values object containing shared data
uji_anova_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        # Update variable choices - reactive to data structure changes
        observe({
            # Create reactive dependency on data and data update counter
            req(values$sovi_data)
            data_counter <- values$data_update_counter # This creates a reactive dependency

            # Debug: Check what data we actually receive
            cat("ANOVA DEBUG: Observer triggered with counter:", data_counter, "\n")
            cat("ANOVA DEBUG: Data dimensions:", nrow(values$sovi_data), "x", ncol(values$sovi_data), "\n")
            cat("ANOVA DEBUG: All column names:", paste(names(values$sovi_data), collapse = ", "), "\n")

            # Check specifically for categorical columns
            cat_columns <- names(values$sovi_data)[sapply(values$sovi_data, function(x) is.character(x) || is.factor(x))]
            cat("ANOVA DEBUG: Categorical columns found:", paste(cat_columns, collapse = ", "), "\n")

            numeric_choices <- get_variable_choices(values$sovi_data, "numeric")
            categorical_choices <- get_variable_choices(values$sovi_data, "categorical")

            # Debug logging to console
            cat("ANOVA: Updating variable choices (counter:", data_counter, ")...\n")
            cat("Categorical choices:", names(categorical_choices), "\n")
            cat("Categorical choice values:", paste(categorical_choices, collapse = ", "), "\n")

            updateSelectInput(session, "dep_var", choices = numeric_choices)
            updateSelectInput(session, "factor1", choices = categorical_choices)
            updateSelectInput(session, "factor2", choices = categorical_choices)
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

                        # Store all necessary data for consistent rendering
                        anova_result(list(
                            model = model,
                            summary = result,
                            type = "one_way",
                            dep_var = input$dep_var,
                            factor1 = input$factor1,
                            data = values$sovi_data
                        ))

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
                # Two-way ANOVA
                req(input$factor2)

                # Check if second factor has enough levels
                factor2_levels <- unique(values$sovi_data[[input$factor2]])
                if (length(factor2_levels) < 2) {
                    showNotification("Faktor 2 harus memiliki minimal 2 level.", type = "error")
                    return()
                }

                # Check if factors are different
                if (input$factor1 == input$factor2) {
                    showNotification("Faktor 1 dan Faktor 2 harus berbeda.", type = "error")
                    return()
                }

                # Build formula based on interaction option
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

                        # Store all necessary data for consistent rendering
                        anova_result(list(
                            model = model,
                            summary = result,
                            type = "two_way",
                            interaction = input$interaction,
                            dep_var = input$dep_var,
                            factor1 = input$factor1,
                            factor2 = input$factor2,
                            data = values$sovi_data
                        ))

                        # Post-hoc test for two-way ANOVA
                        # Perform post-hoc for main effects only if significant
                        anova_table <- result[[1]]

                        # Check significance of factors
                        factor1_significant <- FALSE
                        factor2_significant <- FALSE

                        if (!is.na(anova_table$`Pr(>F)`[1]) && anova_table$`Pr(>F)`[1] < 0.05) {
                            factor1_significant <- TRUE
                        }
                        if (nrow(anova_table) >= 2 && !is.na(anova_table$`Pr(>F)`[2]) && anova_table$`Pr(>F)`[2] < 0.05) {
                            factor2_significant <- TRUE
                        }

                        # Perform post-hoc tests for significant factors
                        if (factor1_significant && length(factor1_levels) > 2) {
                            # Create single-factor model for factor1 post-hoc
                            factor1_formula <- as.formula(paste(input$dep_var, "~", input$factor1))
                            factor1_model <- aov(factor1_formula, data = values$sovi_data)
                            posthoc1 <- TukeyHSD(factor1_model)

                            # Store post-hoc results
                            posthoc_result(list(
                                factor1 = posthoc1,
                                factor1_name = input$factor1,
                                factor2_name = input$factor2
                            ))
                        }
                    },
                    error = function(e) {
                        showNotification(paste("Error dalam Two-Way ANOVA:", e$message), type = "error")
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
                cat("Dependent Variable:", result$dep_var, "\n")
                cat("Factor:", result$factor1, "\n\n")
            } else if (result$type == "two_way") {
                cat("Two-Way ANOVA\n")
                cat("Dependent Variable:", result$dep_var, "\n")
                cat("Factor 1:", result$factor1, "\n")
                cat("Factor 2:", result$factor2, "\n")
                if (result$interaction) {
                    cat("Interaction:", paste(result$factor1, "*", result$factor2), "\n\n")
                } else {
                    cat("Model: Main effects only (no interaction)\n\n")
                }
            }

            print(result$summary)
        })

        # ANOVA interpretation using helper function
        output$interpretation <- renderUI({
            req(anova_result())

            result <- anova_result()

            # Use the interpretation helper function
            interpretation_text <- interpret_anova(
                anova_result = result$summary,
                alpha = 0.05,
                type = result$type
            )

            # Convert to HTML with proper formatting
            interpretation_html <- gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", interpretation_text)
            interpretation_html <- gsub("\\n", "<br>", interpretation_html)

            HTML(paste0(
                "<div style='padding: 15px; background-color: #f8f9fa; border-left: 4px solid #007bff; margin: 10px 0;'>",
                interpretation_html,
                "</div>"
            ))
        })

        # Post-hoc test results
        output$posthoc_results <- renderPrint({
            req(posthoc_result())

            result <- posthoc_result()

            cat("Post-Hoc Test Results (Tukey HSD)\n")
            cat("=================================\n\n")

            print(result)
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

            result <- anova_result()

            # Use stored data and variable names instead of current inputs
            plot_data <- result$data
            dep_var <- result$dep_var
            factor1 <- result$factor1

            if (result$type == "one_way") {
                # Box plot for one-way ANOVA
                p <- ggplot(plot_data, aes_string(x = factor1, y = dep_var)) +
                    geom_boxplot(fill = "lightblue", alpha = 0.7) +
                    geom_jitter(width = 0.2, alpha = 0.5) +
                    labs(
                        title = paste("Box Plot:", dep_var, "by", factor1),
                        x = factor1,
                        y = dep_var
                    ) +
                    theme_minimal()
            } else if (result$type == "two_way") {
                # Get factor2 from stored results
                factor2 <- result$factor2

                # Interaction plot for two-way ANOVA
                if (result$interaction) {
                    # Interaction plot
                    p <- ggplot(plot_data, aes_string(
                        x = factor1, y = dep_var,
                        color = factor2
                    )) +
                        stat_summary(fun = mean, geom = "point", size = 3) +
                        stat_summary(fun = mean, geom = "line", aes_string(group = factor2), size = 1) +
                        labs(
                            title = paste("Interaction Plot:", dep_var, "by", factor1, "and", factor2),
                            x = factor1,
                            y = paste("Mean", dep_var),
                            color = factor2
                        ) +
                        theme_minimal()
                } else {
                    # Side-by-side box plots
                    p <- ggplot(plot_data, aes_string(
                        x = factor1, y = dep_var,
                        fill = factor2
                    )) +
                        geom_boxplot(alpha = 0.7, position = position_dodge(0.8)) +
                        labs(
                            title = paste("Box Plot:", dep_var, "by", factor1, "and", factor2),
                            x = factor1,
                            y = dep_var,
                            fill = factor2
                        ) +
                        theme_minimal()
                }
            }

            plotly::ggplotly(p, tooltip = c("x", "y"))
        })

        # Enhanced Download Handlers

        # 1. Individual interpretation download (Word)
        output$download_interpretation <- downloadHandler(
            filename = function() {
                paste0("interpretasi_anova_", Sys.Date(), ".docx")
            },
            content = function(file) {
                req(anova_result())

                result <- anova_result()
                anova_table <- result$summary[[1]]

                if (result$type == "one_way") {
                    p_value <- anova_table$`Pr(>F)`[1]
                    f_stat <- anova_table$`F value`[1]

                    # Calculate effect size
                    ss_factor <- anova_table$`Sum Sq`[1]
                    ss_total <- sum(anova_table$`Sum Sq`, na.rm = TRUE)
                    eta_squared <- ss_factor / ss_total

                    effect_size_interpretation <- if (eta_squared < 0.01) {
                        "sangat kecil (< 1%)"
                    } else if (eta_squared < 0.06) {
                        "kecil (1-6%)"
                    } else if (eta_squared < 0.14) {
                        "sedang (6-14%)"
                    } else {
                        "besar (> 14%)"
                    }

                    group_counts <- table(values$sovi_data[[input$factor1]])
                    n_groups <- length(group_counts)

                    base_interpretation <- interpret_p_value(
                        p_value,
                        h0 = paste("Semua rata-rata grup", input$factor1, "sama"),
                        h1 = paste("Minimal ada satu rata-rata grup", input$factor1, "yang berbeda")
                    )

                    confidence_level <- ifelse(p_value < 0.001, "sangat tinggi",
                        ifelse(p_value < 0.01, "tinggi",
                            ifelse(p_value < 0.05, "cukup", "rendah")
                        )
                    )

                    practical_recommendation <- if (p_value < 0.05) {
                        if (eta_squared > 0.06) {
                            paste(
                                "Perbedaan antar grup memiliki signifikansi praktis yang berarti.",
                                "Disarankan untuk melakukan analisis lebih lanjut pada grup-grup spesifik",
                                "yang berbeda menggunakan uji post-hoc."
                            )
                        } else {
                            paste(
                                "Meskipun secara statistik signifikan, ukuran efek relatif kecil.",
                                "Pertimbangkan relevansi praktis dari perbedaan yang ditemukan."
                            )
                        }
                    } else {
                        paste(
                            "Tidak ada bukti yang cukup untuk menyimpulkan adanya perbedaan antar grup.",
                            "Pastikan ukuran sampel memadai dan pertimbangkan faktor lain yang mungkin mempengaruhi."
                        )
                    }

                    # Create Word document
                    doc <- officer::read_docx()

                    doc <- doc %>%
                        officer::body_add_par("INTERPRETASI HASIL ANOVA", style = "heading 1") %>%
                        officer::body_add_par(paste("Tanggal Analisis:", Sys.Date()), style = "Normal") %>%
                        officer::body_add_par("", style = "Normal") %>%
                        officer::body_add_par("KESIMPULAN STATISTIK", style = "heading 2") %>%
                        officer::body_add_par(base_interpretation, style = "Normal") %>%
                        officer::body_add_par("", style = "Normal") %>%
                        officer::body_add_par("DETAIL ANALISIS", style = "heading 2") %>%
                        officer::body_add_par(paste("• F-statistic:", format_number(f_stat, 4)), style = "Normal") %>%
                        officer::body_add_par(paste("• Derajat kebebasan:", anova_table$Df[1], "dan", anova_table$Df[2]), style = "Normal") %>%
                        officer::body_add_par(paste("• Tingkat kepercayaan:", confidence_level), style = "Normal") %>%
                        officer::body_add_par(paste(
                            "• Ukuran efek (η²):", format_number(eta_squared, 4),
                            paste0("(", effect_size_interpretation, ")")
                        ), style = "Normal") %>%
                        officer::body_add_par(paste("• Jumlah grup:", n_groups), style = "Normal") %>%
                        officer::body_add_par(paste("• Total observasi:", sum(group_counts)), style = "Normal") %>%
                        officer::body_add_par("", style = "Normal") %>%
                        officer::body_add_par("REKOMENDASI PRAKTIS", style = "heading 2") %>%
                        officer::body_add_par(practical_recommendation, style = "Normal")

                    if (p_value < 0.05 && n_groups > 2) {
                        doc <- doc %>%
                            officer::body_add_par("", style = "Normal") %>%
                            officer::body_add_par("LANGKAH SELANJUTNYA", style = "heading 2") %>%
                            officer::body_add_par(paste(
                                "Karena ANOVA menunjukkan perbedaan signifikan dan terdapat lebih dari 2 grup,",
                                "lakukan uji post-hoc untuk mengidentifikasi",
                                "pasangan grup mana yang berbeda secara signifikan."
                            ), style = "Normal")
                    }

                    print(doc, target = file)
                }
            }
        )

        # JPG plot download
        output$download_plot_jpg <- downloadHandler(
            filename = function() {
                paste0("plot_anova_", Sys.Date(), ".jpg")
            },
            content = function(file) {
                req(anova_result())

                result <- anova_result()

                # Use stored data and variable names instead of current inputs
                plot_data <- result$data
                dep_var <- result$dep_var
                factor1 <- result$factor1

                if (result$type == "one_way") {
                    # Box plot for one-way ANOVA
                    p <- ggplot(plot_data, aes_string(x = factor1, y = dep_var)) +
                        geom_boxplot(fill = "lightblue", alpha = 0.7) +
                        geom_jitter(width = 0.2, alpha = 0.5) +
                        labs(
                            title = paste("Box Plot:", dep_var, "by", factor1),
                            x = factor1,
                            y = dep_var
                        ) +
                        theme_minimal()
                } else if (result$type == "two_way") {
                    # Get factor2 from stored results
                    factor2 <- result$factor2

                    # Interaction plot for two-way ANOVA
                    if (result$interaction) {
                        # Interaction plot
                        p <- ggplot(plot_data, aes_string(
                            x = factor1, y = dep_var,
                            color = factor2
                        )) +
                            stat_summary(fun = mean, geom = "point", size = 3) +
                            stat_summary(fun = mean, geom = "line", aes_string(group = factor2), size = 1) +
                            labs(
                                title = paste("Interaction Plot:", dep_var, "by", factor1, "and", factor2),
                                x = factor1,
                                y = paste("Mean", dep_var),
                                color = factor2
                            ) +
                            theme_minimal()
                    } else {
                        # Side-by-side box plots
                        p <- ggplot(plot_data, aes_string(
                            x = factor1, y = dep_var,
                            fill = factor2
                        )) +
                            geom_boxplot(alpha = 0.7, position = position_dodge(0.8)) +
                            labs(
                                title = paste("Box Plot:", dep_var, "by", factor1, "and", factor2),
                                x = factor1,
                                y = dep_var,
                                fill = factor2
                            ) +
                            theme_minimal()
                    }
                }

                ggsave(file, p, width = 12, height = 8, dpi = 300, device = "jpeg")
            }
        )

        # 2. Comprehensive PDF report
        output$download_report_pdf <- downloadHandler(
            filename = function() {
                paste0("laporan_anova_", Sys.Date(), ".pdf")
            },
            content = function(file) {
                req(anova_result())

                result <- anova_result()
                anova_table <- result$summary[[1]]

                # Prepare parameters for R Markdown template
                params <- list(
                    data = result$data,
                    anova_result = result,
                    anova_table = anova_table,
                    dep_var = result$dep_var,
                    factor1 = result$factor1,
                    factor2 = result$factor2,
                    anova_type = result$type,
                    interaction = result$interaction,
                    posthoc_result = posthoc_result(),
                    analysis_date = Sys.Date(),
                    interpretation = interpret_anova(result$summary, alpha = 0.05, type = result$type)
                )

                # Render the R Markdown template
                tryCatch(
                    {
                        rmarkdown::render(
                            input = "reports/laporan_anova.Rmd",
                            output_file = file,
                            output_format = "pdf_document",
                            params = params,
                            envir = new.env(parent = globalenv()),
                            quiet = TRUE
                        )
                    },
                    error = function(e) {
                        # If PDF generation fails, create a simple error document
                        writeLines(paste("Error generating PDF report:", e$message), file)
                        showNotification("PDF generation failed. Please try the Word format.", type = "error")
                    }
                )
            }
        )

        # 3. Comprehensive Word report
        output$download_report_word <- downloadHandler(
            filename = function() {
                paste0("laporan_anova_", Sys.Date(), ".docx")
            },
            content = function(file) {
                req(anova_result())

                result <- anova_result()
                anova_table <- result$summary[[1]]

                # Prepare parameters for R Markdown template
                params <- list(
                    data = result$data,
                    anova_result = result,
                    anova_table = anova_table,
                    dep_var = result$dep_var,
                    factor1 = result$factor1,
                    factor2 = result$factor2,
                    anova_type = result$type,
                    interaction = result$interaction,
                    posthoc_result = posthoc_result(),
                    analysis_date = Sys.Date(),
                    interpretation = interpret_anova(result$summary, alpha = 0.05, type = result$type)
                )

                # Create temporary Rmd file for Word output
                temp_rmd <- tempfile(fileext = ".Rmd")
                file.copy("reports/laporan_anova.Rmd", temp_rmd)

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
