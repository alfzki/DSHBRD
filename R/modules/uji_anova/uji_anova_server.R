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
            }

            do.call(tagList, interpretations)
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
