# Uji Rata Server Module
# Server logic for mean difference testing

#' Uji Rata Server Module
#'
#' Server logic for mean difference tests (t-tests)
#'
#' @param id Module ID for namespacing
#' @param values Reactive values object containing shared data
uji_rata_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        # Update variable choices
        observe({
            if (!is.null(values$sovi_data)) {
                numeric_vars <- get_numeric_columns(values$sovi_data)
                categorical_vars <- get_categorical_columns(values$sovi_data)

                updateSelectInput(session, "var_one", choices = numeric_vars)
                updateSelectInput(session, "var_two", choices = numeric_vars)
                updateSelectInput(session, "group_two", choices = categorical_vars)
            }
        })

        # Reactive values
        test_result <- reactiveVal(NULL)

        # Run test
        observeEvent(input$run_test, {
            if (!validate_data(values$sovi_data, "Data SOVI")) {
                return()
            }

            if (input$test_type == "one_sample") {
                req(input$var_one, input$mu_test)

                var_data <- values$sovi_data[[input$var_one]]
                var_data <- var_data[!is.na(var_data)]

                if (length(var_data) < 2) {
                    showNotification("Data tidak cukup untuk uji t.", type = "error")
                    return()
                }

                result <- t.test(var_data, mu = input$mu_test, alternative = input$alternative)
                test_result(result)
            } else if (input$test_type == "two_sample") {
                req(input$var_two, input$group_two)

                group_var <- values$sovi_data[[input$group_two]]
                unique_groups <- unique(group_var)

                if (length(unique_groups) != 2) {
                    showNotification("Variabel grup harus memiliki tepat 2 kategori.", type = "error")
                    return()
                }

                var_data <- values$sovi_data[[input$var_two]]

                # Split data by group
                group1_data <- var_data[group_var == unique_groups[1]]
                group2_data <- var_data[group_var == unique_groups[2]]

                # Remove NA values
                group1_data <- group1_data[!is.na(group1_data)]
                group2_data <- group2_data[!is.na(group2_data)]

                if (length(group1_data) < 2 || length(group2_data) < 2) {
                    showNotification("Setiap grup harus memiliki minimal 2 observasi.", type = "error")
                    return()
                }

                result <- t.test(group1_data, group2_data, alternative = input$alternative)
                test_result(result)
            }

            showNotification("Uji berhasil dilakukan!", type = "message")
        })

        # Display test results
        output$test_results <- renderPrint({
            req(test_result())
            test_result()
        })

        # Generate interpretation
        output$interpretation <- renderUI({
            req(test_result())

            result <- test_result()
            p_value <- result$p.value

            if (input$test_type == "one_sample") {
                h0 <- paste("Rata-rata populasi =", input$mu_test)
                h1 <- switch(input$alternative,
                    "two.sided" = paste("Rata-rata populasi ≠", input$mu_test),
                    "greater" = paste("Rata-rata populasi >", input$mu_test),
                    "less" = paste("Rata-rata populasi <", input$mu_test)
                )
            } else {
                h0 <- "Rata-rata kedua grup sama"
                h1 <- switch(input$alternative,
                    "two.sided" = "Rata-rata kedua grup berbeda",
                    "greater" = "Rata-rata grup 1 > rata-rata grup 2",
                    "less" = "Rata-rata grup 1 < rata-rata grup 2"
                )
            }

            interpretation <- interpret_p_value(p_value, h0 = h0, h1 = h1)

            tagList(
                h4("Interpretasi Hasil Uji:"),
                p(interpretation),
                p(paste("Nilai t-statistik:", format_number(result$statistic))),
                p(paste("Derajat kebebasan:", result$parameter)),
                p(paste(
                    "Interval kepercayaan 95%:",
                    format_number(result$conf.int[1]), "hingga",
                    format_number(result$conf.int[2])
                ))
            )
        })

        # Download handler
        output$download_results <- downloadHandler(
            filename = function() {
                paste0("uji_beda_rata_", Sys.Date(), ".pdf")
            },
            content = function(file) {
                temp_rmd <- tempfile(fileext = ".Rmd")

                rmd_content <- '---
title: "Hasil Uji Beda Rata-Rata"
output: pdf_document
date: "`r Sys.Date()`"
---

# Uji Beda Rata-Rata

Hasil uji statistik untuk perbedaan rata-rata.

## Interpretasi

Berdasarkan hasil uji t, dapat disimpulkan apakah terdapat perbedaan signifikan.
'

                writeLines(rmd_content, temp_rmd)
                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )
    })
}
