# Uji Proporsi & Varians Server Module
# Server logic for proportion and variance testing

#' Uji Proporsi & Varians Server Module
#'
#' Server logic for proportion and variance tests
#'
#' @param id Module ID for namespacing
#' @param values Reactive values object containing shared data
uji_prop_var_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        # Update variable choices - reactive to data structure changes
        observe({
            # Create reactive dependency on data structure
            req(values$sovi_data)
            data_structure <- list(
                nrow = nrow(values$sovi_data),
                ncol = ncol(values$sovi_data), 
                column_names = names(values$sovi_data)
            )
            
            numeric_choices <- get_variable_choices(values$sovi_data, "numeric")
            updateSelectInput(session, "var_variance", choices = numeric_choices, selected = numeric_choices[1])
        })

        # Reactive values
        test_result <- reactiveVal(NULL)

        # Clear test result when test choice changes to prevent errors
        observeEvent(input$test_choice, {
            test_result(NULL)
        })

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
            
            # Validate that result matches the current test type
            tryCatch({
                if (input$test_choice == "proportion") {
                    # Validate proportion test result structure
                    if (is.null(result$conf.int) || is.null(result$statistic) || is.null(result$parameter)) {
                        cat("Error: Invalid test result for proportion test. Please run the test again.\n")
                        return()
                    }
                    
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
                    # Validate variance test result structure
                    if (is.null(result$sample.var) || is.null(result$data.name) || is.null(result$null.value)) {
                        cat("Error: Invalid test result for variance test. Please run the test again.\n")
                        return()
                    }
                    
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
            }, error = function(e) {
                cat("Error displaying results: ", e$message, "\n")
                cat("Please run the test again after changing the test type.\n")
            })
        })

        # Generate interpretation using helper functions
        output$interpretation <- renderUI({
            req(test_result())

            result <- test_result()
            
            # Use appropriate interpretation helper function with error handling
            interpretation_text <- tryCatch({
                if (input$test_choice == "proportion") {
                    # Validate proportion result before interpretation
                    if (is.null(result$estimate) || is.null(result$p.value)) {
                        return("**Error:** Invalid test result. Please run the proportion test again.")
                    }
                    interpret_prop_test(result, alpha = 0.05)
                } else {
                    # Validate variance result before interpretation
                    if (is.null(result$sample.var) || is.null(result$p.value)) {
                        return("**Error:** Invalid test result. Please run the variance test again.")
                    }
                    interpret_var_test(result, alpha = 0.05)
                }
            }, error = function(e) {
                paste("**Error generating interpretation:** Please run the", 
                      ifelse(input$test_choice == "proportion", "proportion", "variance"), 
                      "test again.")
            })

            # Convert to HTML with proper formatting
            interpretation_html <- gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", interpretation_text)
            interpretation_html <- gsub("\\n", "<br>", interpretation_html)

            HTML(paste0("<div style='padding: 15px; background-color: #f8f9fa; border-left: 4px solid #007bff; margin: 10px 0;'>",
                       interpretation_html,
                       "</div>"))
        })

        # Download interpretation as Word document
        output$download_interpretation <- downloadHandler(
            filename = function() {
                test_name <- ifelse(input$test_choice == "proportion", "proporsi", "varians")
                paste0("interpretasi_uji_", test_name, "_", Sys.Date(), ".docx")
            },
            content = function(file) {
                req(test_result())

                result <- test_result()
                test_title <- ifelse(input$test_choice == "proportion",
                    "Uji Proporsi Satu Sampel",
                    "Uji Varians Satu Sampel"
                )

                # Generate interpretation text
                interpretation_text <- tryCatch({
                    if (input$test_choice == "proportion") {
                        interpret_prop_test(result, alpha = 0.05)
                    } else {
                        interpret_var_test(result, alpha = 0.05)
                    }
                }, error = function(e) {
                    paste("Error generating interpretation:", e$message)
                })

                # Create Word document using officer
                doc <- officer::read_docx()
                doc <- officer::body_add_par(doc, "NusaStat Dashboard", style = "heading 1")
                doc <- officer::body_add_par(doc, paste("Interpretasi", test_title), style = "heading 2")
                doc <- officer::body_add_par(doc, paste("Tanggal:", format(Sys.Date(), "%d %B %Y")))
                doc <- officer::body_add_par(doc, "")
                doc <- officer::body_add_par(doc, interpretation_text)

                print(doc, target = file)
            }
        )

        # Download report as PDF
        output$download_report_pdf <- downloadHandler(
            filename = function() {
                test_name <- ifelse(input$test_choice == "proportion", "uji_proporsi", "uji_varians")
                paste0("laporan_", test_name, "_", Sys.Date(), ".pdf")
            },
            content = function(file) {
                temp_rmd <- tempfile(fileext = ".Rmd")

                test_title <- ifelse(input$test_choice == "proportion",
                    "Uji Proporsi Satu Sampel",
                    "Uji Varians Satu Sampel"
                )

                rmd_content <- paste0('---
title: "', test_title, ' - NusaStat Dashboard"
author: "NusaStat Dashboard"
date: "`r format(Sys.Date(), \'%d %B %Y\')`"
output:
  pdf_document:
    latex_engine: xelatex
    keep_tex: false
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
```

# ', test_title, '

## Informasi Analisis

**Jenis Uji:** ', test_title, '

**Tanggal Analisis:** `r format(Sys.Date(), \'%d %B %Y\')`

**Sumber Data:** SUSENAS 2017, BPS-Statistics Indonesia

---

## Parameter Uji

Parameter uji yang digunakan dalam analisis.

---

## Hasil Uji Statistik

Hasil perhitungan statistik uji dan interpretasinya.

---

## Interpretasi dan Kesimpulan

Berdasarkan hasil uji, dapat disimpulkan apakah hipotesis nol ditolak atau tidak pada tingkat signifikansi yang ditentukan.
')

                writeLines(rmd_content, temp_rmd)
                
                # Render using the correct pattern to avoid pandoc error
                tryCatch({
                    output_path <- rmarkdown::render(input = temp_rmd, output_format = "pdf_document", quiet = TRUE)
                    file.copy(output_path, file)
                }, error = function(e) {
                    # If PDF generation fails, create a simple error document
                    writeLines(paste("Error generating PDF report:", e$message), file)
                    showNotification("PDF generation failed. Please try the Word format.", type = "error")
                })
            }
        )

        # Download report as Word document
        output$download_report_word <- downloadHandler(
            filename = function() {
                test_name <- ifelse(input$test_choice == "proportion", "uji_proporsi", "uji_varians")
                paste0("laporan_", test_name, "_", Sys.Date(), ".docx")
            },
            content = function(file) {
                temp_rmd <- tempfile(fileext = ".Rmd")

                test_title <- ifelse(input$test_choice == "proportion",
                    "Uji Proporsi Satu Sampel",
                    "Uji Varians Satu Sampel"
                )

                rmd_content <- paste0('---
title: "', test_title, ' - NusaStat Dashboard"
author: "NusaStat Dashboard"
date: "`r format(Sys.Date(), \'%d %B %Y\')`"
output:
  word_document:
    reference_docx: NULL
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
```

# ', test_title, '

## Informasi Analisis

**Jenis Uji:** ', test_title, '

**Tanggal Analisis:** `r format(Sys.Date(), \'%d %B %Y\')`

**Sumber Data:** SUSENAS 2017, BPS-Statistics Indonesia

---

## Parameter Uji

Parameter uji yang digunakan dalam analisis.

---

## Hasil Uji Statistik

Hasil perhitungan statistik uji dan interpretasinya.

---

## Interpretasi dan Kesimpulan

Berdasarkan hasil uji, dapat disimpulkan apakah hipotesis nol ditolak atau tidak pada tingkat signifikansi yang ditentukan.
')

                writeLines(rmd_content, temp_rmd)
                
                # Render using the correct pattern to avoid pandoc error
                output_path <- rmarkdown::render(input = temp_rmd, output_format = "word_document", quiet = TRUE)
                file.copy(output_path, file)
            }
        )

        # Download handler (keeping the existing one for compatibility)
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
                
                # Render using the correct pattern to avoid pandoc error
                tryCatch({
                    output_path <- rmarkdown::render(input = temp_rmd, output_format = "pdf_document", quiet = TRUE)
                    file.copy(output_path, file)
                }, error = function(e) {
                    # If PDF generation fails, create a simple error document
                    writeLines(paste("Error generating PDF report:", e$message), file)
                    showNotification("PDF generation failed. Please try the Word format.", type = "error")
                })
            }
        )
    })
}
