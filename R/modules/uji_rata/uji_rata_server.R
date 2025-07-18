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
        # Update variable choices - reactive to data structure changes
        observe({
            # Create reactive dependency on data and data update counter
            req(values$sovi_data)
            data_counter <- values$data_update_counter  # This creates a reactive dependency
            
            numeric_choices <- get_variable_choices(values$sovi_data, "numeric")
            categorical_choices <- get_variable_choices(values$sovi_data, "categorical")

            updateSelectInput(session, "var_one", choices = numeric_choices)
            updateSelectInput(session, "var_two", choices = numeric_choices)
            updateSelectInput(session, "group_two", choices = categorical_choices)
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

        # Generate ENHANCED DYNAMIC interpretation using helper function
        output$interpretation <- renderUI({
            req(test_result())

            result <- test_result()
            test_type <- input$test_type
            
            # Use the interpretation helper function
            interpretation_text <- interpret_ttest(
                test_result = result,
                alpha = 0.05,
                test_type = test_type
            )

            # Convert to HTML with proper formatting
            interpretation_html <- gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", interpretation_text)
            interpretation_html <- gsub("\\n", "<br>", interpretation_html)

            HTML(paste0("<div style='padding: 15px; background-color: #f8f9fa; border-left: 4px solid #007bff; margin: 10px 0;'>",
                       interpretation_html,
                       "</div>"))
        })

        # Individual interpretation download using officer
        output$download_interpretation <- downloadHandler(
            filename = function() {
                paste0("interpretasi_uji_rata_", Sys.Date(), ".docx")
            },
            content = function(file) {
                req(test_result())

                result <- test_result()
                p_value <- result$p.value

                # Generate interpretation text
                test_type_text <- if (input$test_type == "one_sample") "Uji t Satu Sampel" else "Uji t Dua Sampel"

                interpretation_text <- paste(
                    "Interpretasi", test_type_text, "\n\n",
                    "Hasil Uji:\n",
                    "- t-statistik:", format_number(result$statistic, 4), "\n",
                    "- p-value:", format_number(p_value, 6), "\n",
                    "- Derajat kebebasan:", result$parameter, "\n",
                    "- Interval kepercayaan 95%: [", format_number(result$conf.int[1], 4),
                    ", ", format_number(result$conf.int[2], 4), "]\n\n",
                    "Kesimpulan:\n",
                    if (p_value < 0.05) {
                        "Pada tingkat signifikansi α = 0.05, terdapat bukti yang cukup untuk menolak hipotesis nol."
                    } else {
                        "Pada tingkat signifikansi α = 0.05, tidak terdapat bukti yang cukup untuk menolak hipotesis nol."
                    }
                )

                # Create Word document using officer
                doc <- officer::read_docx()
                doc <- officer::body_add_par(doc, "NusaStat Dashboard", style = "heading 1")
                doc <- officer::body_add_par(doc, paste("Interpretasi", test_type_text), style = "heading 2")
                doc <- officer::body_add_par(doc, paste("Tanggal:", format(Sys.Date(), "%d %B %Y")))
                doc <- officer::body_add_par(doc, "")
                doc <- officer::body_add_par(doc, interpretation_text)

                # Save document
                print(doc, target = file)
            }
        )

        # Comprehensive PDF report download
        output$download_report_pdf <- downloadHandler(
            filename = function() {
                test_type_text <- if (input$test_type == "one_sample") "satu_sampel" else "dua_sampel"
                paste0("laporan_uji_rata_", test_type_text, "_", Sys.Date(), ".pdf")
            },
            content = function(file) {
                req(test_result())

                result <- test_result()
                test_type_text <- if (input$test_type == "one_sample") "Uji t Satu Sampel" else "Uji t Dua Sampel"

                # Prepare input parameters
                input_params <- list(
                    test_type = input$test_type,
                    alpha = 0.05,
                    alternative = input$alternative
                )

                if (input$test_type == "one_sample") {
                    input_params$mu0 <- input$mu_test
                    variabel <- input$var_one
                } else {
                    variabel <- paste(input$var_two, "by", input$group_two)
                }

                # Generate enhanced interpretation using helper function
                interpretation_text <- interpret_ttest(result, input$test_type)

                # Render report using template
                tryCatch({
                    rmarkdown::render(
                        input = here::here("reports", "laporan_uji_rata.Rmd"),
                        output_file = file,
                        output_format = "pdf_document",
                        params = list(
                            jenis_uji = test_type_text,
                            variabel = variabel,
                            hasil_uji = result,
                            interpretasi = interpretation_text,
                            input_params = input_params
                        ),
                        quiet = TRUE
                    )
                }, error = function(e) {
                    # If PDF generation fails, create a simple error document
                    writeLines(paste("Error generating PDF report:", e$message), file)
                    showNotification("PDF generation failed. Please try the Word format.", type = "error")
                })
            }
        )

        # Comprehensive Word report download
        output$download_report_word <- downloadHandler(
            filename = function() {
                test_type_text <- if (input$test_type == "one_sample") "satu_sampel" else "dua_sampel"
                paste0("laporan_uji_rata_", test_type_text, "_", Sys.Date(), ".docx")
            },
            content = function(file) {
                req(test_result())

                result <- test_result()
                test_type_text <- if (input$test_type == "one_sample") "Uji t Satu Sampel" else "Uji t Dua Sampel"

                # Prepare input parameters
                input_params <- list(
                    test_type = input$test_type,
                    alpha = 0.05,
                    alternative = input$alternative
                )

                if (input$test_type == "one_sample") {
                    input_params$mu0 <- input$mu_test
                    variabel <- input$var_one
                } else {
                    variabel <- paste(input$var_two, "by", input$group_two)
                }

                # Generate enhanced interpretation using helper function
                interpretation_text <- interpret_ttest(result, input$test_type)

                # Render report using template (Word output)
                rmarkdown::render(
                    input = here::here("reports", "laporan_uji_rata.Rmd"),
                    output_file = file,
                    output_format = "word_document",
                    params = list(
                        jenis_uji = test_type_text,
                        variabel = variabel,
                        hasil_uji = result,
                        interpretasi = interpretation_text,
                        input_params = input_params
                    ),
                    quiet = TRUE
                )
            }
        )
    })
}
