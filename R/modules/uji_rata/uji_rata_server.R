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
                numeric_choices <- get_variable_choices(values$sovi_data, "numeric")
                categorical_choices <- get_variable_choices(values$sovi_data, "categorical")

                updateSelectInput(session, "var_one", choices = numeric_choices)
                updateSelectInput(session, "var_two", choices = numeric_choices)
                updateSelectInput(session, "group_two", choices = categorical_choices)
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

        # Generate ENHANCED DYNAMIC interpretation
        output$interpretation <- renderUI({
            req(test_result())

            result <- test_result()
            p_value <- result$p.value
            t_stat <- result$statistic
            df <- result$parameter
            conf_int <- result$conf.int
            alpha <- 0.05 # Could be made dynamic

            # Effect size calculation (Cohen's d for two-sample, d for one-sample)
            effect_size <- NULL
            if (input$test_type == "one_sample") {
                # Cohen's d for one-sample t-test
                var_data <- values$sovi_data[[input$var_one]]
                var_data <- var_data[!is.na(var_data)]
                effect_size <- (mean(var_data) - input$mu_test) / sd(var_data)

                h0 <- paste("μ =", input$mu_test)
                h1 <- switch(input$alternative,
                    "two.sided" = paste("μ ≠", input$mu_test),
                    "greater" = paste("μ >", input$mu_test),
                    "less" = paste("μ <", input$mu_test)
                )

                sample_mean <- mean(var_data)
                sample_sd <- sd(var_data)
                sample_n <- length(var_data)
            } else {
                # Two-sample t-test
                group_var <- values$sovi_data[[input$group_two]]
                unique_groups <- unique(group_var)
                var_data <- values$sovi_data[[input$var_two]]

                group1_data <- var_data[group_var == unique_groups[1] & !is.na(var_data)]
                group2_data <- var_data[group_var == unique_groups[2] & !is.na(var_data)]

                # Cohen's d for two-sample t-test
                pooled_sd <- sqrt(((length(group1_data) - 1) * var(group1_data) +
                    (length(group2_data) - 1) * var(group2_data)) /
                    (length(group1_data) + length(group2_data) - 2))
                effect_size <- (mean(group1_data) - mean(group2_data)) / pooled_sd

                h0 <- "μ₁ = μ₂"
                h1 <- switch(input$alternative,
                    "two.sided" = "μ₁ ≠ μ₂",
                    "greater" = "μ₁ > μ₂",
                    "less" = "μ₁ < μ₂"
                )
            }

            # Effect size interpretation
            effect_interpretation <- if (is.null(effect_size)) {
                "Tidak dapat dihitung"
            } else if (abs(effect_size) < 0.2) {
                "kecil"
            } else if (abs(effect_size) < 0.5) {
                "sedang"
            } else if (abs(effect_size) < 0.8) {
                "besar"
            } else {
                "sangat besar"
            }

            # Statistical decision
            is_significant <- p_value < alpha

            # Power and confidence
            confidence_level <- if (p_value < 0.001) {
                "sangat tinggi"
            } else if (p_value < 0.01) {
                "tinggi"
            } else if (p_value < 0.05) {
                "cukup"
            } else {
                "rendah"
            }

            tagList(
                # Main decision box
                div(
                    style = if (is_significant) {
                        "background-color: #d4edda; border: 1px solid #c3e6cb; padding: 15px; border-radius: 5px; margin-bottom: 15px;"
                    } else {
                        "background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 15px; border-radius: 5px; margin-bottom: 15px;"
                    },
                    h5(if (is_significant) "✅ Hasil Signifikan" else "❌ Hasil Tidak Signifikan"),
                    p(
                        strong("Keputusan Statistik: "),
                        if (is_significant) {
                            paste("Tolak H₀ pada α =", alpha)
                        } else {
                            paste("Gagal menolak H₀ pada α =", alpha)
                        }
                    ),
                    p(
                        strong("Kesimpulan: "),
                        if (is_significant) {
                            paste("Terdapat bukti statistik yang", confidence_level, "untuk menolak hipotesis nol.")
                        } else {
                            "Tidak terdapat bukti statistik yang cukup untuk menolak hipotesis nol."
                        }
                    )
                ),

                # Statistical details
                div(
                    style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                    h5("📊 Detail Statistik:"),
                    tags$ul(
                        tags$li(paste("t-statistik:", format_number(t_stat, 4))),
                        tags$li(paste("Derajat kebebasan:", df)),
                        tags$li(paste("p-value:", format_number(p_value, 6))),
                        tags$li(paste(
                            "Interval kepercayaan 95%: [",
                            format_number(conf_int[1], 4), ",",
                            format_number(conf_int[2], 4), "]"
                        )),
                        if (!is.null(effect_size)) {
                            tags$li(paste(
                                "Effect size (Cohen's d):", format_number(effect_size, 4),
                                "(", effect_interpretation, ")"
                            ))
                        }
                    )
                ),

                # Hypotheses
                div(
                    style = "background-color: #e8f4fd; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                    h5("🔬 Hipotesis:"),
                    p(paste("H₀:", h0)),
                    p(paste("H₁:", h1)),
                    p(paste("Tingkat signifikansi: α =", alpha))
                ),

                # Practical interpretation
                if (input$test_type == "one_sample") {
                    div(
                        style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                        h5("💡 Interpretasi Praktis:"),
                        p(paste("Rata-rata sampel:", format_number(sample_mean, 4))),
                        p(paste("Standar deviasi:", format_number(sample_sd, 4))),
                        p(paste("Ukuran sampel:", sample_n)),
                        if (is_significant) {
                            p(paste(
                                "Rata-rata populasi secara signifikan berbeda dari", input$mu_test,
                                "dengan tingkat kepercayaan", confidence_level, "."
                            ))
                        } else {
                            p(paste("Tidak ada bukti yang cukup bahwa rata-rata populasi berbeda dari", input$mu_test, "."))
                        }
                    )
                } else {
                    div(
                        style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                        h5("💡 Interpretasi Praktis:"),
                        p(paste("Rata-rata", unique_groups[1], ":", format_number(mean(group1_data), 4))),
                        p(paste("Rata-rata", unique_groups[2], ":", format_number(mean(group2_data), 4))),
                        p(paste("Selisih rata-rata:", format_number(abs(mean(group1_data) - mean(group2_data)), 4))),
                        if (is_significant) {
                            p(paste(
                                "Terdapat perbedaan rata-rata yang signifikan antara", unique_groups[1],
                                "dan", unique_groups[2], "dengan ukuran efek", effect_interpretation, "."
                            ))
                        } else {
                            p(paste(
                                "Tidak terdapat perbedaan rata-rata yang signifikan antara", unique_groups[1],
                                "dan", unique_groups[2], "."
                            ))
                        }
                    )
                },

                # Recommendations
                div(
                    style = "background-color: #d1ecf1; padding: 15px; border-radius: 5px;",
                    h5("📋 Rekomendasi:"),
                    if (is_significant && !is.null(effect_size) && abs(effect_size) < 0.3) {
                        p("⚠️ Meskipun signifikan secara statistik, ukuran efek relatif kecil. Pertimbangkan kepentingan praktis dari temuan ini.")
                    } else if (is_significant) {
                        p("✅ Hasil menunjukkan perbedaan yang signifikan secara statistik dan praktis.")
                    } else {
                        p("🔍 Pertimbangkan untuk memperbesar ukuran sampel atau memeriksa asumsi uji jika diperlukan.")
                    },
                    if (df < 30) {
                        p("📝 Catatan: Ukuran sampel relatif kecil. Pastikan asumsi normalitas terpenuhi.")
                    }
                )
            )
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

                # Generate interpretation text
                interpretation_text <- paste(
                    "Berdasarkan hasil uji t, dengan p-value =", format_number(result$p.value, 6),
                    if (result$p.value < 0.05) {
                        ", terdapat bukti statistik yang cukup untuk menolak hipotesis nol pada tingkat signifikansi α = 0.05."
                    } else {
                        ", tidak terdapat bukti statistik yang cukup untuk menolak hipotesis nol pada tingkat signifikansi α = 0.05."
                    },
                    "\n\nInterval kepercayaan 95% untuk perbedaan rata-rata adalah [",
                    format_number(result$conf.int[1], 4), ", ", format_number(result$conf.int[2], 4), "]."
                )

                # Render report using template
                rmarkdown::render(
                    input = here::here("reports", "laporan_uji_rata.Rmd"),
                    output_file = file,
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

                # Generate interpretation text
                interpretation_text <- paste(
                    "Berdasarkan hasil uji t, dengan p-value =", format_number(result$p.value, 6),
                    if (result$p.value < 0.05) {
                        ", terdapat bukti statistik yang cukup untuk menolak hipotesis nol pada tingkat signifikansi α = 0.05."
                    } else {
                        ", tidak terdapat bukti statistik yang cukup untuk menolak hipotesis nol pada tingkat signifikansi α = 0.05."
                    },
                    "\n\nInterval kepercayaan 95% untuk perbedaan rata-rata adalah [",
                    format_number(result$conf.int[1], 4), ", ", format_number(result$conf.int[2], 4), "]."
                )

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
