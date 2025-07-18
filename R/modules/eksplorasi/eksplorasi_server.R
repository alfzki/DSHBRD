# Eksplorasi Server Module
# Server logic for data exploration functionality

#' Eksplorasi Server Module
#'
#' Server logic for data exploration and visualization features
#'
#' @param id Module ID for namespacing
#' @param values Reactive values object containing shared data
eksplorasi_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        # Update variable choices
        observe({
            if (!is.null(values$sovi_data)) {
                numeric_choices <- get_variable_choices(values$sovi_data, "numeric")
                updateSelectInput(session, "select_var", choices = numeric_choices)
            }
        })

        # Summary statistics
        output$summary_stats <- renderPrint({
            req(input$select_var)
            if (!validate_data(values$sovi_data, "Data SOVI")) {
                return()
            }

            var_data <- values$sovi_data[[input$select_var]]
            summary(var_data)
        })

        # Interpretation of statistics - ENHANCED DYNAMIC VERSION
        output$interpretation_stats <- renderUI({
            req(input$select_var)
            if (!validate_data(values$sovi_data, "Data SOVI")) {
                return()
            }

            var_data <- values$sovi_data[[input$select_var]]
            var_summary <- summary(var_data)

            # Calculate additional statistics
            mean_val <- var_summary["Mean"]
            median_val <- var_summary["Median"]
            min_val <- var_summary["Min."]
            max_val <- var_summary["Max."]
            q1_val <- var_summary["1st Qu."]
            q3_val <- var_summary["3rd Qu."]

            # Calculate derived statistics
            range_val <- max_val - min_val
            iqr_val <- q3_val - q1_val
            sd_val <- sd(var_data, na.rm = TRUE)
            cv_val <- (sd_val / mean_val) * 100 # Coefficient of variation

            # Determine distribution shape
            skewness_interpretation <- if (abs(mean_val - median_val) < 0.1 * median_val) {
                "relatif simetris"
            } else if (mean_val > median_val) {
                "cenderung miring ke kanan (positively skewed)"
            } else {
                "cenderung miring ke kiri (negatively skewed)"
            }

            # Determine variability level
            variability_level <- if (cv_val < 15) {
                "rendah"
            } else if (cv_val > 35) {
                "tinggi"
            } else {
                "sedang"
            }

            # Generate outlier information
            outlier_threshold_lower <- q1_val - 1.5 * iqr_val
            outlier_threshold_upper <- q3_val + 1.5 * iqr_val
            potential_outliers <- sum(var_data < outlier_threshold_lower | var_data > outlier_threshold_upper, na.rm = TRUE)
            outlier_percentage <- round((potential_outliers / length(var_data)) * 100, 2)

            tagList(
                h4("Interpretasi Statistik Deskriptif:"),

                # Basic statistics box
                div(
                    style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                    h5("📊 Statistik Dasar:"),
                    tags$ul(
                        tags$li(paste(
                            "Nilai rata-rata:", format_number(mean_val),
                            if (mean_val > median_val) {
                                "(sedikit lebih tinggi dari median)"
                            } else if (mean_val < median_val) {
                                "(sedikit lebih rendah dari median)"
                            } else {
                                "(sama dengan median)"
                            }
                        )),
                        tags$li(paste("Nilai median:", format_number(median_val), "(nilai tengah)")),
                        tags$li(paste(
                            "Rentang nilai:", format_number(min_val), "-", format_number(max_val),
                            "(selisih:", format_number(range_val), ")"
                        )),
                        tags$li(paste(
                            "Rentang interkuartil (IQR):", format_number(iqr_val),
                            "(Q1:", format_number(q1_val), "- Q3:", format_number(q3_val), ")"
                        ))
                    )
                ),

                # Distribution characteristics
                div(
                    style = "background-color: #e8f4fd; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                    h5("📈 Karakteristik Distribusi:"),
                    p(paste("Bentuk distribusi:", skewness_interpretation)),
                    p(paste(
                        "Tingkat variabilitas:", variability_level,
                        "(Koefisien Variasi:", format_number(cv_val), "%)"
                    )),
                    p(paste("Standar deviasi:", format_number(sd_val))),
                    if (potential_outliers > 0) {
                        p(
                            paste(
                                "⚠️ Terdeteksi", potential_outliers, "nilai ekstrem potensial",
                                "(", outlier_percentage, "% dari data)"
                            ),
                            style = "color: #fd7e14; font-weight: bold;"
                        )
                    } else {
                        p("✅ Tidak terdeteksi nilai ekstrem yang signifikan",
                            style = "color: #28a745; font-weight: bold;"
                        )
                    }
                ),

                # Contextual interpretation
                div(
                    style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                    h5("🎯 Interpretasi Kontekstual:"),
                    p(paste("Variabel", input$select_var, "menunjukkan distribusi dengan:")),
                    tags$ul(
                        tags$li(paste("Pusat distribusi (median) berada di", format_number(median_val))),
                        tags$li(paste(
                            "50% data berada dalam rentang", format_number(q1_val),
                            "hingga", format_number(q3_val)
                        )),
                        tags$li(paste(
                            "Tingkat penyebaran", variability_level,
                            "dengan koefisien variasi", format_number(cv_val), "%"
                        ))
                    )
                ),

                # Practical implications
                div(
                    style = "background-color: #d1ecf1; padding: 15px; border-radius: 5px;",
                    h5("💡 Implikasi Praktis:"),
                    p(
                        if (cv_val < 15) {
                            paste("Data", input$select_var, "relatif homogen di seluruh observasi, menunjukkan konsistensi yang baik.")
                        } else if (cv_val > 35) {
                            paste("Data", input$select_var, "menunjukkan variabilitas tinggi, mengindikasikan heterogenitas yang perlu diperhatikan dalam analisis lanjutan.")
                        } else {
                            paste("Data", input$select_var, "menunjukkan variabilitas sedang, sesuai untuk berbagai jenis analisis statistik.")
                        }
                    ),
                    if (skewness_interpretation != "relatif simetris") {
                        p(paste(
                            "Distribusi yang", skewness_interpretation,
                            "menunjukkan perlunya pertimbangan khusus dalam pemilihan metode analisis."
                        ))
                    },
                    if (potential_outliers > 0) {
                        p("Nilai ekstrem yang terdeteksi perlu dievaluasi lebih lanjut untuk memastikan kualitas data.")
                    }
                )
            )
        })

        # Plot visualization
        output$plot_viz <- plotly::renderPlotly({
            req(input$select_var)
            if (!validate_data(values$sovi_data, "Data SOVI")) {
                return()
            }

            var_data <- values$sovi_data[[input$select_var]]

            # Create histogram
            p <- ggplot(values$sovi_data, aes_string(x = input$select_var)) +
                geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
                labs(
                    title = paste("Distribusi", input$select_var),
                    x = input$select_var,
                    y = "Frekuensi"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                    axis.title = element_text(size = 12),
                    axis.text = element_text(size = 10)
                )

            plotly::ggplotly(p, tooltip = c("x", "y"))
        })

        # Map visualization (placeholder)
        output$map_viz <- leaflet::renderLeaflet({
            # This is a placeholder map - in real implementation, you would need spatial data
            leaflet::leaflet() %>%
                leaflet::addTiles() %>%
                leaflet::setView(lng = 106.845599, lat = -6.208763, zoom = 5) %>%
                leaflet::addMarkers(
                    lng = 106.845599, lat = -6.208763,
                    popup = "Indonesia - Peta data spasial memerlukan file shapefile tambahan"
                )
        })

        # Download handlers
        output$download_summary <- downloadHandler(
            filename = function() {
                paste0("ringkasan_", input$select_var, "_", Sys.Date(), ".pdf")
            },
            content = function(file) {
                # Create temporary R Markdown file
                temp_rmd <- tempfile(fileext = ".Rmd")

                rmd_content <- paste0('---
title: "Ringkasan Statistik - ', input$select_var, '"
output: pdf_document
date: "`r Sys.Date()`"
---

# Analisis Deskriptif Variabel: ', input$select_var, "

```{r echo=FALSE}
var_data <- c(", paste(values$sovi_data[[input$select_var]], collapse = ","), ")
summary(var_data)
```

## Interpretasi

Variabel ", input$select_var, " menunjukkan distribusi dengan karakteristik statistik di atas.
")

                writeLines(rmd_content, temp_rmd)
                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )

        output$download_plot <- downloadHandler(
            filename = function() {
                paste0("plot_", input$select_var, "_", Sys.Date(), ".png")
            },
            content = function(file) {
                req(input$select_var)

                p <- ggplot(values$sovi_data, aes_string(x = input$select_var)) +
                    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
                    labs(
                        title = paste("Distribusi", input$select_var),
                        x = input$select_var,
                        y = "Frekuensi"
                    ) +
                    theme_minimal()

                ggsave(file, p, width = 10, height = 6, dpi = 300)
            }
        )

        # Individual interpretation download using officer
        output$download_interpretation <- downloadHandler(
            filename = function() {
                paste0("interpretasi_", input$select_var, "_", Sys.Date(), ".docx")
            },
            content = function(file) {
                req(input$select_var)

                # Get interpretation text
                var_data <- values$sovi_data[[input$select_var]]
                var_summary <- summary(var_data)

                interpretation_text <- paste(
                    "Interpretasi Statistik Deskriptif untuk Variabel:", input$select_var, "\n\n",
                    "Variabel", input$select_var, "memiliki:\n",
                    "- Nilai rata-rata:", format_number(var_summary["Mean"]), "\n",
                    "- Nilai median:", format_number(var_summary["Median"]), "\n",
                    "- Nilai minimum:", format_number(var_summary["Min."]), "\n",
                    "- Nilai maksimum:", format_number(var_summary["Max."]), "\n",
                    "- Kuartil pertama (Q1):", format_number(var_summary["1st Qu."]), "\n",
                    "- Kuartil ketiga (Q3):", format_number(var_summary["3rd Qu."]), "\n\n",
                    "Berdasarkan statistik di atas, variabel", input$select_var,
                    "menunjukkan distribusi dengan rentang nilai dari",
                    format_number(var_summary["Min."]), "hingga",
                    format_number(var_summary["Max."]), "."
                )

                # Create Word document using officer
                doc <- officer::read_docx()
                doc <- officer::body_add_par(doc, "NusaStat Dashboard", style = "heading 1")
                doc <- officer::body_add_par(doc, "Interpretasi Eksplorasi Data", style = "heading 2")
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
                paste0("laporan_eksplorasi_", input$select_var, "_", Sys.Date(), ".pdf")
            },
            content = function(file) {
                req(input$select_var)

                # Create temporary plot file
                temp_plot <- tempfile(fileext = ".png")
                p <- ggplot(values$sovi_data, aes_string(x = input$select_var)) +
                    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
                    labs(
                        title = paste("Distribusi", input$select_var),
                        x = input$select_var,
                        y = "Frekuensi"
                    ) +
                    theme_minimal() +
                    theme(
                        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                        axis.title = element_text(size = 12),
                        axis.text = element_text(size = 10)
                    )
                ggsave(temp_plot, p, width = 10, height = 6, dpi = 300)

                # Prepare data for report
                var_data <- values$sovi_data[[input$select_var]]
                var_summary <- summary(var_data)

                interpretation_text <- paste(
                    "Berdasarkan analisis statistik deskriptif, variabel", input$select_var,
                    "memiliki nilai rata-rata", format_number(var_summary["Mean"]),
                    "dan median", format_number(var_summary["Median"]), ".",
                    "Distribusi data menunjukkan rentang dari",
                    format_number(var_summary["Min."]), "hingga",
                    format_number(var_summary["Max."]),
                    "dengan kuartil pertama", format_number(var_summary["1st Qu."]),
                    "dan kuartil ketiga", format_number(var_summary["3rd Qu."]), ".",
                    "\n\nAnalisis ini memberikan gambaran komprehensif tentang karakteristik distribusi",
                    "variabel dalam dataset kerentanan sosial Indonesia."
                )

                dataset_info <- list(
                    n_obs = nrow(values$sovi_data),
                    n_vars = ncol(values$sovi_data),
                    n_numeric = length(get_numeric_columns(values$sovi_data)),
                    n_categorical = length(get_categorical_columns(values$sovi_data))
                )

                # Render report using template
                rmarkdown::render(
                    input = here::here("reports", "laporan_eksplorasi.Rmd"),
                    output_file = file,
                    params = list(
                        var_terpilih = input$select_var,
                        data_summary = var_summary,
                        plot_path = temp_plot,
                        interpretasi = interpretation_text,
                        dataset_info = dataset_info
                    ),
                    quiet = TRUE
                )

                # Clean up temporary file
                if (file.exists(temp_plot)) file.remove(temp_plot)
            }
        )

        # Comprehensive Word report download
        output$download_report_word <- downloadHandler(
            filename = function() {
                paste0("laporan_eksplorasi_", input$select_var, "_", Sys.Date(), ".docx")
            },
            content = function(file) {
                req(input$select_var)

                # Create temporary plot file
                temp_plot <- tempfile(fileext = ".png")
                p <- ggplot(values$sovi_data, aes_string(x = input$select_var)) +
                    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
                    labs(
                        title = paste("Distribusi", input$select_var),
                        x = input$select_var,
                        y = "Frekuensi"
                    ) +
                    theme_minimal() +
                    theme(
                        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                        axis.title = element_text(size = 12),
                        axis.text = element_text(size = 10)
                    )
                ggsave(temp_plot, p, width = 10, height = 6, dpi = 300)

                # Prepare data for report
                var_data <- values$sovi_data[[input$select_var]]
                var_summary <- summary(var_data)

                interpretation_text <- paste(
                    "Berdasarkan analisis statistik deskriptif, variabel", input$select_var,
                    "memiliki nilai rata-rata", format_number(var_summary["Mean"]),
                    "dan median", format_number(var_summary["Median"]), ".",
                    "Distribusi data menunjukkan rentang dari",
                    format_number(var_summary["Min."]), "hingga",
                    format_number(var_summary["Max."]),
                    "dengan kuartil pertama", format_number(var_summary["1st Qu."]),
                    "dan kuartil ketiga", format_number(var_summary["3rd Qu."]), ".",
                    "\n\nAnalisis ini memberikan gambaran komprehensif tentang karakteristik distribusi",
                    "variabel dalam dataset kerentanan sosial Indonesia."
                )

                dataset_info <- list(
                    n_obs = nrow(values$sovi_data),
                    n_vars = ncol(values$sovi_data),
                    n_numeric = length(get_numeric_columns(values$sovi_data)),
                    n_categorical = length(get_categorical_columns(values$sovi_data))
                )

                # Render report using template (Word output)
                rmarkdown::render(
                    input = here::here("reports", "laporan_eksplorasi.Rmd"),
                    output_file = file,
                    output_format = "word_document",
                    params = list(
                        var_terpilih = input$select_var,
                        data_summary = var_summary,
                        plot_path = temp_plot,
                        interpretasi = interpretation_text,
                        dataset_info = dataset_info
                    ),
                    quiet = TRUE
                )

                # Clean up temporary file
                if (file.exists(temp_plot)) file.remove(temp_plot)
            }
        )
    })
}
