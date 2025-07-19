# ==============================================================================
# MODUL SERVER EKSPLORASI DATA
# ==============================================================================
#
# Tujuan: Logika server untuk fungsionalitas eksplorasi dan visualisasi data
# Penulis: Tim Dashboard ALIVA
# Terakhir Diperbarui: Juli 2025
#
# Deskripsi:
# Modul ini menyediakan kemampuan eksplorasi data yang komprehensif meliputi:
# - Ringkasan statistik dan interpretasi
# - Visualisasi interaktif (histogram, peta spasial)
# - Fungsionalitas export untuk laporan dan plot
# - Interpretasi dinamis ukuran statistik
#
# Dependensi:
# - ggplot2: Untuk plotting
# - plotly: Untuk plot interaktif
# - leaflet: Untuk visualisasi spasial
# - DT: Untuk tabel data
# - rmarkdown/officer: Untuk generasi laporan
# ==============================================================================

#' Modul Server Eksplorasi
#'
#' @description Logika server untuk eksplorasi dan visualisasi data yang komprehensif
#' @param id Character. ID modul untuk namespacing
#' @param values Reactive values. Objek yang berisi data aplikasi bersama
#' @return Fungsi server modul Shiny
#' @author Tim Dashboard ALIVA
eksplorasi_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        # ==============================================================================
        # REACTIVE VARIABLE UPDATES
        # ==============================================================================

        # Update variable choices reactively when data changes
        observe({
            req(values$sovi_data)
            # Create reactive dependency on data update counter
            data_counter <- values$data_update_counter

            # Get numeric variable choices and update UI
            numeric_choices <- get_variable_choices(values$sovi_data, "numeric")
            updateSelectInput(session, "select_var", choices = numeric_choices)
        })

        # ==============================================================================
        # STATISTICAL SUMMARY OUTPUTS
        # ==============================================================================


        # Generate basic statistical summary
        output$summary_stats <- renderPrint({
            req(input$select_var)

            # Validate data availability
            if (!validate_data(values$sovi_data, "Data SOVI")) {
                return(NULL)
            }

            # Extract variable data and compute summary
            var_data <- values$sovi_data[[input$select_var]]

            # Return formatted summary statistics
            summary(var_data)
        })


        # Generate comprehensive statistical interpretation
        output$interpretation_stats <- renderUI({
            req(input$select_var)

            # Validate data availability
            if (!validate_data(values$sovi_data, "Data SOVI")) {
                return(NULL)
            }

            # Extract and compute statistics
            var_data <- values$sovi_data[[input$select_var]]
            var_summary <- summary(var_data)
            statistics <- calculate_extended_statistics(var_data, var_summary)

            # Generate interpretation components
            generate_interpretation_ui(input$select_var, statistics)
        })

        # ==============================================================================
        # HELPER FUNCTIONS FOR STATISTICAL INTERPRETATION
        # ==============================================================================

        # Calculate extended statistical measures
        calculate_extended_statistics <- function(var_data, var_summary) {
            list(
                mean_val = var_summary["Mean"],
                median_val = var_summary["Median"],
                min_val = var_summary["Min."],
                max_val = var_summary["Max."],
                q1_val = var_summary["1st Qu."],
                q3_val = var_summary["3rd Qu."],
                range_val = var_summary["Max."] - var_summary["Min."],
                iqr_val = var_summary["3rd Qu."] - var_summary["1st Qu."],
                sd_val = sd(var_data, na.rm = TRUE),
                cv_val = (sd(var_data, na.rm = TRUE) / var_summary["Mean"]) * 100
            )
        }

        # Generate interpretation UI components
        generate_interpretation_ui <- function(var_name, stats) {
            # Determine distribution characteristics
            skewness_interpretation <- determine_skewness(stats$mean_val, stats$median_val)
            variability_level <- determine_variability_level(stats$cv_val)
            outlier_info <- calculate_outlier_information(var_data, stats$q1_val, stats$q3_val, stats$iqr_val)

            # Build comprehensive interpretation UI
            tagList(
                h4("Interpretasi Statistik Deskriptif:", style = "color: #2c3e50; font-weight: bold;"),

                # Basic Statistics Panel
                create_basic_stats_panel(stats),

                # Distribution Characteristics Panel
                create_distribution_panel(skewness_interpretation, variability_level, stats, outlier_info),

                # Contextual Interpretation Panel
                create_contextual_panel(var_name, stats),

                # Practical Implications Panel
                create_practical_implications_panel(var_name, stats$cv_val, skewness_interpretation, outlier_info)
            )
        }

        # Determine skewness interpretation
        determine_skewness <- function(mean_val, median_val) {
            if (abs(mean_val - median_val) < 0.1 * median_val) {
                "relatif simetris"
            } else if (mean_val > median_val) {
                "cenderung miring ke kanan (positively skewed)"
            } else {
                "cenderung miring ke kiri (negatively skewed)"
            }
        }

        # Determine variability level
        determine_variability_level <- function(cv_val) {
            if (cv_val < 15) {
                "rendah"
            } else if (cv_val > 35) {
                "tinggi"
            } else {
                "sedang"
            }
        }

        # Calculate outlier information
        calculate_outlier_information <- function(var_data, q1_val, q3_val, iqr_val) {
            outlier_threshold_lower <- q1_val - 1.5 * iqr_val
            outlier_threshold_upper <- q3_val + 1.5 * iqr_val
            potential_outliers <- sum(var_data < outlier_threshold_lower |
                var_data > outlier_threshold_upper, na.rm = TRUE)
            outlier_percentage <- round((potential_outliers / length(var_data)) * 100, 2)

            list(count = potential_outliers, percentage = outlier_percentage)
        }

        # Create basic statistics panel
        create_basic_stats_panel <- function(stats) {
            div(
                style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                h5("📊 Statistik Dasar:", style = "color: #495057; font-weight: bold;"),
                tags$ul(
                    tags$li(paste(
                        "Nilai rata-rata:", format_number(stats$mean_val),
                        if (stats$mean_val > stats$median_val) {
                            "(sedikit lebih tinggi dari median)"
                        } else if (stats$mean_val < stats$median_val) {
                            "(sedikit lebih rendah dari median)"
                        } else {
                            "(sama dengan median)"
                        }
                    )),
                    tags$li(paste("Nilai median:", format_number(stats$median_val), "(nilai tengah)")),
                    tags$li(paste(
                        "Rentang nilai:", format_number(stats$min_val), "-", format_number(stats$max_val),
                        "(selisih:", format_number(stats$range_val), ")"
                    )),
                    tags$li(paste(
                        "Rentang interkuartil (IQR):", format_number(stats$iqr_val),
                        "(Q1:", format_number(stats$q1_val), "- Q3:", format_number(stats$q3_val), ")"
                    ))
                )
            )
        }

        # Create distribution characteristics panel
        create_distribution_panel <- function(skewness_interpretation, variability_level, stats, outlier_info) {
            div(
                style = "background-color: #e8f4fd; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                h5("📈 Karakteristik Distribusi:", style = "color: #0c5460; font-weight: bold;"),
                p(paste("Bentuk distribusi:", skewness_interpretation)),
                p(paste(
                    "Tingkat variabilitas:", variability_level,
                    "(Koefisien Variasi:", format_number(stats$cv_val), "%)"
                )),
                p(paste("Standar deviasi:", format_number(stats$sd_val))),
                if (outlier_info$count > 0) {
                    p(
                        paste(
                            "⚠️ Terdeteksi", outlier_info$count, "nilai ekstrem potensial",
                            "(", outlier_info$percentage, "% dari data)"
                        ),
                        style = "color: #fd7e14; font-weight: bold;"
                    )
                } else {
                    p("✅ Tidak terdeteksi nilai ekstrem yang signifikan",
                        style = "color: #28a745; font-weight: bold;"
                    )
                }
            )
        }

        # Create contextual interpretation panel
        create_contextual_panel <- function(var_name, stats) {
            div(
                style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                h5("🎯 Interpretasi Kontekstual:", style = "color: #856404; font-weight: bold;"),
                p(paste("Variabel", var_name, "menunjukkan distribusi dengan:")),
                tags$ul(
                    tags$li(paste("Pusat distribusi (median) berada di", format_number(stats$median_val))),
                    tags$li(paste(
                        "50% data berada dalam rentang", format_number(stats$q1_val),
                        "hingga", format_number(stats$q3_val)
                    )),
                    tags$li(paste(
                        "Tingkat penyebaran", determine_variability_level(stats$cv_val),
                        "dengan koefisien variasi", format_number(stats$cv_val), "%"
                    ))
                )
            )
        }

        # Create practical implications panel
        create_practical_implications_panel <- function(var_name, cv_val, skewness_interpretation, outlier_info) {
            div(
                style = "background-color: #d1ecf1; padding: 15px; border-radius: 5px;",
                h5("💡 Implikasi Praktis:", style = "color: #0c5460; font-weight: bold;"),
                p(
                    if (cv_val < 15) {
                        paste("Data", var_name, "relatif homogen di seluruh observasi, menunjukkan konsistensi yang baik.")
                    } else if (cv_val > 35) {
                        paste("Data", var_name, "menunjukkan variabilitas tinggi, mengindikasikan heterogenitas yang perlu diperhatikan dalam analisis lanjutan.")
                    } else {
                        paste("Data", var_name, "menunjukkan variabilitas sedang, sesuai untuk berbagai jenis analisis statistik.")
                    }
                ),
                if (skewness_interpretation != "relatif simetris") {
                    p(paste(
                        "Distribusi yang", skewness_interpretation,
                        "menunjukkan perlunya pertimbangan khusus dalam pemilihan metode analisis."
                    ))
                },
                if (outlier_info$count > 0) {
                    p("Nilai ekstrem yang terdeteksi perlu dievaluasi lebih lanjut untuk memastikan kualitas data.")
                }
            )
        }

        # ==============================================================================
        # VISUALIZATION OUTPUTS
        # ==============================================================================

        # Generate interactive histogram plot
        output$plot_viz <- plotly::renderPlotly({
            req(input$select_var)

            # Validate data availability
            if (!validate_data(values$sovi_data, "Data SOVI")) {
                return(NULL)
            }

            # Create histogram using ggplot2
            p <- ggplot(values$sovi_data, aes_string(x = input$select_var)) +
                geom_histogram(
                    bins = 30,
                    fill = "steelblue",
                    alpha = 0.7,
                    color = "white",
                    boundary = 0
                ) +
                labs(
                    title = paste("Distribusi Variabel:", input$select_var),
                    x = input$select_var,
                    y = "Frekuensi"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                    axis.title = element_text(size = 12),
                    axis.text = element_text(size = 10),
                    panel.grid.minor = element_blank()
                )

            # Convert to interactive plotly object
            plotly::ggplotly(p, tooltip = c("x", "y")) %>%
                plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
        })


        # Generate interactive spatial visualization map
        output$map_viz <- leaflet::renderLeaflet({
            req(input$select_var)

            # Load and validate GeoJSON data
            geojson_path <- here::here("data", "indonesia_kabkota.geojson")

            if (!file.exists(geojson_path)) {
                return(create_fallback_map())
            }

            # Attempt to create spatial visualization
            tryCatch(
                {
                    create_spatial_vulnerability_map(geojson_path, input$select_var, values$sovi_data)
                },
                error = function(e) {
                    message("Map creation error: ", e$message)
                    create_error_map(e$message)
                }
            )
        })

        # ==============================================================================
        # HELPER FUNCTIONS FOR MAP VISUALIZATION
        # ==============================================================================

        # Create fallback map when GeoJSON is not available
        create_fallback_map <- function() {
            leaflet::leaflet() %>%
                leaflet::addTiles() %>%
                leaflet::setView(lng = 117.0, lat = -2.5, zoom = 5) %>%
                leaflet::addMarkers(
                    lng = 117.0, lat = -2.5,
                    popup = "Data peta spasial tidak tersedia. Silakan unduh file GeoJSON Indonesia."
                )
        }

        # Create error map for troubleshooting
        create_error_map <- function(error_message) {
            leaflet::leaflet() %>%
                leaflet::addTiles() %>%
                leaflet::setView(lng = 117.0, lat = -2.5, zoom = 5) %>%
                leaflet::addMarkers(
                    lng = 117.0, lat = -2.5,
                    popup = paste("Error loading map data:", error_message)
                )
        }

        # Create spatial vulnerability map with data overlay
        create_spatial_vulnerability_map <- function(geojson_path, selected_var, sovi_data) {
            # Read the GeoJSON file
            indonesia_map <- sf::st_read(geojson_path, quiet = TRUE)

            # Get the selected variable data
            var_data <- sovi_data[[selected_var]]

            # Create simulated spatial data for demonstration
            # Note: In production, this would use actual district-level data joins
            set.seed(123) # For reproducible results
            n_districts <- nrow(indonesia_map)

            # Simulate district values based on variable's distribution
            mean_val <- mean(var_data, na.rm = TRUE)
            sd_val <- sd(var_data, na.rm = TRUE)
            simulated_values <- rnorm(n_districts, mean = mean_val, sd = sd_val)

            # Add simulated data to spatial data
            indonesia_map$vulnerability_value <- simulated_values

            # Create color palette
            pal <- leaflet::colorNumeric(
                palette = "YlOrRd",
                domain = indonesia_map$vulnerability_value,
                na.color = "transparent"
            )

            # Create informative popup labels
            labels <- sprintf(
                "<strong>%s</strong><br/>
                Nilai %s: <strong>%.3f</strong><br/>
                <em>Catatan: Data ini disimulasikan untuk demonstrasi</em>",
                indonesia_map$NAME_2, # District name
                selected_var,
                indonesia_map$vulnerability_value
            ) %>% lapply(htmltools::HTML)

            # Build the interactive leaflet map
            leaflet::leaflet(indonesia_map) %>%
                leaflet::addTiles() %>%
                leaflet::setView(lng = 117.0, lat = -2.5, zoom = 5) %>%
                leaflet::addPolygons(
                    fillColor = ~ pal(vulnerability_value),
                    weight = 1,
                    opacity = 1,
                    color = "white",
                    dashArray = "1",
                    fillOpacity = 0.7,
                    highlight = leaflet::highlightOptions(
                        weight = 3,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.9,
                        bringToFront = TRUE
                    ),
                    label = labels,
                    labelOptions = leaflet::labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"
                    )
                ) %>%
                leaflet::addLegend(
                    pal = pal,
                    values = ~vulnerability_value,
                    opacity = 0.9,
                    title = paste("Nilai", selected_var),
                    position = "bottomright"
                ) %>%
                leaflet::addControl(
                    html = paste0(
                        "<div style='background: white; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>",
                        "<h4 style='margin: 0 0 10px 0; color: #2c3e50;'>Peta Kerentanan Sosial Indonesia</h4>",
                        "<p style='margin: 0;'><strong>Variabel:</strong> ", selected_var, "</p>",
                        "<p style='margin: 5px 0 0 0; font-style: italic; color: #6c757d;'>",
                        "Catatan: Data spasial ini disimulasikan untuk tujuan demonstrasi.</p>",
                        "</div>"
                    ),
                    position = "topright"
                )
        }

        # ==============================================================================
        # DOWNLOAD HANDLERS
        # ==============================================================================

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

        # Plot download as JPG
        output$download_plot_jpg <- downloadHandler(
            filename = function() {
                paste0("plot_", input$select_var, "_", Sys.Date(), ".jpg")
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

                ggsave(file, p, width = 10, height = 6, dpi = 300, device = "jpeg")
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

                # Create temporary plot file with clean path
                temp_plot <- tempfile(fileext = ".png", tmpdir = tempdir())
                temp_plot <- normalizePath(temp_plot, winslash = "/", mustWork = FALSE)
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

                # Prepare enhanced data for report
                var_data <- values$sovi_data[[input$select_var]]
                var_summary <- summary(var_data)

                # Calculate additional statistics
                mean_val <- var_summary["Mean"]
                median_val <- var_summary["Median"]
                sd_val <- sd(var_data, na.rm = TRUE)
                cv_val <- (sd_val / mean_val) * 100

                # Generate enhanced interpretation using comprehensive analysis
                interpretation_text <- paste(
                    "**LAPORAN EKSPLORASI DATA KOMPREHENSIF**\n\n",
                    "**Variabel yang Dianalisis:** ", input$select_var, "\n\n",
                    "**ANALISIS STATISTIK DESKRIPTIF**\n\n",
                    "1. **Tendensi Sentral:**\n",
                    "   - Rata-rata (Mean): ", format_number(mean_val, 4), "\n",
                    "   - Median: ", format_number(median_val, 4), "\n",
                    "   - Selisih rata-rata dan median: ", format_number(abs(mean_val - median_val), 4), "\n\n",
                    "2. **Ukuran Penyebaran:**\n",
                    "   - Standar deviasi: ", format_number(sd_val, 4), "\n",
                    "   - Koefisien variasi: ", format_number(cv_val, 2), "%\n",
                    "   - Minimum: ", format_number(var_summary["Min."], 4), "\n",
                    "   - Maksimum: ", format_number(var_summary["Max."], 4), "\n",
                    "   - Kuartil 1 (Q1): ", format_number(var_summary["1st Qu."], 4), "\n",
                    "   - Kuartil 3 (Q3): ", format_number(var_summary["3rd Qu."], 4), "\n",
                    "   - Interquartile Range (IQR): ", format_number(var_summary["3rd Qu."] - var_summary["1st Qu."], 4), "\n\n",
                    "**INTERPRETASI DAN ANALISIS:**\n\n",
                    "3. **Bentuk Distribusi:**\n",
                    if (abs(mean_val - median_val) < 0.1 * median_val) {
                        "   - Distribusi data relatif SIMETRIS karena rata-rata dan median hampir sama.\n"
                    } else if (mean_val > median_val) {
                        "   - Distribusi data MIRING KE KANAN (positively skewed) karena rata-rata > median.\n"
                    } else {
                        "   - Distribusi data MIRING KE KIRI (negatively skewed) karena rata-rata < median.\n"
                    },
                    "\n4. **Tingkat Variabilitas:**\n",
                    if (cv_val < 15) {
                        "   - Variabilitas RENDAH (CV < 15%) - data relatif homogen dan konsisten.\n"
                    } else if (cv_val > 35) {
                        "   - Variabilitas TINGGI (CV > 35%) - data cukup heterogen dengan penyebaran luas.\n"
                    } else {
                        "   - Variabilitas SEDANG (15% ≤ CV ≤ 35%) - tingkat keragaman data normal.\n"
                    },
                    "\n5. **Kesimpulan Analisis:**\n",
                    "   Berdasarkan analisis statistik deskriptif komprehensif, variabel ", input$select_var,
                    " menunjukkan karakteristik distribusi dengan rentang nilai yang mencakup spektrum luas ",
                    "dalam konteks analisis kerentanan sosial. Data ini dapat digunakan untuk memahami ",
                    "pola sebaran indikator sosial-ekonomi di berbagai wilayah Indonesia.\n\n",
                    "**IMPLIKASI PRAKTIS:**\n",
                    "- Nilai rata-rata dapat dijadikan benchmarking untuk evaluasi tingkat kerentanan\n",
                    "- Tingkat variabilitas menunjukkan sejauh mana heterogenitas kondisi antar wilayah\n",
                    "- Informasi distribusi membantu dalam perumusan kebijakan yang tepat sasaran\n\n",
                    "**Tanggal Analisis:** ", format(Sys.Date(), "%d %B %Y"), "\n",
                    "**Sumber Data:** ALIVA Dashboard - Analisis Kerentanan Sosial Indonesia"
                )

                dataset_info <- list(
                    n_obs = nrow(values$sovi_data),
                    n_vars = ncol(values$sovi_data),
                    n_numeric = length(get_numeric_columns(values$sovi_data)),
                    n_categorical = length(get_categorical_columns(values$sovi_data))
                )

                # Render report using template
                tryCatch(
                    {
                        rmarkdown::render(
                            input = here::here("reports", "laporan_eksplorasi.Rmd"),
                            output_file = file,
                            output_format = "pdf_document",
                            params = list(
                                var_terpilih = input$select_var,
                                data_summary = var_summary,
                                plot_path = temp_plot,
                                interpretasi = interpretation_text,
                                dataset_info = dataset_info
                            ),
                            quiet = TRUE
                        )
                    },
                    error = function(e) {
                        # If PDF generation fails, create a simple error document
                        writeLines(paste("Error generating PDF report:", e$message), file)
                        showNotification("PDF generation failed. Please try the Word format.", type = "error")
                    }
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

                # Create temporary plot file with clean path
                temp_plot <- tempfile(fileext = ".png", tmpdir = tempdir())
                temp_plot <- normalizePath(temp_plot, winslash = "/", mustWork = FALSE)
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

                # Prepare enhanced data for report
                var_data <- values$sovi_data[[input$select_var]]
                var_summary <- summary(var_data)

                # Calculate additional statistics
                mean_val <- var_summary["Mean"]
                median_val <- var_summary["Median"]
                sd_val <- sd(var_data, na.rm = TRUE)
                cv_val <- (sd_val / mean_val) * 100

                # Generate enhanced interpretation using comprehensive analysis
                interpretation_text <- paste(
                    "**LAPORAN EKSPLORASI DATA KOMPREHENSIF**\n\n",
                    "**Variabel yang Dianalisis:** ", input$select_var, "\n\n",
                    "**ANALISIS STATISTIK DESKRIPTIF**\n\n",
                    "1. **Tendensi Sentral:**\n",
                    "   - Rata-rata (Mean): ", format_number(mean_val, 4), "\n",
                    "   - Median: ", format_number(median_val, 4), "\n",
                    "   - Selisih rata-rata dan median: ", format_number(abs(mean_val - median_val), 4), "\n\n",
                    "2. **Ukuran Penyebaran:**\n",
                    "   - Standar deviasi: ", format_number(sd_val, 4), "\n",
                    "   - Koefisien variasi: ", format_number(cv_val, 2), "%\n",
                    "   - Minimum: ", format_number(var_summary["Min."], 4), "\n",
                    "   - Maksimum: ", format_number(var_summary["Max."], 4), "\n",
                    "   - Kuartil 1 (Q1): ", format_number(var_summary["1st Qu."], 4), "\n",
                    "   - Kuartil 3 (Q3): ", format_number(var_summary["3rd Qu."], 4), "\n",
                    "   - Interquartile Range (IQR): ", format_number(var_summary["3rd Qu."] - var_summary["1st Qu."], 4), "\n\n",
                    "**INTERPRETASI DAN ANALISIS:**\n\n",
                    "3. **Bentuk Distribusi:**\n",
                    if (abs(mean_val - median_val) < 0.1 * median_val) {
                        "   - Distribusi data relatif SIMETRIS karena rata-rata dan median hampir sama.\n"
                    } else if (mean_val > median_val) {
                        "   - Distribusi data MIRING KE KANAN (positively skewed) karena rata-rata > median.\n"
                    } else {
                        "   - Distribusi data MIRING KE KIRI (negatively skewed) karena rata-rata < median.\n"
                    },
                    "\n4. **Tingkat Variabilitas:**\n",
                    if (cv_val < 15) {
                        "   - Variabilitas RENDAH (CV < 15%) - data relatif homogen dan konsisten.\n"
                    } else if (cv_val > 35) {
                        "   - Variabilitas TINGGI (CV > 35%) - data cukup heterogen dengan penyebaran luas.\n"
                    } else {
                        "   - Variabilitas SEDANG (15% ≤ CV ≤ 35%) - tingkat keragaman data normal.\n"
                    },
                    "\n5. **Kesimpulan Analisis:**\n",
                    "   Berdasarkan analisis statistik deskriptif komprehensif, variabel ", input$select_var,
                    " menunjukkan karakteristik distribusi dengan rentang nilai yang mencakup spektrum luas ",
                    "dalam konteks analisis kerentanan sosial. Data ini dapat digunakan untuk memahami ",
                    "pola sebaran indikator sosial-ekonomi di berbagai wilayah Indonesia.\n\n",
                    "**IMPLIKASI PRAKTIS:**\n",
                    "- Nilai rata-rata dapat dijadikan benchmarking untuk evaluasi tingkat kerentanan\n",
                    "- Tingkat variabilitas menunjukkan sejauh mana heterogenitas kondisi antar wilayah\n",
                    "- Informasi distribusi membantu dalam perumusan kebijakan yang tepat sasaran\n\n",
                    "**Tanggal Analisis:** ", format(Sys.Date(), "%d %B %Y"), "\n",
                    "**Sumber Data:** ALIVA Dashboard - Analisis Kerentanan Sosial Indonesia"
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
