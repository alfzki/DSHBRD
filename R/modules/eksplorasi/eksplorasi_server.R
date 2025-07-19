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
        # Update variable choices - reactive to data structure changes
        observe({
            # Create reactive dependency on data and data update counter
            req(values$sovi_data)
            data_counter <- values$data_update_counter # This creates a reactive dependency

            numeric_choices <- get_variable_choices(values$sovi_data, "numeric")
            updateSelectInput(session, "select_var", choices = numeric_choices)
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

        # Interactive Map visualization with social vulnerability data
        output$map_viz <- leaflet::renderLeaflet({
            req(input$select_var)

            # Read GeoJSON data for Indonesian districts
            geojson_path <- here::here("data", "indonesia_kabkota.geojson")

            if (!file.exists(geojson_path)) {
                # Fallback to basic map if GeoJSON not available
                return(
                    leaflet::leaflet() %>%
                        leaflet::addTiles() %>%
                        leaflet::setView(lng = 117.0, lat = -2.5, zoom = 5) %>%
                        leaflet::addMarkers(
                            lng = 117.0, lat = -2.5,
                            popup = "Data peta spasial tidak tersedia. Silakan unduh file GeoJSON Indonesia."
                        )
                )
            }

            tryCatch(
                {
                    # Read the GeoJSON file
                    indonesia_map <- sf::st_read(geojson_path, quiet = TRUE)

                    # Get the selected variable data
                    var_data <- values$sovi_data[[input$select_var]]

                    # Create sample spatial data (normally you'd join with actual district data)
                    # For demo purposes, create random data for each district
                    set.seed(123) # For reproducible results
                    n_districts <- nrow(indonesia_map)

                    # Simulate district values based on the variable's distribution
                    mean_val <- mean(var_data, na.rm = TRUE)
                    sd_val <- sd(var_data, na.rm = TRUE)
                    simulated_values <- rnorm(n_districts, mean = mean_val, sd = sd_val)

                    # Add the simulated data to the spatial data
                    indonesia_map$vulnerability_value <- simulated_values

                    # Create color palette
                    pal <- leaflet::colorNumeric(
                        palette = "YlOrRd",
                        domain = indonesia_map$vulnerability_value,
                        na.color = "transparent"
                    )

                    # Create labels for popups
                    labels <- sprintf(
                        "<strong>%s</strong><br/>
                     Nilai %s: <strong>%.3f</strong><br/>
                     <em>Catatan: Data ini disimulasikan untuk demonstrasi</em>",
                        indonesia_map$NAME_2, # District name
                        input$select_var,
                        indonesia_map$vulnerability_value
                    ) %>% lapply(htmltools::HTML)

                    # Create the leaflet map
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
                            title = paste("Nilai", input$select_var),
                            position = "bottomright"
                        ) %>%
                        leaflet::addControl(
                            html = paste0(
                                "<div style='background: white; padding: 10px; border-radius: 5px;'>",
                                "<h4>Peta Kerentanan Sosial Indonesia</h4>",
                                "<p><strong>Variabel:</strong> ", input$select_var, "</p>",
                                "<p><em>Catatan: Data spasial ini disimulasikan untuk tujuan demonstrasi.</em></p>",
                                "</div>"
                            ),
                            position = "topright"
                        )
                },
                error = function(e) {
                    # Error handling - show basic map
                    leaflet::leaflet() %>%
                        leaflet::addTiles() %>%
                        leaflet::setView(lng = 117.0, lat = -2.5, zoom = 5) %>%
                        leaflet::addMarkers(
                            lng = 117.0, lat = -2.5,
                            popup = paste("Error loading map data:", e$message)
                        )
                }
            )
        })

        # Download handlers
        output$download_summary <- downloadHandler(
            filename = function() {
                paste("eksplorasi_summary_", input$select_var, "_", Sys.Date(), ".pdf", sep = "")
            },
            content = function(file) {
                temp_rmd <- tempfile(fileext = ".Rmd")

                # Get current variable data
                var_data <- values$sovi_data[[input$select_var]]
                var_summary <- summary(var_data)

                writeLines(c(
                    "---",
                    "title: 'Laporan Eksplorasi Data'",
                    paste("subtitle: 'Analisis Variabel:", input$select_var, "'"),
                    "author: 'ALIVA Dashboard'",
                    "date: '`r Sys.Date()`'",
                    "output: pdf_document",
                    "---",
                    "",
                    paste("# Analisis Eksplorasi:", input$select_var),
                    "",
                    "## Ringkasan Statistik",
                    "",
                    "```{r, echo=FALSE}",
                    "summary_data <- c(",
                    paste("'Minimum' =", var_summary["Min."], ","),
                    paste("'Q1' =", var_summary["1st Qu."], ","),
                    paste("'Median' =", var_summary["Median"], ","),
                    paste("'Mean' =", var_summary["Mean"], ","),
                    paste("'Q3' =", var_summary["3rd Qu."], ","),
                    paste("'Maximum' =", var_summary["Max."]),
                    ")",
                    "knitr::kable(summary_data, col.names = 'Nilai')",
                    "```",
                    "",
                    "## Interpretasi",
                    "",
                    paste("Variabel", input$select_var, "menunjukkan karakteristik distribusi yang dapat dianalisis sebagai berikut:"),
                    "",
                    paste("- **Nilai rata-rata**: ", format_number(var_summary["Mean"])),
                    paste("- **Nilai median**: ", format_number(var_summary["Median"])),
                    paste("- **Rentang nilai**: ", format_number(var_summary["Min."]), " hingga ", format_number(var_summary["Max."])),
                    paste("- **Standar deviasi**: ", format_number(sd(var_data, na.rm = TRUE))),
                    "",
                    "Dataset berasal dari SUSENAS 2017, BPS-Statistics Indonesia."
                ), temp_rmd)

                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )

        output$download_plot <- downloadHandler(
            filename = function() {
                paste("eksplorasi_plot_", input$select_var, "_", Sys.Date(), ".png", sep = "")
            },
            content = function(file) {
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

                ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
            }
        )
    })
}