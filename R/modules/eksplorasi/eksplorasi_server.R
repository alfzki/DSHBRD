# Eksplorasi Server Module
# Comprehensive server logic for data exploration functionality

#' Eksplorasi Server Module
#'
#' Server logic for comprehensive data exploration features
#'
#' @param id Module ID for namespacing  
#' @param values Reactive values object containing shared data
eksplorasi_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        
        # Reactive values for storing plot objects
        current_plot <- reactiveVal(NULL)
        current_data_summary <- reactiveVal(NULL)
        
        # =================================================================
        # UPDATE VARIABLE CHOICES FOR ALL TABS
        # =================================================================
        
        observe({
            req(values$sovi_data)
            
            numeric_choices <- get_variable_choices(values$sovi_data, "numeric")
            categorical_choices <- get_variable_choices(values$sovi_data, "categorical")
            all_choices <- get_variable_choices(values$sovi_data, "all")
            
            # Update for univariate tab
            updateSelectInput(session, "univar_var", choices = numeric_choices)
            
            # Update for bivariate tab
            updateSelectInput(session, "bivar_x", choices = all_choices)
            updateSelectInput(session, "bivar_y", choices = numeric_choices)
            
            # Update for correlation tab
            updateCheckboxGroupInput(session, "corr_vars", choices = numeric_choices, 
                                   selected = numeric_choices[1:min(6, length(numeric_choices))])
            
            # Update for group analysis
            updateSelectInput(session, "group_var", choices = categorical_choices)
            updateSelectInput(session, "group_target", choices = numeric_choices)
            
            # Update for data tables
            updateSelectInput(session, "table_columns", choices = all_choices, 
                            selected = all_choices[1:min(10, length(all_choices))])
            
            # Update for maps
            updateSelectInput(session, "map_var", choices = numeric_choices)
        })
        
        # =================================================================
        # TAB 1: UNIVARIATE ANALYSIS
        # =================================================================
        
        # Generate univariate plots
        observeEvent(input$generate_univar, {
            req(input$univar_var, input$univar_plot_type)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return()
            
            var_data <- values$sovi_data[[input$univar_var]]
            var_name <- input$univar_var
            
            p <- switch(input$univar_plot_type,
                "histogram" = {
                    plot_ly(x = ~var_data, type = "histogram", nbinsx = input$hist_bins %||% 20) %>%
                        layout(title = paste("Histogram:", var_name),
                               xaxis = list(title = var_name),
                               yaxis = list(title = "Frequency"))
                },
                "density" = {
                    dens <- density(var_data, na.rm = TRUE)
                    plot_ly(x = ~dens$x, y = ~dens$y, type = "scatter", mode = "lines", fill = "tozeroy") %>%
                        layout(title = paste("Density Plot:", var_name),
                               xaxis = list(title = var_name),
                               yaxis = list(title = "Density"))
                },
                "boxplot" = {
                    plot_ly(y = ~var_data, type = "box", name = var_name) %>%
                        layout(title = paste("Boxplot:", var_name),
                               yaxis = list(title = var_name))
                },
                "qqplot" = {
                    qqnorm_data <- qqnorm(var_data, plot.it = FALSE)
                    plot_ly(x = ~qqnorm_data$x, y = ~qqnorm_data$y, type = "scatter", mode = "markers") %>%
                        add_lines(x = ~qqnorm_data$x, y = ~qqnorm_data$x, line = list(color = "red")) %>%
                        layout(title = paste("Q-Q Plot:", var_name),
                               xaxis = list(title = "Theoretical Quantiles"),
                               yaxis = list(title = "Sample Quantiles"))
                }
            )
            
            current_plot(p)
        })
        
        # Display univariate plot
        output$univar_plot <- renderPlotly({
            current_plot()
        })
        
        # Univariate summary statistics
        output$univar_summary <- renderText({
            req(input$univar_var)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return("Data not available")
            
            var_data <- values$sovi_data[[input$univar_var]]
            
            paste0(
                "STATISTIK DESKRIPTIF: ", input$univar_var, "\n",
                "========================================\n",
                "Mean: ", round(mean(var_data, na.rm = TRUE), 4), "\n",
                "Median: ", round(median(var_data, na.rm = TRUE), 4), "\n", 
                "Standard Deviation: ", round(sd(var_data, na.rm = TRUE), 4), "\n",
                "Min: ", round(min(var_data, na.rm = TRUE), 4), "\n",
                "Max: ", round(max(var_data, na.rm = TRUE), 4), "\n",
                "Q1: ", round(quantile(var_data, 0.25, na.rm = TRUE), 4), "\n",
                "Q3: ", round(quantile(var_data, 0.75, na.rm = TRUE), 4), "\n",
                "IQR: ", round(IQR(var_data, na.rm = TRUE), 4), "\n",
                "Skewness: ", round((3 * (mean(var_data, na.rm = TRUE) - median(var_data, na.rm = TRUE))) / sd(var_data, na.rm = TRUE), 4), "\n",
                "Missing Values: ", sum(is.na(var_data)), " (", round(sum(is.na(var_data))/length(var_data)*100, 2), "%)"
            )
        })
        
        # =================================================================
        # TAB 2: BIVARIATE ANALYSIS
        # =================================================================
        
        # Generate bivariate plots
        observeEvent(input$generate_bivar, {
            req(input$bivar_x, input$bivar_y, input$bivar_plot_type)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return()
            
            x_data <- values$sovi_data[[input$bivar_x]]
            y_data <- values$sovi_data[[input$bivar_y]]
            
            p <- switch(input$bivar_plot_type,
                "scatter" = {
                    p <- plot_ly(x = ~x_data, y = ~y_data, type = "scatter", mode = "markers") %>%
                        layout(title = paste(input$bivar_y, "vs", input$bivar_x),
                               xaxis = list(title = input$bivar_x),
                               yaxis = list(title = input$bivar_y))
                    
                    if (input$add_smooth && is.numeric(x_data)) {
                        # Add trend line
                        fit <- lm(y_data ~ x_data)
                        p <- p %>% add_lines(x = ~x_data, y = ~fitted(fit), name = "Trend Line")
                    }
                    p
                },
                "boxplot" = {
                    plot_ly(x = ~x_data, y = ~y_data, type = "box", color = ~x_data) %>%
                        layout(title = paste(input$bivar_y, "by", input$bivar_x),
                               xaxis = list(title = input$bivar_x),
                               yaxis = list(title = input$bivar_y))
                },
                "bar" = {
                    if (is.character(x_data) || is.factor(x_data)) {
                        agg_data <- aggregate(y_data, by = list(x_data), FUN = mean, na.rm = TRUE)
                        plot_ly(x = ~agg_data$Group.1, y = ~agg_data$x, type = "bar") %>%
                            layout(title = paste("Mean", input$bivar_y, "by", input$bivar_x),
                                   xaxis = list(title = input$bivar_x),
                                   yaxis = list(title = paste("Mean", input$bivar_y)))
                    } else {
                        plot_ly(x = ~x_data, y = ~y_data, type = "bar") %>%
                            layout(title = paste(input$bivar_y, "vs", input$bivar_x),
                                   xaxis = list(title = input$bivar_x),
                                   yaxis = list(title = input$bivar_y))
                    }
                },
                "line" = {
                    # Sort by x for line plot
                    order_idx <- order(x_data)
                    plot_ly(x = ~x_data[order_idx], y = ~y_data[order_idx], type = "scatter", mode = "lines+markers") %>%
                        layout(title = paste(input$bivar_y, "vs", input$bivar_x),
                               xaxis = list(title = input$bivar_x),
                               yaxis = list(title = input$bivar_y))
                }
            )
            
            current_plot(p)
        })
        
        # Display bivariate plot
        output$bivar_plot <- renderPlotly({
            current_plot()
        })
        
        # Correlation statistics
        output$correlation_stats <- renderText({
            req(input$bivar_x, input$bivar_y)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return("Data not available")
            
            x_data <- values$sovi_data[[input$bivar_x]]
            y_data <- values$sovi_data[[input$bivar_y]]
            
            if (!is.numeric(x_data) || !is.numeric(y_data)) {
                return("Correlation analysis requires both variables to be numeric")
            }
            
            pearson_cor <- cor(x_data, y_data, use = "complete.obs", method = "pearson")
            spearman_cor <- cor(x_data, y_data, use = "complete.obs", method = "spearman")
            
            # Correlation test
            cor_test <- cor.test(x_data, y_data)
            
            paste0(
                "ANALISIS KORELASI\n",
                "==================\n",
                "Pearson Correlation: ", round(pearson_cor, 4), "\n",
                "Spearman Correlation: ", round(spearman_cor, 4), "\n",
                "P-value: ", round(cor_test$p.value, 6), "\n",
                "95% CI: [", round(cor_test$conf.int[1], 4), ", ", round(cor_test$conf.int[2], 4), "]\n\n",
                "Interpretasi:\n",
                if (abs(pearson_cor) < 0.3) "Korelasi lemah" else
                if (abs(pearson_cor) < 0.7) "Korelasi sedang" else "Korelasi kuat", "\n",
                if (cor_test$p.value < 0.05) "Signifikan (p < 0.05)" else "Tidak signifikan (p ≥ 0.05)"
            )
        })
        
        # =================================================================
        # TAB 3: CORRELATION MATRIX & HEATMAP
        # =================================================================
        
        # Generate correlation heatmap
        observeEvent(input$generate_corr, {
            req(input$corr_vars, input$corr_method)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return()
            
            if (length(input$corr_vars) < 2) {
                showNotification("Pilih minimal 2 variabel untuk analisis korelasi", type = "warning")
                return()
            }
            
            # Calculate correlation matrix
            corr_data <- values$sovi_data %>% select(all_of(input$corr_vars))
            corr_matrix <- cor(corr_data, use = "complete.obs", method = input$corr_method)
            
            # Create heatmap
            p <- plot_ly(
                z = ~corr_matrix,
                x = ~colnames(corr_matrix),
                y = ~rownames(corr_matrix),
                type = "heatmap",
                colorscale = "RdBu",
                zmid = 0,
                zmin = -1,
                zmax = 1
            ) %>%
                layout(title = paste("Correlation Heatmap -", input$corr_method))
            
            current_plot(p)
        })
        
        # Display correlation heatmap
        output$correlation_heatmap <- renderPlotly({
            current_plot()
        })
        
        # Correlation table
        output$correlation_table <- DT::renderDT({
            req(input$corr_vars, input$corr_method)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return(NULL)
            
            if (length(input$corr_vars) < 2) return(NULL)
            
            corr_data <- values$sovi_data %>% select(all_of(input$corr_vars))
            corr_matrix <- cor(corr_data, use = "complete.obs", method = input$corr_method)
            
            # Convert to long format for table
            corr_df <- as.data.frame(corr_matrix)
            corr_df$Variable1 <- rownames(corr_df)
            corr_long <- corr_df %>%
                pivot_longer(-Variable1, names_to = "Variable2", values_to = "Correlation") %>%
                filter(Variable1 != Variable2) %>%
                arrange(desc(abs(Correlation)))
            
            DT::datatable(corr_long,
                options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE
            ) %>%
                DT::formatRound("Correlation", digits = 4) %>%
                DT::formatStyle("Correlation",
                    backgroundColor = DT::styleInterval(
                        c(-0.7, -0.3, 0.3, 0.7),
                        c("#d73027", "#fee08b", "#ffffff", "#fee08b", "#d73027")
                    )
                )
        })
        
        # =================================================================
        # TAB 4: GROUP ANALYSIS
        # =================================================================
        
        # Generate group analysis
        observeEvent(input$generate_group, {
            req(input$group_var, input$group_target, input$group_analysis_type)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return()
            
            group_data <- values$sovi_data[[input$group_var]]
            target_data <- values$sovi_data[[input$group_target]]
            
            p <- switch(input$group_analysis_type,
                "boxplot" = {
                    plot_ly(x = ~group_data, y = ~target_data, type = "box", color = ~group_data) %>%
                        layout(title = paste(input$group_target, "by", input$group_var),
                               xaxis = list(title = input$group_var),
                               yaxis = list(title = input$group_target))
                },
                "barchart" = {
                    agg_data <- aggregate(target_data, by = list(group_data), FUN = mean, na.rm = TRUE)
                    plot_ly(x = ~agg_data$Group.1, y = ~agg_data$x, type = "bar") %>%
                        layout(title = paste("Mean", input$group_target, "by", input$group_var),
                               xaxis = list(title = input$group_var),
                               yaxis = list(title = paste("Mean", input$group_target)))
                },
                "violin" = {
                    plot_ly(x = ~group_data, y = ~target_data, type = "violin", color = ~group_data) %>%
                        layout(title = paste(input$group_target, "by", input$group_var),
                               xaxis = list(title = input$group_var),
                               yaxis = list(title = input$group_target))
                },
                "descriptive" = {
                    # For descriptive, we'll show a bar chart of means
                    agg_data <- aggregate(target_data, by = list(group_data), FUN = mean, na.rm = TRUE)
                    plot_ly(x = ~agg_data$Group.1, y = ~agg_data$x, type = "bar") %>%
                        layout(title = paste("Mean", input$group_target, "by", input$group_var),
                               xaxis = list(title = input$group_var),
                               yaxis = list(title = paste("Mean", input$group_target)))
                }
            )
            
            current_plot(p)
        })
        
        # Display group plot
        output$group_plot <- renderPlotly({
            current_plot()
        })
        
        # Group statistics table
        output$group_stats_table <- DT::renderDT({
            req(input$group_var, input$group_target)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return(NULL)
            
            group_stats <- values$sovi_data %>%
                group_by(!!sym(input$group_var)) %>%
                summarise(
                    N = n(),
                    Mean = round(mean(!!sym(input$group_target), na.rm = TRUE), 4),
                    SD = round(sd(!!sym(input$group_target), na.rm = TRUE), 4),
                    Min = round(min(!!sym(input$group_target), na.rm = TRUE), 4),
                    Max = round(max(!!sym(input$group_target), na.rm = TRUE), 4),
                    Median = round(median(!!sym(input$group_target), na.rm = TRUE), 4),
                    Missing = sum(is.na(!!sym(input$group_target))),
                    .groups = "drop"
                )
            
            DT::datatable(group_stats,
                options = list(pageLength = 10, scrollX = TRUE, dom = "tp"),
                rownames = FALSE,
                caption = paste("Statistik", input$group_target, "berdasarkan", input$group_var)
            )
        })
        
        # =================================================================
        # TAB 5: INTERACTIVE DATA TABLES
        # =================================================================
        
        # Update interactive table
        observeEvent(input$update_table, {
            req(input$table_columns)
            
            # This will trigger the table rendering
        })
        
        # Interactive data table
        output$interactive_table <- DT::renderDT({
            if (!validate_data(values$sovi_data, "Data SOVI")) return(NULL)
            
            # Filter columns based on selection
            if (!is.null(input$table_columns) && length(input$table_columns) > 0) {
                data_to_show <- values$sovi_data %>% select(all_of(input$table_columns))
            } else {
                data_to_show <- values$sovi_data
            }
            
            # Filter only numeric if requested
            if (input$show_only_numeric) {
                numeric_cols <- sapply(data_to_show, is.numeric)
                data_to_show <- data_to_show[numeric_cols]
            }
            
            # Store for downloads
            current_data_summary(data_to_show)
            
            DT::datatable(data_to_show,
                options = list(
                    pageLength = input$table_rows %||% 25,
                    scrollX = TRUE,
                    dom = "Bfrtip",
                    buttons = c("copy", "csv", "excel", "pdf")
                ),
                class = "table-striped table-hover"
            ) %>%
                DT::formatRound(columns = sapply(data_to_show, is.numeric), digits = 4)
        })
        
        # Table information
        output$table_info <- renderText({
            if (!validate_data(values$sovi_data, "Data SOVI")) return("Data not available")
            
            display_data <- current_data_summary() %||% values$sovi_data
            
            paste0(
                "INFORMASI DATASET\n",
                "==================\n",
                "Total Baris: ", nrow(display_data), "\n",
                "Total Kolom: ", ncol(display_data), "\n",
                "Kolom Numerik: ", sum(sapply(display_data, is.numeric)), "\n",
                "Kolom Kategorik: ", sum(sapply(display_data, function(x) is.character(x) || is.factor(x))), "\n",
                "Total Missing Values: ", sum(is.na(display_data)), "\n",
                "Memory Usage: ", round(object.size(display_data) / 1024^2, 2), " MB"
            )
        })
        
        # =================================================================
        # TAB 6: THEMATIC MAPS
        # =================================================================
        
        # Generate thematic map
        observeEvent(input$generate_map, {
            req(input$map_var)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return()
            
            # This is a simplified example - in a real implementation,
            # you would need actual geographic data/shapefiles
            map_data <- values$sovi_data[[input$map_var]]
            
            # Create quantile breaks for color coding
            breaks <- quantile(map_data, probs = seq(0, 1, length.out = input$map_bins + 1), na.rm = TRUE)
            map_colors <- colorBin(input$map_color_scheme, domain = map_data, bins = breaks)
            
            # Create basic leaflet map with sample points
            # Note: This would be enhanced with actual geographic boundaries
            m <- leaflet() %>%
                addTiles() %>%
                setView(lng = 118.0148634, lat = -2.548926, zoom = 5)
            
            if (input$show_markers) {
                # Add sample markers (in real implementation, use actual coordinates)
                sample_indices <- sample(nrow(values$sovi_data), min(100, nrow(values$sovi_data)))
                sample_data <- values$sovi_data[sample_indices, ]
                
                # Generate random coordinates for demonstration
                set.seed(123)
                lngs <- runif(nrow(sample_data), min = 95, max = 141)
                lats <- runif(nrow(sample_data), min = -11, max = 6)
                
                m <- m %>%
                    addCircleMarkers(
                        lng = lngs,
                        lat = lats,
                        radius = 5,
                        color = ~map_colors(sample_data[[input$map_var]]),
                        stroke = FALSE,
                        fillOpacity = 0.8,
                        popup = ~paste0(
                            "District: ", sample_data$DISTRICTCODE, "<br/>",
                            input$map_var, ": ", round(sample_data[[input$map_var]], 2)
                        )
                    ) %>%
                    addLegend(
                        position = "bottomright",
                        pal = map_colors,
                        values = map_data,
                        title = input$map_var,
                        opacity = 0.7
                    )
            }
            
            # Store the map object for potential downloads
            current_plot(m)
        })
        
        # Display thematic map
        output$thematic_map <- renderLeaflet({
            current_plot() %||% leaflet() %>% addTiles() %>% 
                setView(lng = 118.0148634, lat = -2.548926, zoom = 5)
        })
        
        # Map summary
        output$map_summary <- renderText({
            req(input$map_var)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return("Data not available")
            
            map_data <- values$sovi_data[[input$map_var]]
            
            paste0(
                "RINGKASAN PETA TEMATIK\n",
                "======================\n",
                "Variabel: ", input$map_var, "\n",
                "Jumlah Lokasi: ", length(map_data), "\n",
                "Nilai Min: ", round(min(map_data, na.rm = TRUE), 4), "\n",
                "Nilai Max: ", round(max(map_data, na.rm = TRUE), 4), "\n",
                "Rata-rata: ", round(mean(map_data, na.rm = TRUE), 4), "\n",
                "Skema Warna: ", input$map_color_scheme, "\n",
                "Jumlah Kelas: ", input$map_bins
            )
        })
        
        # =================================================================
        # DOWNLOAD HANDLERS
        # =================================================================
        
        # Download plot as PNG
        output$download_plot_png <- downloadHandler(
            filename = function() {
                paste("ALIVA_Eksplorasi_Plot_", Sys.Date(), ".png", sep = "")
            },
            content = function(file) {
                if (!is.null(current_plot())) {
                    # For plotly objects
                    if (inherits(current_plot(), "plotly")) {
                        export(current_plot(), file = file, format = "png")
                    } else {
                        # For other plot types, create a generic message
                        png(file, width = 800, height = 600)
                        plot(1, main = "Plot not available for PNG export")
                        dev.off()
                    }
                }
            }
        )
        
        # Download plot as JPEG
        output$download_plot_jpg <- downloadHandler(
            filename = function() {
                paste("ALIVA_Eksplorasi_Plot_", Sys.Date(), ".jpg", sep = "")
            },
            content = function(file) {
                if (!is.null(current_plot())) {
                    if (inherits(current_plot(), "plotly")) {
                        export(current_plot(), file = file, format = "jpeg")
                    } else {
                        jpeg(file, width = 800, height = 600)
                        plot(1, main = "Plot not available for JPEG export")
                        dev.off()
                    }
                }
            }
        )
        
        # Download summary CSV
        output$download_summary_csv <- downloadHandler(
            filename = function() {
                paste("ALIVA_Eksplorasi_Summary_", Sys.Date(), ".csv", sep = "")
            },
            content = function(file) {
                data_to_export <- current_data_summary() %||% values$sovi_data
                write.csv(data_to_export, file, row.names = FALSE)
            }
        )
        
        # Download Excel
        output$download_data_excel <- downloadHandler(
            filename = function() {
                paste("ALIVA_Eksplorasi_Data_", Sys.Date(), ".xlsx", sep = "")
            },
            content = function(file) {
                # This would require the openxlsx package
                # For now, export as CSV
                data_to_export <- current_data_summary() %||% values$sovi_data
                write.csv(data_to_export, file, row.names = FALSE)
            }
        )
        
        # Download interpretation
        output$download_interpretation <- downloadHandler(
            filename = function() {
                paste("ALIVA_Eksplorasi_Interpretasi_", Sys.Date(), ".docx", sep = "")
            },
            content = function(file) {
                temp_rmd <- tempfile(fileext = ".Rmd")
                
                writeLines(c(
                    "---",
                    "title: 'ALIVA Dashboard - Hasil Eksplorasi Data'",
                    "author: 'ALIVA Analytics'",
                    "date: '`r Sys.Date()`'",
                    "output: word_document",
                    "---",
                    "",
                    "# Laporan Eksplorasi Data",
                    "",
                    "## Ringkasan Analisis",
                    paste("Analisis eksplorasi dilakukan pada:", Sys.time()),
                    "",
                    "## Fitur yang Dianalisis:",
                    "- Analisis Univariat (Histogram, Density, Boxplot, Q-Q Plot)",
                    "- Analisis Bivariat (Scatter, Box, Bar, Line Plot)",
                    "- Matriks Korelasi dan Heatmap", 
                    "- Analisis Kelompok",
                    "- Tabel Interaktif",
                    "- Peta Tematik",
                    "",
                    "Dataset berisi", nrow(values$sovi_data), "observasi dengan", ncol(values$sovi_data), "variabel."
                ), temp_rmd)
                
                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )
        
        # Download PDF report
        output$download_report_pdf <- downloadHandler(
            filename = function() {
                paste("ALIVA_Eksplorasi_Report_", Sys.Date(), ".pdf", sep = "")
            },
            content = function(file) {
                temp_rmd <- tempfile(fileext = ".Rmd")
                
                writeLines(c(
                    "---",
                    "title: 'ALIVA Dashboard - Laporan Eksplorasi Data'",
                    "author: 'ALIVA Analytics'", 
                    "date: '`r Sys.Date()`'",
                    "output: pdf_document",
                    "---",
                    "",
                    "# Laporan Komprehensif Eksplorasi Data",
                    "",
                    "## Dataset Information",
                    paste("- Observasi:", nrow(values$sovi_data)),
                    paste("- Variabel:", ncol(values$sovi_data)),
                    paste("- Variabel Numerik:", length(get_numeric_columns(values$sovi_data))),
                    paste("- Variabel Kategorik:", length(get_categorical_columns(values$sovi_data))),
                    "",
                    "## Fitur Eksplorasi Tersedia:",
                    "1. **Analisis Univariat**: Statistik deskriptif dan visualisasi distribusi",
                    "2. **Analisis Bivariat**: Hubungan antar variabel",
                    "3. **Matriks Korelasi**: Heatmap korelasi dengan berbagai metode",
                    "4. **Analisis Kelompok**: Perbandingan statistik antar grup",
                    "5. **Tabel Interaktif**: Eksplorasi data dengan filter dinamis",
                    "6. **Peta Tematik**: Visualisasi spasial data Indonesia"
                ), temp_rmd)
                
                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )
        
        # Download Word report
        output$download_report_word <- downloadHandler(
            filename = function() {
                paste("ALIVA_Eksplorasi_Report_", Sys.Date(), ".docx", sep = "")
            },
            content = function(file) {
                temp_rmd <- tempfile(fileext = ".Rmd")
                
                writeLines(c(
                    "---",
                    "title: 'ALIVA Dashboard - Laporan Eksplorasi Data'",
                    "author: 'ALIVA Analytics'",
                    "date: '`r Sys.Date()`'",
                    "output: word_document",
                    "---",
                    "",
                    "# Laporan Eksplorasi Data ALIVA",
                    "",
                    "Laporan ini berisi ringkasan lengkap eksplorasi data yang dilakukan menggunakan ALIVA Dashboard.",
                    "",
                    "## Fitur yang Digunakan:",
                    "",
                    "### 1. Analisis Univariat",
                    "- Histogram untuk distribusi frekuensi",
                    "- Density plot untuk bentuk distribusi",
                    "- Boxplot untuk deteksi outlier",
                    "- Q-Q plot untuk uji normalitas visual",
                    "",
                    "### 2. Analisis Bivariat", 
                    "- Scatter plot untuk korelasi",
                    "- Box plot berdasarkan kategori",
                    "- Bar chart untuk perbandingan",
                    "- Line plot untuk trend",
                    "",
                    "### 3. Matriks Korelasi",
                    "- Heatmap interaktif",
                    "- Metode Pearson, Spearman, Kendall",
                    "- Uji signifikansi korelasi",
                    "",
                    "### 4. Analisis Kelompok",
                    "- Statistik deskriptif per grup", 
                    "- Visualisasi perbandingan antar grup",
                    "",
                    "### 5. Eksplorasi Data Interaktif",
                    "- Filter data dinamis",
                    "- Tabel sortable dan searchable",
                    "",
                    "### 6. Peta Tematik",
                    "- Visualisasi geografis Indonesia",
                    "- Gradasi warna berdasarkan nilai variabel"
                ), temp_rmd)
                
                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )
        
        # Additional download handlers for filtered data
        output$download_filtered_csv <- downloadHandler(
            filename = function() {
                paste("ALIVA_Filtered_Data_", Sys.Date(), ".csv", sep = "")
            },
            content = function(file) {
                data_to_export <- current_data_summary() %||% values$sovi_data
                write.csv(data_to_export, file, row.names = FALSE)
            }
        )
        
        output$download_filtered_excel <- downloadHandler(
            filename = function() {
                paste("ALIVA_Filtered_Data_", Sys.Date(), ".xlsx", sep = "")
            },
            content = function(file) {
                data_to_export <- current_data_summary() %||% values$sovi_data
                # Export as CSV for now (would need openxlsx for real Excel)
                write.csv(data_to_export, file, row.names = FALSE)
            }
        )
    })
}
