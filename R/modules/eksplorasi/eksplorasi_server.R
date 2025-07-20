# Eksplorasi Server Module
# Logic server komprehensif untuk fungsionalitas eksplorasi data

# Load helper functions
source(file.path("R", "modules", "eksplorasi", "eksplorasi_helpers.R"), local = TRUE)

#' Modul Server Eksplorasi
#'
#' Logic server untuk fitur eksplorasi data komprehensif
#'
#' @param id ID modul untuk namespacing  
#' @param values Objek reactive values berisi data bersama
eksplorasi_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        
        # Reactive values untuk menyimpan objek plot
        current_plot <- reactiveVal(NULL)
        current_data_summary <- reactiveVal(NULL)
        
        # =================================================================
        # UPDATE PILIHAN VARIABEL UNTUK SEMUA TAB
        # =================================================================
        
        observe({
            req(values$sovi_data)
            
            numeric_choices <- get_variable_choices(values$sovi_data, "numeric")
            categorical_choices <- get_variable_choices(values$sovi_data, "categorical")
            all_choices <- get_variable_choices(values$sovi_data, "all")
            
            # Update untuk tab univariat
            updateSelectInput(session, "univar_var", choices = numeric_choices)
            
            # Update untuk tab bivariat
            updateSelectInput(session, "bivar_x", choices = all_choices)
            updateSelectInput(session, "bivar_y", choices = numeric_choices)
            
            # Update untuk tab korelasi
            updateCheckboxGroupInput(session, "corr_vars", choices = numeric_choices, 
                                   selected = numeric_choices[1:min(6, length(numeric_choices))])
            
            # Update untuk analisis kelompok
            updateSelectInput(session, "group_var", choices = categorical_choices)
            updateSelectInput(session, "group_target", choices = numeric_choices)
            
            # Update untuk tabel data
            updateSelectInput(session, "table_columns", choices = all_choices, 
                            selected = all_choices[1:min(10, length(all_choices))])
            
            # Update untuk peta
            updateSelectInput(session, "map_var", choices = numeric_choices)
        })
        
        # =================================================================
        # TAB 1: ANALISIS UNIVARIAT
        # =================================================================
        
        # Membuat plot univariat
        observeEvent(input$generate_univar, {
            req(input$univar_var, input$univar_plot_type)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return()
            
            var_data <- values$sovi_data[[input$univar_var]]
            var_name <- input$univar_var
            
            # Menggunakan helper function untuk membuat plot
            p <- create_univariate_plot(var_data, var_name, input$univar_plot_type, input$hist_bins %||% 20)
            current_plot(p)
        })
        
        # Menampilkan plot univariat
        output$univar_plot <- renderPlotly({
            current_plot()
        })
        
        # Statistik ringkasan univariat
        output$univar_summary <- renderText({
            req(input$univar_var)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return("Data tidak tersedia")
            
            var_data <- values$sovi_data[[input$univar_var]]
            
            # Menggunakan helper function untuk menghitung ringkasan
            calculate_univariate_summary(var_data, input$univar_var)
        })
        
        # =================================================================
        # TAB 2: ANALISIS BIVARIAT
        # =================================================================
        
        # Membuat plot bivariat
        observeEvent(input$generate_bivar, {
            req(input$bivar_x, input$bivar_y, input$bivar_plot_type)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return()
            
            x_data <- values$sovi_data[[input$bivar_x]]
            y_data <- values$sovi_data[[input$bivar_y]]
            x_name <- input$bivar_x
            y_name <- input$bivar_y
            
            # Menggunakan helper function untuk plot dengan penanganan khusus untuk scatter dengan smooth
            if (input$bivar_plot_type == "scatter" && input$add_smooth && is.numeric(x_data)) {
                p <- create_bivariate_plot(x_data, y_data, x_name, y_name, "smooth")
            } else {
                # Untuk plot tipe lain, gunakan implementasi khusus sementara
                p <- switch(input$bivar_plot_type,
                    "scatter" = create_bivariate_plot(x_data, y_data, x_name, y_name, "scatter"),
                    "line" = create_bivariate_plot(x_data, y_data, x_name, y_name, "line"),
                    "boxplot" = {
                        plot_ly(x = ~x_data, y = ~y_data, type = "box", color = ~x_data) %>%
                            layout(title = paste(y_name, "berdasarkan", x_name),
                                   xaxis = list(title = x_name),
                                   yaxis = list(title = y_name))
                    },
                    "bar" = {
                        if (is.character(x_data) || is.factor(x_data)) {
                            agg_data <- aggregate(y_data, by = list(x_data), FUN = mean, na.rm = TRUE)
                            plot_ly(x = ~agg_data$Group.1, y = ~agg_data$x, type = "bar") %>%
                                layout(title = paste("Rerata", y_name, "berdasarkan", x_name),
                                       xaxis = list(title = x_name),
                                       yaxis = list(title = paste("Rerata", y_name)))
                        } else {
                            plot_ly(x = ~x_data, y = ~y_data, type = "bar") %>%
                                layout(title = paste(y_name, "vs", x_name),
                                       xaxis = list(title = x_name),
                                       yaxis = list(title = y_name))
                        }
                    }
                )
            }
            
            current_plot(p)
        })
        
        # Menampilkan plot bivariat
        output$bivar_plot <- renderPlotly({
            current_plot()
        })
        
        # Statistik korelasi
        output$correlation_stats <- renderText({
            req(input$bivar_x, input$bivar_y)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return("Data tidak tersedia")
            
            x_data <- values$sovi_data[[input$bivar_x]]
            y_data <- values$sovi_data[[input$bivar_y]]
            
            if (!is.numeric(x_data) || !is.numeric(y_data)) {
                return("Analisis korelasi memerlukan kedua variabel bersifat numerik")
            }
            
            pearson_cor <- cor(x_data, y_data, use = "complete.obs", method = "pearson")
            spearman_cor <- cor(x_data, y_data, use = "complete.obs", method = "spearman")
            
            # Uji korelasi
            cor_test <- cor.test(x_data, y_data)
            
            paste0(
                "ANALISIS KORELASI\n",
                "==================\n",
                "Korelasi Pearson: ", round(pearson_cor, 4), "\n",
                "Korelasi Spearman: ", round(spearman_cor, 4), "\n",
                "Nilai P: ", round(cor_test$p.value, 6), "\n",
                "95% CI: [", round(cor_test$conf.int[1], 4), ", ", round(cor_test$conf.int[2], 4), "]\n\n",
                "Interpretasi:\n",
                if (abs(pearson_cor) < 0.3) "Korelasi lemah" else
                if (abs(pearson_cor) < 0.7) "Korelasi sedang" else "Korelasi kuat", "\n",
                if (cor_test$p.value < 0.05) "Signifikan (p < 0.05)" else "Tidak signifikan (p ≥ 0.05)"
            )
        })
        
        # =================================================================
        # TAB 3: MATRIKS KORELASI & HEATMAP
        # =================================================================
        
        # Membuat heatmap korelasi
        observeEvent(input$generate_corr, {
            req(input$corr_vars, input$corr_method)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return()
            
            if (length(input$corr_vars) < 2) {
                showNotification("Pilih minimal 2 variabel untuk analisis korelasi", type = "warning")
                return()
            }
            
            # Menggunakan helper function untuk membuat heatmap korelasi
            p <- create_correlation_heatmap(values$sovi_data, input$corr_vars, input$corr_method)
            current_plot(p)
        })
        
        # Menampilkan heatmap korelasi
        output$correlation_heatmap <- renderPlotly({
            current_plot()
        })
        
        # Tabel korelasi
        output$correlation_table <- DT::renderDT({
            req(input$corr_vars, input$corr_method)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return(NULL)
            
            if (length(input$corr_vars) < 2) return(NULL)
            
            corr_data <- values$sovi_data %>% select(all_of(input$corr_vars))
            corr_matrix <- cor(corr_data, use = "complete.obs", method = input$corr_method)
            
            # Konversi ke format panjang untuk tabel
            corr_df <- as.data.frame(corr_matrix)
            corr_df$Variabel1 <- rownames(corr_df)
            corr_long <- corr_df %>%
                pivot_longer(-Variabel1, names_to = "Variabel2", values_to = "Korelasi") %>%
                filter(Variabel1 != Variabel2) %>%
                arrange(desc(abs(Korelasi)))
            
            DT::datatable(corr_long,
                options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE
            ) %>%
                DT::formatRound("Korelasi", digits = 4) %>%
                DT::formatStyle("Korelasi",
                    backgroundColor = DT::styleInterval(
                        c(-0.7, -0.3, 0.3, 0.7),
                        c("#d73027", "#fee08b", "#ffffff", "#fee08b", "#d73027")
                    )
                )
        })
        
        # =================================================================
        # TAB 4: ANALISIS KELOMPOK
        # =================================================================
        
        # Membuat analisis kelompok
        observeEvent(input$generate_group, {
            req(input$group_var, input$group_target, input$group_analysis_type)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return()
            
            # Menggunakan helper function untuk analisis kelompok
            result <- create_group_analysis(values$sovi_data, input$group_var, input$group_target, input$group_analysis_type)
            
            if (input$group_analysis_type == "summary") {
                # Untuk summary, simpan data di current_data_summary
                current_data_summary(result)
            } else {
                # Untuk plot, simpan di current_plot
                current_plot(result)
            }
        })
        
        # Menampilkan plot kelompok
        output$group_plot <- renderPlotly({
            current_plot()
        })
        
        # Tabel statistik kelompok
        output$group_stats_table <- DT::renderDT({
            req(input$group_var, input$group_target)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return(NULL)
            
            # Jika ada data summary dari helper, gunakan itu, otherwise buat baru
            if (!is.null(current_data_summary())) {
                group_stats <- current_data_summary()
            } else {
                group_stats <- values$sovi_data %>%
                    group_by(!!sym(input$group_var)) %>%
                    summarise(
                        N = n(),
                        Rerata = round(mean(!!sym(input$group_target), na.rm = TRUE), 4),
                        SD = round(sd(!!sym(input$group_target), na.rm = TRUE), 4),
                        Min = round(min(!!sym(input$group_target), na.rm = TRUE), 4),
                        Max = round(max(!!sym(input$group_target), na.rm = TRUE), 4),
                        Median = round(median(!!sym(input$group_target), na.rm = TRUE), 4),
                        Missing = sum(is.na(!!sym(input$group_target))),
                        .groups = "drop"
                    )
            }
            
            DT::datatable(group_stats,
                options = list(pageLength = 10, scrollX = TRUE, dom = "tp"),
                rownames = FALSE,
                caption = paste("Statistik", input$group_target, "berdasarkan", input$group_var)
            )
        })
        
        # =================================================================
        # TAB 5: TABEL DATA INTERAKTIF
        # =================================================================
        
        # Update tabel interaktif
        observeEvent(input$update_table, {
            req(input$table_columns)
            
            # Ini akan memicu rendering tabel
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
        # TAB 6: PETA TEMATIK
        # =================================================================
        
        # Membuat peta tematik
        observeEvent(input$generate_map, {
            req(input$map_var)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return()
            
            # Menggunakan helper function untuk placeholder peta
            p <- create_thematic_map_placeholder(values$sovi_data, input$map_var)
            current_plot(p)
                
        })
        
        # Menampilkan peta tematik  
        output$thematic_map <- renderPlotly({
            current_plot()
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
        # HANDLER DOWNLOAD
        # =================================================================
        
        # Download plot sebagai PNG
        output$download_plot_png <- downloadHandler(
            filename = function() {
                create_download_filename("ALIVA_Eksplorasi_Plot", ".png")
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
        
        # Download plot sebagai JPEG
        output$download_plot_jpg <- downloadHandler(
            filename = function() {
                create_download_filename("ALIVA_Eksplorasi_Plot", ".jpg")
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
        
        # Download ringkasan CSV
        output$download_summary_csv <- downloadHandler(
            filename = function() {
                create_download_filename("ALIVA_Eksplorasi_Summary", ".csv")
            },
            content = function(file) {
                data_to_export <- current_data_summary() %||% values$sovi_data
                write.csv(data_to_export, file, row.names = FALSE)
            }
        )
        
        # Download Excel
        output$download_data_excel <- downloadHandler(
            filename = function() {
                create_download_filename("ALIVA_Eksplorasi_Data", ".xlsx")
            },
            content = function(file) {
                # Ini akan memerlukan package openxlsx
                # Untuk sementara, export sebagai CSV
                data_to_export <- current_data_summary() %||% values$sovi_data
                write.csv(data_to_export, file, row.names = FALSE)
            }
        )
        
        # Download interpretasi
        output$download_interpretation <- downloadHandler(
            filename = function() {
                create_download_filename("ALIVA_Eksplorasi_Interpretasi", ".docx")
            },
            content = function(file) {
                temp_rmd <- tempfile(fileext = ".Rmd")
                
                # Menggunakan helper function untuk konten laporan
                report_content <- create_report_content(values$sovi_data, "interpretation")
                
                writeLines(c(
                    "---",
                    "title: 'ALIVA Dashboard - Hasil Eksplorasi Data'",
                    "author: 'ALIVA Analytics'",
                    "date: '`r Sys.Date()`'",
                    "output: word_document",
                    "---",
                    "",
                    report_content
                ), temp_rmd)
                
                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )
        
        # Download laporan PDF
        output$download_report_pdf <- downloadHandler(
            filename = function() {
                create_download_filename("ALIVA_Eksplorasi_Report", ".pdf")
            },
            content = function(file) {
                temp_rmd <- tempfile(fileext = ".Rmd")
                
                # Menggunakan helper function untuk konten laporan
                report_content <- create_report_content(values$sovi_data, "pdf")
                
                writeLines(c(
                    "---",
                    "title: 'ALIVA Dashboard - Laporan Eksplorasi Data'",
                    "author: 'ALIVA Analytics'",
                    "date: '`r Sys.Date()`'",
                    "output: pdf_document",
                    "---",
                    "",
                    report_content
                ), temp_rmd)
                
                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )
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
                create_download_filename("ALIVA_Filtered_Data", ".xlsx")
            },
            content = function(file) {
                data_to_export <- current_data_summary() %||% values$sovi_data
                # Export sebagai CSV untuk sementara (akan perlu openxlsx untuk Excel asli)
                write.csv(data_to_export, file, row.names = FALSE)
            }
        )
    })
}
