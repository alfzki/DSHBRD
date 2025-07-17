# Server Modules for NusaStat Dashboard
# This file contains all server module logic

# Beranda Server Module
# =====================
beranda_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        # Welcome content
        output$welcome_content <- renderUI({
            tagList(
                h3("NusaStat: Dasbor Interaktif Analisis Kerentanan Sosial & Statistik Indonesia"),
                p("Selamat datang di NusaStat Dashboard! Aplikasi ini dirancang untuk membantu analisis statistik
          data kerentanan sosial Indonesia dengan fitur-fitur yang komprehensif dan mudah digunakan."),
                hr(),
                h4("Fitur Utama:"),
                tags$ul(
                    tags$li("Manajemen Data: Transformasi variabel kontinu menjadi kategorik"),
                    tags$li("Eksplorasi Data: Statistik deskriptif dan visualisasi interaktif"),
                    tags$li("Uji Asumsi: Uji normalitas dan homogenitas varians"),
                    tags$li("Statistik Inferensia: Uji beda rata-rata, proporsi, varians, dan ANOVA"),
                    tags$li("Regresi Linear Berganda: Model prediktif dengan uji asumsi lengkap"),
                    tags$li("Unduhan: Semua hasil dapat diunduh dalam format PDF, Word, atau CSV")
                ),
                hr(),
                h4("Sumber Data:"),
                p("Data yang digunakan dalam dashboard ini berasal dari SUSENAS (Survei Sosial Ekonomi Nasional)
          2017 yang diterbitkan oleh BPS-Statistics Indonesia. Data mencakup indikator kerentanan sosial
          dari 514 kabupaten/kota di Indonesia.")
            )
        })

        # Dataset information
        output$dataset_info <- renderUI({
            if (is.null(values$sovi_data)) {
                return(p("Data sedang dimuat..."))
            }

            tagList(
                h5("Informasi Dataset SOVI:"),
                p(paste("Jumlah observasi:", nrow(values$sovi_data))),
                p(paste("Jumlah variabel:", ncol(values$sovi_data))),
                p(paste("Variabel numerik:", length(get_numeric_columns(values$sovi_data)))),
                p(paste("Variabel kategorik:", length(get_categorical_columns(values$sovi_data)))),
                hr(),
                h5("Informasi Dataset Distance:"),
                p(paste("Jumlah observasi:", nrow(values$distance_data))),
                p(paste("Jumlah variabel:", ncol(values$distance_data)))
            )
        })

        # Metadata table
        output$metadata_table <- DT::renderDT({
            metadata <- data.frame(
                Variabel = c(
                    "kepadatan_penduduk", "rasio_ketergantungan", "persentase_lansia",
                    "persentase_balita", "persentase_disabilitas", "persentase_buta_huruf",
                    "persentase_kemiskinan", "persentase_pengangguran", "indeks_kesehatan",
                    "indeks_pendidikan", "indeks_ekonomi", "akses_listrik", "akses_air_bersih",
                    "akses_sanitasi", "sovi_score"
                ),
                Deskripsi = c(
                    "Kepadatan penduduk per km²", "Rasio ketergantungan (%)",
                    "Persentase populasi lansia", "Persentase populasi balita",
                    "Persentase populasi disabilitas", "Persentase buta huruf",
                    "Persentase kemiskinan", "Persentase pengangguran",
                    "Indeks kesehatan", "Indeks pendidikan", "Indeks ekonomi",
                    "Persentase akses listrik", "Persentase akses air bersih",
                    "Persentase akses sanitasi", "Skor kerentanan sosial"
                ),
                Satuan = c(
                    "jiwa/km²", "%", "%", "%", "%", "%", "%", "%",
                    "0-100", "0-100", "0-100", "%", "%", "%", "0-100"
                ),
                Sumber = rep("SUSENAS 2017", 15)
            )

            DT::datatable(metadata,
                options = list(pageLength = 10, scrollX = TRUE),
                class = "table-striped table-hover"
            )
        })

        # Download handler for dashboard info
        output$download_info <- downloadHandler(
            filename = "NusaStat_Info.pdf",
            content = function(file) {
                # Create a temporary R Markdown file
                temp_rmd <- tempfile(fileext = ".Rmd")

                rmd_content <- '---
title: "NusaStat Dashboard - Informasi Lengkap"
output: pdf_document
date: "`r Sys.Date()`"
---

# Tentang NusaStat Dashboard

NusaStat adalah dashboard interaktif untuk analisis kerentanan sosial dan statistik Indonesia yang dikembangkan menggunakan R Shiny.

## Fitur Utama

1. **Manajemen Data**: Transformasi variabel kontinu menjadi kategorik
2. **Eksplorasi Data**: Statistik deskriptif dan visualisasi
3. **Uji Asumsi**: Uji normalitas dan homogenitas
4. **Statistik Inferensia**: Berbagai uji statistik
5. **Regresi Linear Berganda**: Model prediktif lengkap

## Sumber Data

Data berasal dari SUSENAS 2017, BPS-Statistics Indonesia, mencakup 514 kabupaten/kota.

## Metadata Variabel

```{r echo=FALSE}
metadata <- data.frame(
  Variabel = c("kepadatan_penduduk", "rasio_ketergantungan", "persentase_lansia"),
  Deskripsi = c("Kepadatan penduduk per km²", "Rasio ketergantungan (%)", "Persentase populasi lansia"),
  Satuan = c("jiwa/km²", "%", "%")
)
knitr::kable(metadata)
```
'

                writeLines(rmd_content, temp_rmd)
                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )
    })
}

# Manajemen Data Server Module
# ============================
manajemen_data_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        # Update variable choices
        observe({
            if (!is.null(values$sovi_data)) {
                numeric_vars <- get_numeric_columns(values$sovi_data)
                updateSelectInput(session, "select_var", choices = numeric_vars)
            }
        })

        # Reactive values for this module
        processed_result <- reactiveVal(NULL)

        # Process categorization
        observeEvent(input$process_btn, {
            req(input$select_var, input$num_kategori, input$method)

            if (!validate_data(values$sovi_data, "Data SOVI")) {
                return()
            }

            var_name <- input$select_var
            n_cat <- input$num_kategori
            method <- input$method

            # Get the variable
            var_data <- values$sovi_data[[var_name]]

            # Create categories
            if (method == "interval") {
                breaks <- seq(min(var_data, na.rm = TRUE), max(var_data, na.rm = TRUE),
                    length.out = n_cat + 1
                )
                categories <- cut(var_data,
                    breaks = breaks, include.lowest = TRUE,
                    labels = paste("Kategori", 1:n_cat)
                )
            } else if (method == "quantile") {
                breaks <- quantile(var_data, probs = seq(0, 1, length.out = n_cat + 1), na.rm = TRUE)
                categories <- cut(var_data,
                    breaks = breaks, include.lowest = TRUE,
                    labels = paste("Kategori", 1:n_cat)
                )
            }

            # Create result data frame
            result_data <- values$sovi_data %>%
                mutate(
                    !!paste0(var_name, "_kategori") := categories
                ) %>%
                select(kode_wilayah, nama_wilayah, !!sym(var_name), !!sym(paste0(var_name, "_kategori")))

            processed_result(result_data)

            showNotification("Kategorisasi berhasil!", type = "message")
        })

        # Display result table
        output$result_table <- DT::renderDT({
            req(processed_result())

            DT::datatable(processed_result(),
                options = list(pageLength = 10, scrollX = TRUE),
                class = "table-striped table-hover"
            )
        })

        # Generate interpretation
        output$interpretation <- renderText({
            req(processed_result(), input$select_var, input$num_kategori, input$method)

            var_name <- input$select_var
            n_cat <- input$num_kategori
            method_text <- ifelse(input$method == "interval", "interval sama", "kuantil")

            # Get category summary
            cat_col <- paste0(var_name, "_kategori")
            cat_summary <- processed_result() %>%
                count(!!sym(cat_col)) %>%
                pull(n)

            paste0(
                "Hasil Kategorisasi Variabel: ", var_name, "\n",
                "Metode: ", method_text, "\n",
                "Jumlah kategori: ", n_cat, "\n",
                "Distribusi kategori:\n",
                paste(paste("Kategori", 1:n_cat, ":", cat_summary, "observasi"), collapse = "\n"),
                "\n\nInterpretasi: Variabel '", var_name, "' telah berhasil diubah menjadi ",
                n_cat, " kategori menggunakan metode ", method_text,
                ". Setiap kategori memiliki distribusi yang ",
                ifelse(input$method == "quantile", "relatif sama", "berdasarkan interval nilai yang sama"), "."
            )
        })

        # Download handlers
        output$download_result <- downloadHandler(
            filename = function() {
                paste0("kategorisasi_", input$select_var, "_", Sys.Date(), ".csv")
            },
            content = function(file) {
                req(processed_result())
                readr::write_csv(processed_result(), file)
            }
        )

        output$download_interpretation <- downloadHandler(
            filename = function() {
                paste0("interpretasi_", input$select_var, "_", Sys.Date(), ".txt")
            },
            content = function(file) {
                req(processed_result(), input$select_var)
                interpretation_text <- isolate({
                    paste0(
                        "Hasil Kategorisasi Variabel: ", input$select_var, "\n",
                        "Tanggal: ", Sys.Date(), "\n",
                        "Metode: ", ifelse(input$method == "interval", "interval sama", "kuantil"), "\n",
                        "Jumlah kategori: ", input$num_kategori, "\n\n",
                        "Interpretasi lengkap tersedia di dashboard."
                    )
                })
                writeLines(interpretation_text, file)
            }
        )
    })
}

# Eksplorasi Data Server Module
# =============================
eksplorasi_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        # Update variable choices
        observe({
            if (!is.null(values$sovi_data)) {
                numeric_vars <- get_numeric_columns(values$sovi_data)
                updateSelectInput(session, "select_var", choices = numeric_vars)
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

        # Interpretation of statistics
        output$interpretation_stats <- renderUI({
            req(input$select_var)
            if (!validate_data(values$sovi_data, "Data SOVI")) {
                return()
            }

            var_data <- values$sovi_data[[input$select_var]]
            var_summary <- summary(var_data)

            tagList(
                h4("Interpretasi Statistik Deskriptif:"),
                p(paste("Variabel", input$select_var, "memiliki:")),
                tags$ul(
                    tags$li(paste("Nilai rata-rata:", format_number(var_summary["Mean"]))),
                    tags$li(paste("Nilai median:", format_number(var_summary["Median"]))),
                    tags$li(paste("Nilai minimum:", format_number(var_summary["Min."]))),
                    tags$li(paste("Nilai maksimum:", format_number(var_summary["Max."]))),
                    tags$li(paste("Kuartil pertama (Q1):", format_number(var_summary["1st Qu."]))),
                    tags$li(paste("Kuartil ketiga (Q3):", format_number(var_summary["3rd Qu."])))
                ),
                p(paste(
                    "Berdasarkan statistik di atas, variabel", input$select_var,
                    "menunjukkan distribusi dengan rentang nilai dari",
                    format_number(var_summary["Min."]), "hingga",
                    format_number(var_summary["Max."]), "."
                ))
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
    })
}

# Uji Asumsi Server Module
# ========================
uji_asumsi_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        # Update variable choices
        observe({
            if (!is.null(values$sovi_data)) {
                numeric_vars <- get_numeric_columns(values$sovi_data)
                categorical_vars <- get_categorical_columns(values$sovi_data)

                updateSelectInput(session, "var_normal", choices = numeric_vars)
                updateSelectInput(session, "var_homogen", choices = numeric_vars)
                updateSelectInput(session, "group_homogen", choices = categorical_vars)
            }
        })

        # Normality test
        output$normality_test <- renderPrint({
            req(input$var_normal)
            if (!validate_data(values$sovi_data, "Data SOVI")) {
                return()
            }

            var_data <- values$sovi_data[[input$var_normal]]
            var_data <- var_data[!is.na(var_data)]

            if (length(var_data) < 3) {
                cat("Data tidak cukup untuk uji normalitas.")
                return()
            }

            if (length(var_data) > 5000) {
                var_data <- sample(var_data, 5000)
                cat("Catatan: Menggunakan sampel 5000 observasi untuk uji Shapiro-Wilk\n\n")
            }

            shapiro.test(var_data)
        })

        # Normality plot
        output$normality_plot <- renderPlot({
            req(input$var_normal)
            if (!validate_data(values$sovi_data, "Data SOVI")) {
                return()
            }

            var_data <- values$sovi_data[[input$var_normal]]

            par(mfrow = c(1, 2))

            # Q-Q plot
            qqnorm(var_data, main = paste("Q-Q Plot -", input$var_normal))
            qqline(var_data, col = "red")

            # Histogram with normal curve
            hist(var_data,
                freq = FALSE, main = paste("Histogram -", input$var_normal),
                xlab = input$var_normal, ylab = "Density"
            )
            curve(dnorm(x, mean = mean(var_data, na.rm = TRUE), sd = sd(var_data, na.rm = TRUE)),
                add = TRUE, col = "red", lwd = 2
            )
        })

        # Normality interpretation
        output$interpretation_normal <- renderUI({
            req(input$var_normal)
            if (!validate_data(values$sovi_data, "Data SOVI")) {
                return()
            }

            var_data <- values$sovi_data[[input$var_normal]]
            var_data <- var_data[!is.na(var_data)]

            if (length(var_data) < 3) {
                return(p("Data tidak cukup untuk interpretasi."))
            }

            if (length(var_data) > 5000) {
                var_data <- sample(var_data, 5000)
            }

            test_result <- shapiro.test(var_data)

            interpretation <- interpret_p_value(
                test_result$p.value,
                h0 = "Data berdistribusi normal",
                h1 = "Data tidak berdistribusi normal"
            )

            tagList(
                h4("Interpretasi Uji Normalitas:"),
                p(interpretation),
                p("Jika p-value < 0.05, maka data tidak berdistribusi normal dan mungkin memerlukan transformasi atau uji non-parametrik.")
            )
        })

        # Homogeneity test
        output$homogeneity_test <- renderPrint({
            req(input$var_homogen, input$group_homogen)
            if (!validate_data(values$sovi_data, "Data SOVI")) {
                return()
            }

            # Check if group variable has enough levels
            group_var <- values$sovi_data[[input$group_homogen]]
            if (length(unique(group_var)) < 2) {
                cat("Variabel grup harus memiliki minimal 2 kategori.")
                return()
            }

            # Perform Levene's test
            formula_str <- paste(input$var_homogen, "~", input$group_homogen)
            formula_obj <- as.formula(formula_str)

            tryCatch(
                {
                    car::leveneTest(formula_obj, data = values$sovi_data)
                },
                error = function(e) {
                    cat("Error dalam uji Levene:", e$message)
                }
            )
        })

        # Homogeneity interpretation
        output$interpretation_homogen <- renderUI({
            req(input$var_homogen, input$group_homogen)
            if (!validate_data(values$sovi_data, "Data SOVI")) {
                return()
            }

            group_var <- values$sovi_data[[input$group_homogen]]
            if (length(unique(group_var)) < 2) {
                return(p("Variabel grup harus memiliki minimal 2 kategori."))
            }

            formula_str <- paste(input$var_homogen, "~", input$group_homogen)
            formula_obj <- as.formula(formula_str)

            test_result <- tryCatch(
                {
                    car::leveneTest(formula_obj, data = values$sovi_data)
                },
                error = function(e) {
                    return(NULL)
                }
            )

            if (is.null(test_result)) {
                return(p("Tidak dapat melakukan uji homogenitas."))
            }

            p_value <- test_result$`Pr(>F)`[1]

            interpretation <- interpret_p_value(
                p_value,
                h0 = "Varians antar grup homogen",
                h1 = "Varians antar grup tidak homogen"
            )

            tagList(
                h4("Interpretasi Uji Homogenitas:"),
                p(interpretation),
                p("Jika p-value < 0.05, maka varians antar grup tidak homogen dan asumsi homogenitas dilanggar.")
            )
        })

        # Download handler
        output$download_results <- downloadHandler(
            filename = function() {
                paste0("uji_asumsi_", Sys.Date(), ".pdf")
            },
            content = function(file) {
                temp_rmd <- tempfile(fileext = ".Rmd")

                rmd_content <- '---
title: "Hasil Uji Asumsi"
output: pdf_document
date: "`r Sys.Date()`"
---

# Uji Asumsi Statistik

## Uji Normalitas dan Homogenitas

Hasil uji asumsi untuk analisis statistik.

## Interpretasi

Berdasarkan hasil uji, dapat disimpulkan apakah data memenuhi asumsi normalitas dan homogenitas.
'

                writeLines(rmd_content, temp_rmd)
                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )
    })
}

# Uji Beda Rata-Rata Server Module
# =================================
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

# Continue with other server modules...
# The remaining modules are implemented in additional_server_modules.R
