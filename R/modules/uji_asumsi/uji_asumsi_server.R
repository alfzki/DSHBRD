# Uji Asumsi Server Module
# Server logic for assumption testing functionality

#' Uji Asumsi Server Module
#'
#' Server logic for statistical assumption testing
#'
#' @param id Module ID for namespacing
#' @param values Reactive values object containing shared data
uji_asumsi_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        # Update variable choices - reactive to data structure changes
        observe({
            # Create reactive dependency on data and data update counter
            req(values$sovi_data)
            data_counter <- values$data_update_counter  # This creates a reactive dependency
            
            numeric_choices <- get_variable_choices(values$sovi_data, "numeric")
            categorical_choices <- get_variable_choices(values$sovi_data, "categorical")

            # Debug logging to console
            cat("Uji Asumsi: Updating variable choices (counter:", data_counter, ")...\n")
            cat("Categorical choices:", names(categorical_choices), "\n")

            updateSelectInput(session, "var_normal", choices = numeric_choices, selected = numeric_choices[1])
            updateSelectInput(session, "var_homogen", choices = numeric_choices, selected = numeric_choices[1])
            updateSelectInput(session, "group_homogen", choices = categorical_choices, selected = categorical_choices[1])
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

        # Normality interpretation using helper function
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

            # Use the interpretation helper function
            interpretation_text <- interpret_assumption_test(
                test_result = test_result,
                test_name = "normalitas",
                alpha = 0.05
            )

            # Convert to HTML with proper formatting
            interpretation_html <- gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", interpretation_text)
            interpretation_html <- gsub("\\n", "<br>", interpretation_html)

            HTML(paste0("<div style='padding: 15px; background-color: #f8f9fa; border-left: 4px solid #007bff; margin: 10px 0;'>",
                       interpretation_html,
                       "</div>"))
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

            # Use the interpretation helper function
            interpretation_text <- interpret_assumption_test(
                test_result = list(p.value = p_value, statistic = test_result$`F value`[1]),
                test_name = "homogenitas",
                alpha = 0.05
            )

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
                paste0("interpretasi_uji_asumsi_", Sys.Date(), ".docx")
            },
            content = function(file) {
                req(input$var_normal)
                
                # Get test results
                var_data <- values$sovi_data[[input$var_normal]]
                var_data <- var_data[!is.na(var_data)]
                
                if (length(var_data) > 5000) {
                    var_data <- sample(var_data, 5000)
                }
                
                if (length(var_data) >= 3) {
                    normality_result <- shapiro.test(var_data)
                    
                    # Generate interpretation text
                    interpretation_text <- interpret_assumption_test(
                        test_result = normality_result,
                        test_name = "normalitas",
                        alpha = 0.05
                    )
                    
                    # Create Word document using officer
                    doc <- officer::read_docx()
                    doc <- officer::body_add_par(doc, "Dashboard ALIVA", style = "heading 1")
                    doc <- officer::body_add_par(doc, "Interpretasi Uji Asumsi", style = "heading 2")
                    doc <- officer::body_add_par(doc, paste("Tanggal:", format(Sys.Date(), "%d %B %Y")))
                    doc <- officer::body_add_par(doc, paste("Variabel:", input$var_normal))
                    doc <- officer::body_add_par(doc, "")
                    doc <- officer::body_add_par(doc, interpretation_text)
                    
                    print(doc, target = file)
                } else {
                    # Create empty document if no data
                    doc <- officer::read_docx()
                    doc <- officer::body_add_par(doc, "Interpretasi Uji Asumsi")
                    doc <- officer::body_add_par(doc, "Data tidak cukup untuk melakukan uji asumsi.")
                    print(doc, target = file)
                }
            }
        )

        # Download report as PDF
        output$download_report_pdf <- downloadHandler(
            filename = function() {
                paste0("laporan_uji_asumsi_", Sys.Date(), ".pdf")
            },
            content = function(file) {
                temp_rmd <- tempfile(fileext = ".Rmd")
                
                # Get current test results
                normality_results <- NULL
                homogeneity_results <- NULL
                
                if (!is.null(input$var_normal) && validate_data(values$sovi_data, "Data SOVI")) {
                    var_data <- values$sovi_data[[input$var_normal]]
                    var_data <- var_data[!is.na(var_data)]
                    
                    if (length(var_data) >= 3) {
                        if (length(var_data) > 5000) {
                            var_data <- sample(var_data, 5000)
                        }
                        normality_results <- shapiro.test(var_data)
                    }
                }
                
                if (!is.null(input$var_homogen) && !is.null(input$group_homogen) && 
                    validate_data(values$sovi_data, "Data SOVI")) {
                    
                    group_var <- values$sovi_data[[input$group_homogen]]
                    if (length(unique(group_var)) >= 2) {
                        formula_str <- paste(input$var_homogen, "~", input$group_homogen)
                        formula_obj <- as.formula(formula_str)
                        
                        homogeneity_results <- tryCatch(
                            car::leveneTest(formula_obj, data = values$sovi_data),
                            error = function(e) NULL
                        )
                    }
                }

                # Generate dynamic content based on current test results
                normality_section <- ""
                homogeneity_section <- ""
                
                # Dynamic Normality Section
                if (!is.null(normality_results)) {
                    var_name <- input$var_normal
                    normality_interpretation <- interpret_assumption_test(
                        test_result = normality_results,
                        test_name = "normalitas",
                        alpha = 0.05
                    )
                    
                    normality_section <- paste0(
                        "### Hasil Uji Shapiro-Wilk\n\n",
                        "**Variabel yang diuji:** ", var_name, "\n\n",
                        "- **Statistik Uji (W):** ", round(normality_results$statistic, 4), "\n",
                        "- **P-value:** ", format(normality_results$p.value, scientific = TRUE), "\n",
                        "- **Alpha (α):** 0.05\n\n",
                        "### Interpretasi\n\n",
                        normality_interpretation, "\n\n"
                    )
                } else {
                    normality_section <- paste0(
                        "### Hasil Uji Shapiro-Wilk\n\n",
                        "Tidak ada data yang cukup untuk melakukan uji normalitas atau variabel belum dipilih.\n\n",
                        "### Interpretasi\n\n",
                        "Silakan pilih variabel numerik dan pastikan data memiliki minimal 3 observasi untuk melakukan uji normalitas.\n\n"
                    )
                }
                
                # Dynamic Homogeneity Section  
                if (!is.null(homogeneity_results)) {
                    var_name <- input$var_homogen
                    group_name <- input$group_homogen
                    homogeneity_interpretation <- interpret_assumption_test(
                        test_result = homogeneity_results,
                        test_name = "homogenitas",
                        alpha = 0.05
                    )
                    
                    homogeneity_section <- paste0(
                        "### Hasil Uji Levene\n\n",
                        "**Variabel yang diuji:** ", var_name, "\n",
                        "**Variabel pengelompokan:** ", group_name, "\n\n",
                        "- **Statistik Uji (F):** ", round(homogeneity_results$`F value`[1], 4), "\n",
                        "- **P-value:** ", format(homogeneity_results$`Pr(>F)`[1], scientific = TRUE), "\n",
                        "- **Alpha (α):** 0.05\n\n",
                        "### Interpretasi\n\n",
                        homogeneity_interpretation, "\n\n"
                    )
                } else {
                    homogeneity_section <- paste0(
                        "### Hasil Uji Levene\n\n",
                        "Tidak ada data yang cukup untuk melakukan uji homogenitas atau variabel belum dipilih.\n\n",
                        "### Interpretasi\n\n",
                        "Silakan pilih variabel numerik dan variabel pengelompokan (kategorikal) untuk melakukan uji homogenitas varians.\n\n"
                    )
                }
                
                # Generate conclusion based on results
                conclusion_text <- ""
                if (!is.null(normality_results) && !is.null(homogeneity_results)) {
                    conclusion_text <- paste0(
                        "Berdasarkan hasil uji asumsi yang telah dilakukan:\n\n",
                        "1. **Uji Normalitas**: ", 
                        ifelse(normality_results$p.value >= 0.05, 
                               "Data berdistribusi normal (asumsi terpenuhi)", 
                               "Data tidak berdistribusi normal (asumsi tidak terpenuhi)"), "\n",
                        "2. **Uji Homogenitas**: ",
                        ifelse(homogeneity_results$`Pr(>F)`[1] >= 0.05,
                               "Varians antar grup homogen (asumsi terpenuhi)",
                               "Varians antar grup tidak homogen (asumsi tidak terpenuhi)"), "\n\n",
                        "**Rekomendasi**: ",
                        ifelse(normality_results$p.value >= 0.05 && homogeneity_results$`Pr(>F)`[1] >= 0.05,
                               "Asumsi terpenuhi untuk melakukan uji parametrik.",
                               "Pertimbangkan menggunakan uji non-parametrik atau transformasi data.")
                    )
                } else if (!is.null(normality_results)) {
                    conclusion_text <- paste0(
                        "Berdasarkan uji normalitas yang telah dilakukan, ",
                        ifelse(normality_results$p.value >= 0.05,
                               "data berdistribusi normal sehingga memenuhi asumsi untuk uji parametrik.",
                               "data tidak berdistribusi normal sehingga perlu pertimbangan penggunaan uji non-parametrik.")
                    )
                } else if (!is.null(homogeneity_results)) {
                    conclusion_text <- paste0(
                        "Berdasarkan uji homogenitas yang telah dilakukan, ",
                        ifelse(homogeneity_results$`Pr(>F)`[1] >= 0.05,
                               "varians antar grup homogen sehingga memenuhi asumsi untuk uji parametrik.",
                               "varians antar grup tidak homogen sehingga perlu pertimbangan penggunaan uji non-parametrik.")
                    )
                } else {
                    conclusion_text <- "Silakan lakukan uji asumsi terlebih dahulu untuk mendapatkan rekomendasi analisis yang sesuai."
                }

                rmd_content <- paste0('---
title: "Laporan Uji Asumsi - Dashboard ALIVA"
author: "Dashboard ALIVA"
date: "`r format(Sys.Date(), \'%d %B %Y\')`"
output:
  pdf_document:
    latex_engine: xelatex
    keep_tex: false
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
```

# Laporan Uji Asumsi Statistik

## Informasi Analisis

**Tanggal Analisis:** `r format(Sys.Date(), \'%d %B %Y\')`

**Sumber Data:** SUSENAS 2017, BPS-Statistics Indonesia

---

## Uji Normalitas

', normality_section, '

---

## Uji Homogenitas

', homogeneity_section, '

---

## Kesimpulan

', conclusion_text, '

---

*Laporan ini dihasilkan secara otomatis oleh ALIVA Dashboard pada `r Sys.Date()`*
')

                writeLines(rmd_content, temp_rmd)
                
                # Render using the correct pattern to avoid pandoc error
                tryCatch({
                    # Suppress output during rendering
                    output_path <- suppressMessages(suppressWarnings(
                        rmarkdown::render(
                            input = temp_rmd, 
                            output_format = "pdf_document", 
                            quiet = TRUE,
                            output_dir = tempdir(),
                            clean = TRUE
                        )
                    ))
                    if (file.exists(output_path)) {
                        file.copy(output_path, file)
                        # Clean up temporary files
                        unlink(temp_rmd)
                        if (file.exists(output_path)) unlink(output_path)
                    } else {
                        stop("PDF output file not generated")
                    }
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
                paste0("laporan_uji_asumsi_", Sys.Date(), ".docx")
            },
            content = function(file) {
                temp_rmd <- tempfile(fileext = ".Rmd")
                
                # Get current test results (same as PDF version)
                normality_results <- NULL
                homogeneity_results <- NULL
                
                if (!is.null(input$var_normal) && validate_data(values$sovi_data, "Data SOVI")) {
                    var_data <- values$sovi_data[[input$var_normal]]
                    var_data <- var_data[!is.na(var_data)]
                    
                    if (length(var_data) >= 3) {
                        if (length(var_data) > 5000) {
                            var_data <- sample(var_data, 5000)
                        }
                        normality_results <- shapiro.test(var_data)
                    }
                }
                
                if (!is.null(input$var_homogen) && !is.null(input$group_homogen) && 
                    validate_data(values$sovi_data, "Data SOVI")) {
                    
                    group_var <- values$sovi_data[[input$group_homogen]]
                    if (length(unique(group_var)) >= 2) {
                        formula_str <- paste(input$var_homogen, "~", input$group_homogen)
                        formula_obj <- as.formula(formula_str)
                        
                        homogeneity_results <- tryCatch(
                            car::leveneTest(formula_obj, data = values$sovi_data),
                            error = function(e) NULL
                        )
                    }
                }
                
                # Generate dynamic content (same logic as PDF version)
                normality_section <- ""
                homogeneity_section <- ""
                
                if (!is.null(normality_results)) {
                    var_name <- input$var_normal
                    normality_interpretation <- interpret_assumption_test(
                        test_result = normality_results,
                        test_name = "normalitas",
                        alpha = 0.05
                    )
                    
                    normality_section <- paste0(
                        "### Hasil Uji Shapiro-Wilk\n\n",
                        "**Variabel yang diuji:** ", var_name, "\n\n",
                        "- **Statistik Uji (W):** ", round(normality_results$statistic, 4), "\n",
                        "- **P-value:** ", format(normality_results$p.value, scientific = TRUE), "\n",
                        "- **Alpha (α):** 0.05\n\n",
                        "### Interpretasi\n\n",
                        normality_interpretation, "\n\n"
                    )
                } else {
                    normality_section <- paste0(
                        "### Hasil Uji Shapiro-Wilk\n\n",
                        "Tidak ada data yang cukup untuk melakukan uji normalitas atau variabel belum dipilih.\n\n",
                        "### Interpretasi\n\n",
                        "Silakan pilih variabel numerik dan pastikan data memiliki minimal 3 observasi untuk melakukan uji normalitas.\n\n"
                    )
                }
                
                if (!is.null(homogeneity_results)) {
                    var_name <- input$var_homogen
                    group_name <- input$group_homogen
                    homogeneity_interpretation <- interpret_assumption_test(
                        test_result = homogeneity_results,
                        test_name = "homogenitas",
                        alpha = 0.05
                    )
                    
                    homogeneity_section <- paste0(
                        "### Hasil Uji Levene\n\n",
                        "**Variabel yang diuji:** ", var_name, "\n",
                        "**Variabel pengelompokan:** ", group_name, "\n\n",
                        "- **Statistik Uji (F):** ", round(homogeneity_results$`F value`[1], 4), "\n",
                        "- **P-value:** ", format(homogeneity_results$`Pr(>F)`[1], scientific = TRUE), "\n",
                        "- **Alpha (α):** 0.05\n\n",
                        "### Interpretasi\n\n",
                        homogeneity_interpretation, "\n\n"
                    )
                } else {
                    homogeneity_section <- paste0(
                        "### Hasil Uji Levene\n\n",
                        "Tidak ada data yang cukup untuk melakukan uji homogenitas atau variabel belum dipilih.\n\n",
                        "### Interpretasi\n\n",
                        "Silakan pilih variabel numerik dan variabel pengelompokan (kategorikal) untuk melakukan uji homogenitas varians.\n\n"
                    )
                }
                
                # Generate conclusion
                conclusion_text <- ""
                if (!is.null(normality_results) && !is.null(homogeneity_results)) {
                    conclusion_text <- paste0(
                        "Berdasarkan hasil uji asumsi yang telah dilakukan:\n\n",
                        "1. **Uji Normalitas**: ", 
                        ifelse(normality_results$p.value >= 0.05, 
                               "Data berdistribusi normal (asumsi terpenuhi)", 
                               "Data tidak berdistribusi normal (asumsi tidak terpenuhi)"), "\n",
                        "2. **Uji Homogenitas**: ",
                        ifelse(homogeneity_results$`Pr(>F)`[1] >= 0.05,
                               "Varians antar grup homogen (asumsi terpenuhi)",
                               "Varians antar grup tidak homogen (asumsi tidak terpenuhi)"), "\n\n",
                        "**Rekomendasi**: ",
                        ifelse(normality_results$p.value >= 0.05 && homogeneity_results$`Pr(>F)`[1] >= 0.05,
                               "Asumsi terpenuhi untuk melakukan uji parametrik.",
                               "Pertimbangkan menggunakan uji non-parametrik atau transformasi data.")
                    )
                } else if (!is.null(normality_results)) {
                    conclusion_text <- paste0(
                        "Berdasarkan uji normalitas yang telah dilakukan, ",
                        ifelse(normality_results$p.value >= 0.05,
                               "data berdistribusi normal sehingga memenuhi asumsi untuk uji parametrik.",
                               "data tidak berdistribusi normal sehingga perlu pertimbangan penggunaan uji non-parametrik.")
                    )
                } else if (!is.null(homogeneity_results)) {
                    conclusion_text <- paste0(
                        "Berdasarkan uji homogenitas yang telah dilakukan, ",
                        ifelse(homogeneity_results$`Pr(>F)`[1] >= 0.05,
                               "varians antar grup homogen sehingga memenuhi asumsi untuk uji parametrik.",
                               "varians antar grup tidak homogen sehingga perlu pertimbangan penggunaan uji non-parametrik.")
                    )
                } else {
                    conclusion_text <- "Silakan lakukan uji asumsi terlebih dahulu untuk mendapatkan rekomendasi analisis yang sesuai."
                }

                rmd_content <- paste0('---
title: "Laporan Uji Asumsi - Dashboard ALIVA"
author: "Dashboard ALIVA"
date: "`r format(Sys.Date(), \'%d %B %Y\')`"
output:
  word_document:
    reference_docx: NULL
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
```

# Laporan Uji Asumsi Statistik

## Informasi Analisis

**Tanggal Analisis:** `r format(Sys.Date(), \'%d %B %Y\')`

**Sumber Data:** SUSENAS 2017, BPS-Statistics Indonesia

---

## Uji Normalitas

', normality_section, '

---

## Uji Homogenitas

', homogeneity_section, '

---

## Kesimpulan

', conclusion_text, '

---

*Laporan ini dihasilkan secara otomatis oleh ALIVA Dashboard pada `r Sys.Date()`*
')

                writeLines(rmd_content, temp_rmd)
                
                # Render using the correct pattern to avoid pandoc error
                tryCatch({
                    output_path <- rmarkdown::render(input = temp_rmd, output_format = "word_document", quiet = TRUE)
                    file.copy(output_path, file)
                    # Clean up
                    unlink(temp_rmd)
                    if (file.exists(output_path)) unlink(output_path)
                }, error = function(e) {
                    # If Word generation fails, create a simple error document
                    writeLines(paste("Error generating Word report:", e$message), file)
                    showNotification("Word generation failed. Please try the PDF format.", type = "error")
                })
            }
        )
    })
}
