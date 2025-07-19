# Manajemen Data Server Module
# Server logic for data management functionality

#' Manajemen Data Server Module
#'
#' Server logic for data management and categorization features
#'
#' @param id Module ID for namespacing
#' @param values Reactive values object containing shared data
manajemen_data_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        # Update variable choices - reactive to data structure changes
        observe({
            # Create reactive dependency on data and data update counter
            req(values$sovi_data)
            data_counter <- values$data_update_counter # This creates a reactive dependency

            numeric_choices <- get_variable_choices(values$sovi_data, "numeric")
            updateSelectInput(session, "select_var", choices = numeric_choices, selected = numeric_choices[1])
        })

        # Reactive values for this module
        processed_result <- reactiveVal(NULL)

        # Control conditional panels
        output$show_save_options <- reactive({
            !is.null(processed_result())
        })
        outputOptions(output, "show_save_options", suspendWhenHidden = FALSE)

        output$show_downloads <- reactive({
            !is.null(processed_result())
        })
        outputOptions(output, "show_downloads", suspendWhenHidden = FALSE)

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
                select("DISTRICTCODE", !!sym(var_name), !!sym(paste0(var_name, "_kategori")))

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

        # Initialize interpretation output with safe default
        interpretation_text <- reactiveVal("Klik 'Proses Kategorisasi' untuk melihat interpretasi hasil.")

        output$interpretation <- renderText({
            interpretation_text()
        })

        # Update interpretation only after successful categorization
        observeEvent(processed_result(), {
            if (!is.null(processed_result()) && !is.null(input$select_var)) {
                var_name <- input$select_var
                cat_col <- paste0(var_name, "_kategori")

                if (cat_col %in% names(processed_result())) {
                    n_cat <- input$num_kategori
                    method_text <- ifelse(input$method == "interval", "interval sama", "kuantil")

                    # Get category summary using base R
                    cat_data <- processed_result()[[cat_col]]
                    cat_summary <- table(cat_data)

                    interpretation_text(paste0(
                        "Hasil Kategorisasi Variabel: ", var_name, "\n",
                        "Metode: ", method_text, "\n",
                        "Jumlah kategori: ", n_cat, "\n",
                        "Distribusi kategori:\n",
                        paste(paste(names(cat_summary), ":", as.numeric(cat_summary), "observasi"), collapse = "\n"),
                        "\n\nInterpretasi: Variabel '", var_name, "' telah berhasil diubah menjadi ",
                        n_cat, " kategori menggunakan metode ", method_text,
                        ". Setiap kategori memiliki distribusi yang ",
                        ifelse(input$method == "quantile", "relatif sama", "berdasarkan interval nilai yang sama"), "."
                    ))
                }
            }
        })

        # Reset interpretation when variable changes
        observeEvent(input$select_var, {
            interpretation_text("Klik 'Proses Kategorisasi' untuk melihat interpretasi hasil.")
        })

        # Save categorized variable to main dataset
        observeEvent(input$save_variable, {
            req(processed_result(), input$new_var_name)

            # Validate variable name
            new_var_name <- trimws(input$new_var_name)
            if (new_var_name == "" || !grepl("^[a-zA-Z][a-zA-Z0-9_]*$", new_var_name)) {
                showNotification("Nama variabel harus dimulai dengan huruf dan hanya mengandung huruf, angka, dan underscore.",
                    type = "error", duration = 5
                )
                return()
            }

            # Check if variable name already exists
            if (new_var_name %in% names(values$sovi_data)) {
                showModal(modalDialog(
                    title = "Konfirmasi Penggantian",
                    paste("Variabel", new_var_name, "sudah ada. Apakah Anda ingin menggantinya?"),
                    footer = tagList(
                        modalButton("Batal"),
                        actionButton(session$ns("confirm_replace"), "Ganti", class = "btn-warning")
                    )
                ))
                return()
            }

            # Save to main dataset
            save_categorized_variable(new_var_name)
        })

        # Confirm replace existing variable
        observeEvent(input$confirm_replace, {
            req(input$new_var_name)
            removeModal()
            save_categorized_variable(trimws(input$new_var_name))
        })

        # Helper function to save categorized variable
        save_categorized_variable <- function(var_name) {
            req(processed_result())

            # Get the categorized column
            original_var <- input$select_var
            cat_col_name <- paste0(original_var, "_kategori")

            if (cat_col_name %in% names(processed_result())) {
                # Add to main dataset as a factor variable
                categorized_data <- processed_result()[[cat_col_name]]

                # Use isolate to prevent reactive loops and ensure atomic update
                isolate({
                    # Create completely new data frame to force Shiny to recognize change
                    current_data <- values$sovi_data
                    new_data <- current_data # Create a copy
                    new_data[[var_name]] <- as.factor(categorized_data)

                    # Atomic update - assign the entire data frame
                    values$sovi_data <- new_data

                    # CRITICAL: Save user variable to app state for auto-restoration
                    values$user_created_vars[[var_name]] <- as.factor(categorized_data)
                    cat("MANAJEMEN DATA: Saved user variable to app state:", var_name, "\n")

                    # Save user variable to global state for preservation
                    if (!exists(".app_state", envir = .GlobalEnv)) {
                        .GlobalEnv$.app_state <- list(user_variables = list())
                    }
                    .GlobalEnv$.app_state$user_variables[[var_name]] <- as.factor(categorized_data)
                    cat("SAVED TO GLOBAL STATE: Variable", var_name, "with", length(categorized_data), "values\n")

                    # Debug logging
                    cat("Manajemen Data: Saved new categorical variable:", var_name, "\n")
                    cat("Data columns after save:", paste(names(values$sovi_data), collapse = ", "), "\n")
                    cat("New variable class:", class(values$sovi_data[[var_name]]), "\n")
                    cat("Current categorical variables:", names(get_variable_choices(values$sovi_data, "categorical")), "\n")
                })

                # Increment counter AFTER data is updated to ensure proper sequencing
                values$data_update_counter <- values$data_update_counter + 1
                cat("Data update counter incremented to:", values$data_update_counter, "\n")

                showNotification(paste("Variabel", var_name, "berhasil disimpan ke dataset sebagai variabel kategorik!"),
                    type = "message", duration = 5
                )

                # Clear the input
                updateTextInput(session, "new_var_name", value = "")
            } else {
                showNotification("Error: Data kategorisasi tidak ditemukan.", type = "error", duration = 5)
            }
        }

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
                paste0("interpretasi_", input$select_var, "_", Sys.Date(), ".docx")
            },
            content = function(file) {
                req(processed_result(), input$select_var)

                var_name <- input$select_var
                n_cat <- input$num_kategori
                method_text <- ifelse(input$method == "interval", "interval sama", "kuantil")

                # Get category summary
                cat_col <- paste0(var_name, "_kategori")
                if (cat_col %in% names(processed_result())) {
                    cat_data <- processed_result()[[cat_col]]
                    cat_summary <- table(cat_data)

                    # Create Word document using officer
                    doc <- officer::read_docx()
                    doc <- officer::body_add_par(doc, "Dashboard ALIVA", style = "heading 1")
                    doc <- officer::body_add_par(doc, "Interpretasi Kategorisasi Data", style = "heading 2")
                    doc <- officer::body_add_par(doc, paste("Tanggal:", format(Sys.Date(), "%d %B %Y")))
                    doc <- officer::body_add_par(doc, paste("Variabel:", var_name))
                    doc <- officer::body_add_par(doc, paste("Metode:", method_text))
                    doc <- officer::body_add_par(doc, paste("Jumlah kategori:", n_cat))
                    doc <- officer::body_add_par(doc, "")
                    doc <- officer::body_add_par(doc, "Distribusi Kategori:", style = "heading 3")

                    for (i in 1:length(cat_summary)) {
                        doc <- officer::body_add_par(doc, paste(names(cat_summary)[i], ":", as.numeric(cat_summary)[i], "observasi"))
                    }

                    doc <- officer::body_add_par(doc, "")
                    doc <- officer::body_add_par(doc, paste0(
                        "Interpretasi: Variabel '", var_name, "' telah berhasil diubah menjadi ",
                        n_cat, " kategori menggunakan metode ", method_text,
                        ". Setiap kategori memiliki distribusi yang ",
                        ifelse(input$method == "quantile", "relatif sama", "berdasarkan interval nilai yang sama"), "."
                    ))

                    print(doc, target = file)
                } else {
                    # Create empty document if no data
                    doc <- officer::read_docx()
                    doc <- officer::body_add_par(doc, "Interpretasi Kategorisasi Data")
                    doc <- officer::body_add_par(doc, "Data kategorisasi tidak tersedia.")
                    print(doc, target = file)
                }
            }
        )

        # Download report as PDF
        output$download_report_pdf <- downloadHandler(
            filename = function() {
                paste0("laporan_kategorisasi_", input$select_var, "_", Sys.Date(), ".pdf")
            },
            content = function(file) {
                req(processed_result(), input$select_var)

                temp_rmd <- tempfile(fileext = ".Rmd")
                temp_dir <- tempdir()

                var_name <- input$select_var
                n_cat <- input$num_kategori
                method_text <- ifelse(input$method == "interval", "interval sama", "kuantil")

                rmd_content <- paste0('---
title: "Laporan Kategorisasi Data - ALIVA Dashboard"
author: "ALIVA Dashboard"
date: "', format(Sys.Date(), "%d %B %Y"), '"
output:
  pdf_document:
    latex_engine: xelatex
    keep_tex: false
    includes:
      in_header: !expr NULL
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
options(knitr.kable.NA = "")
```

# Laporan Kategorisasi Data

## Informasi Analisis

**Variabel yang Dikategorisasi:** ', var_name, "

**Tanggal Analisis:** ", format(Sys.Date(), "%d %B %Y"), "

**Sumber Data:** SUSENAS 2017, BPS-Statistics Indonesia

---

## Parameter Kategorisasi

**Metode:** ", method_text, "
**Jumlah Kategori:** ", n_cat, "

---

## Hasil Kategorisasi

Tabel hasil kategorisasi menunjukkan distribusi data ke dalam kategori-kategori yang telah ditentukan.

---

## Interpretasi

Proses kategorisasi telah berhasil dilakukan untuk mengubah variabel kontinu menjadi variabel kategorik menggunakan metode ", method_text, ".

---

## Kesimpulan

Kategorisasi data membantu dalam analisis lebih lanjut dengan menyederhanakan kompleksitas data kontinu menjadi ", n_cat, " kategori yang mudah diinterpretasikan.
")

                writeLines(rmd_content, temp_rmd)

                # Render with comprehensive error handling and log suppression
                tryCatch(
                    {
                        # Capture and suppress all output
                        capture.output(
                            {
                                suppressMessages(suppressWarnings({
                                    output_path <- rmarkdown::render(
                                        input = temp_rmd,
                                        output_format = rmarkdown::pdf_document(
                                            latex_engine = "xelatex",
                                            keep_tex = FALSE
                                        ),
                                        quiet = TRUE,
                                        output_dir = temp_dir,
                                        clean = TRUE,
                                        envir = new.env()
                                    )
                                }))
                            },
                            type = "message"
                        )

                        if (file.exists(output_path)) {
                            file.copy(output_path, file, overwrite = TRUE)
                            # Comprehensive cleanup of all temporary files
                            unlink(temp_rmd)
                            unlink(output_path)
                            # Clean up all LaTeX-related files that might be created
                            temp_files <- list.files(temp_dir, pattern = "^file.*\\.(log|aux|out|tex|fls|fdb_latexmk)$", full.names = TRUE)
                            if (length(temp_files) > 0) unlink(temp_files)
                            # Clean up any other temporary files with the same base name
                            base_name <- tools::file_path_sans_ext(basename(temp_rmd))
                            other_temp_files <- list.files(temp_dir, pattern = paste0("^", base_name, ".*"), full.names = TRUE)
                            if (length(other_temp_files) > 0) unlink(other_temp_files)
                        } else {
                            stop("PDF output file not generated")
                        }
                    },
                    error = function(e) {
                        # Clean up on error as well
                        unlink(temp_rmd)
                        temp_files <- list.files(temp_dir, pattern = "^file.*\\.(log|aux|out|tex|fls|fdb_latexmk)$", full.names = TRUE)
                        if (length(temp_files) > 0) unlink(temp_files)
                        # If PDF generation fails, create a simple error document
                        writeLines(paste("Error generating PDF report:", e$message), file)
                        showNotification("PDF generation failed. Please try the Word format.", type = "warning", duration = 5)
                    }
                )
            }
        )

        # Download report as Word document
        output$download_report_word <- downloadHandler(
            filename = function() {
                paste0("laporan_kategorisasi_", input$select_var, "_", Sys.Date(), ".docx")
            },
            content = function(file) {
                req(processed_result(), input$select_var)

                temp_rmd <- tempfile(fileext = ".Rmd")
                temp_dir <- tempdir()

                var_name <- input$select_var
                n_cat <- input$num_kategori
                method_text <- ifelse(input$method == "interval", "interval sama", "kuantil")

                rmd_content <- paste0('---
title: "Laporan Kategorisasi Data - ALIVA Dashboard"
author: "ALIVA Dashboard"
date: "', format(Sys.Date(), "%d %B %Y"), '"
output:
  word_document:
    reference_docx: NULL
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
options(knitr.kable.NA = "")
```

# Laporan Kategorisasi Data

## Informasi Analisis

**Variabel yang Dikategorisasi:** ', var_name, "

**Tanggal Analisis:** ", format(Sys.Date(), "%d %B %Y"), "

**Sumber Data:** SUSENAS 2017, BPS-Statistics Indonesia

---

## Parameter Kategorisasi

**Metode:** ", method_text, "
**Jumlah Kategori:** ", n_cat, "

---

## Hasil Kategorisasi

Tabel hasil kategorisasi menunjukkan distribusi data ke dalam kategori-kategori yang telah ditentukan.

---

## Interpretasi

Proses kategorisasi telah berhasil dilakukan untuk mengubah variabel kontinu menjadi variabel kategorik menggunakan metode ", method_text, ".

---

## Kesimpulan

Kategorisasi data membantu dalam analisis lebih lanjut dengan menyederhanakan kompleksitas data kontinu menjadi ", n_cat, " kategori yang mudah diinterpretasikan.
")

                writeLines(rmd_content, temp_rmd)

                # Render with comprehensive error handling and log suppression
                tryCatch(
                    {
                        # Capture and suppress all output
                        capture.output(
                            {
                                suppressMessages(suppressWarnings({
                                    output_path <- rmarkdown::render(
                                        input = temp_rmd,
                                        output_format = rmarkdown::word_document(
                                            reference_docx = NULL
                                        ),
                                        quiet = TRUE,
                                        output_dir = temp_dir,
                                        clean = TRUE,
                                        envir = new.env()
                                    )
                                }))
                            },
                            type = "message"
                        )

                        if (file.exists(output_path)) {
                            file.copy(output_path, file, overwrite = TRUE)
                            # Comprehensive cleanup of all temporary files
                            unlink(temp_rmd)
                            unlink(output_path)
                            # Clean up any temporary files that might be created
                            base_name <- tools::file_path_sans_ext(basename(temp_rmd))
                            other_temp_files <- list.files(temp_dir, pattern = paste0("^", base_name, ".*"), full.names = TRUE)
                            if (length(other_temp_files) > 0) unlink(other_temp_files)
                        } else {
                            stop("Word output file not generated")
                        }
                    },
                    error = function(e) {
                        # Clean up on error as well
                        unlink(temp_rmd)
                        base_name <- tools::file_path_sans_ext(basename(temp_rmd))
                        other_temp_files <- list.files(temp_dir, pattern = paste0("^", base_name, ".*"), full.names = TRUE)
                        if (length(other_temp_files) > 0) unlink(other_temp_files)
                        # If Word generation fails, create a simple error document
                        writeLines(paste("Error generating Word report:", e$message), file)
                        showNotification("Word generation failed. Please try the PDF format.", type = "warning", duration = 5)
                    }
                )
            }
        )
    })
}
