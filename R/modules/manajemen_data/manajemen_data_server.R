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
        # Update variable choices
        observe({
            if (!is.null(values$sovi_data)) {
                numeric_choices <- get_variable_choices(values$sovi_data, "numeric")
                updateSelectInput(session, "select_var", choices = numeric_choices)
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
