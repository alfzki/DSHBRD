# Manajemen Data Server Module
# Comprehensive server logic for data management functionality

#' Manajemen Data Server Module
#'
#' Server logic for comprehensive data management features
#'
#' @param id Module ID for namespacing
#' @param values Reactive values object containing shared data
manajemen_data_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        
        # Reactive values for this module
        transformed_data <- reactiveVal(NULL)
        categorized_data <- reactiveVal(NULL)
        derived_data <- reactiveVal(NULL)
        filtered_data <- reactiveVal(NULL)
        
        # =================================================================
        # UPDATE VARIABLE CHOICES FOR ALL TABS
        # =================================================================
        
        observe({
            req(values$sovi_data)
            
            numeric_choices <- get_variable_choices(values$sovi_data, "numeric")
            categorical_choices <- get_variable_choices(values$sovi_data, "categorical")
            all_choices <- get_variable_choices(values$sovi_data, "all")
            
            # Update for transformation tab
            updateSelectInput(session, "transform_var", choices = numeric_choices)
            
            # Update for categorization tab
            updateSelectInput(session, "select_var", choices = numeric_choices)
            
            # Update for derived variables tab
            updateSelectInput(session, "derived_var1", choices = numeric_choices)
            updateSelectInput(session, "derived_var2", choices = numeric_choices)
        })
        
        # =================================================================
        # TAB 1: VARIABLE TRANSFORMATIONS
        # =================================================================
        
        # Apply transformation
        observeEvent(input$apply_transform, {
            req(input$transform_var, input$transform_method)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return()
            
            var_name <- input$transform_var
            method <- input$transform_method
            var_data <- values$sovi_data[[var_name]]
            
            # Perform transformation
            transformed_values <- tryCatch({
                switch(method,
                    "log" = log(var_data + 1), # Add 1 to handle zeros
                    "log10" = log10(var_data + 1),
                    "sqrt" = sqrt(pmax(var_data, 0)), # Ensure non-negative
                    "zscore" = scale(var_data)[,1],
                    "minmax" = {
                        min_val <- input$minmax_min %||% 0
                        max_val <- input$minmax_max %||% 1
                        ((var_data - min(var_data, na.rm = TRUE)) / 
                         (max(var_data, na.rm = TRUE) - min(var_data, na.rm = TRUE))) * 
                         (max_val - min_val) + min_val
                    },
                    "square" = var_data^2,
                    "inverse" = 1 / (var_data + 0.001) # Add small value to avoid division by zero
                )
            }, error = function(e) {
                showNotification(paste("Error dalam transformasi:", e$message), type = "error")
                return(NULL)
            })
            
            if (is.null(transformed_values)) return()
            
            # Create result data frame
            new_var_name <- if (input$transform_var_name != "") {
                input$transform_var_name
            } else {
                paste0(var_name, "_", method)
            }
            
            result_data <- values$sovi_data %>%
                mutate(!!new_var_name := transformed_values) %>%
                select(DISTRICTCODE, !!sym(var_name), !!sym(new_var_name))
            
            transformed_data(result_data)
            
            # Add to main dataset if name provided
            if (input$transform_var_name != "") {
                values$sovi_data[[new_var_name]] <- transformed_values
                values$user_created_vars[[new_var_name]] <- transformed_values
                values$data_update_counter <- values$data_update_counter + 1
                showNotification(paste("Variabel", new_var_name, "berhasil ditambahkan ke dataset!"), type = "message")
            }
        })
        
        # Display transformation results
        output$transform_result_table <- DT::renderDT({
            req(transformed_data())
            DT::datatable(transformed_data(),
                options = list(pageLength = 10, scrollX = TRUE),
                class = "table-striped table-hover"
            ) %>% DT::formatRound(columns = 2:3, digits = 4)
        })
        
        # Transformation comparison plot
        output$transform_comparison <- renderPlotly({
            req(transformed_data(), input$transform_var)
            
            original_var <- transformed_data()[[2]]
            transformed_var <- transformed_data()[[3]]
            
            p1 <- plot_ly(x = ~original_var, type = "histogram", name = "Original") %>%
                layout(xaxis = list(title = "Original Values"))
            
            p2 <- plot_ly(x = ~transformed_var, type = "histogram", name = "Transformed") %>%
                layout(xaxis = list(title = "Transformed Values"))
            
            subplot(p1, p2, nrows = 2, shareY = TRUE) %>%
                layout(title = paste("Comparison:", input$transform_var, "->", input$transform_method))
        })
        
        # Transformation interpretation
        output$transform_interpretation <- renderText({
            req(transformed_data(), input$transform_var, input$transform_method)
            
            original_var <- transformed_data()[[2]]
            transformed_var <- transformed_data()[[3]]
            
            method_names <- c(
                "log" = "logaritma natural",
                "log10" = "logaritma basis 10", 
                "sqrt" = "akar kuadrat",
                "zscore" = "standardisasi z-score",
                "minmax" = "normalisasi min-max",
                "square" = "kuadrat",
                "inverse" = "invers"
            )
            
            paste0(
                "Hasil Transformasi Variabel: ", input$transform_var, "\n",
                "Metode: ", method_names[input$transform_method], "\n\n",
                "Statistik Original:\n",
                "Mean: ", round(mean(original_var, na.rm = TRUE), 4), "\n",
                "SD: ", round(sd(original_var, na.rm = TRUE), 4), "\n",
                "Min: ", round(min(original_var, na.rm = TRUE), 4), "\n",
                "Max: ", round(max(original_var, na.rm = TRUE), 4), "\n\n",
                "Statistik Transformed:\n",
                "Mean: ", round(mean(transformed_var, na.rm = TRUE), 4), "\n",
                "SD: ", round(sd(transformed_var, na.rm = TRUE), 4), "\n",
                "Min: ", round(min(transformed_var, na.rm = TRUE), 4), "\n",
                "Max: ", round(max(transformed_var, na.rm = TRUE), 4), "\n\n",
                "Interpretasi: Transformasi ", method_names[input$transform_method], 
                " telah diterapkan untuk ",
                switch(input$transform_method,
                    "zscore" = "menstandarisasi data dengan mean=0 dan sd=1",
                    "minmax" = "menormalisasi data ke rentang yang ditentukan",
                    "log" = "mengurangi skewness dan menstabilkan varians",
                    "sqrt" = "mengurangi pengaruh outlier dan skewness",
                    "square" = "memperkuat pengaruh nilai besar",
                    "inverse" = "membalik hubungan dan mengurangi pengaruh nilai besar"
                )
            )
        })
        
        # =================================================================
        # TAB 2: CATEGORIZATION (Enhanced from original)
        # =================================================================
        
        # Process categorization
        observeEvent(input$process_btn, {
            req(input$select_var, input$num_kategori, input$method)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return()
            
            var_name <- input$select_var
            n_cat <- input$num_kategori
            method <- input$method
            var_data <- values$sovi_data[[var_name]]
            
            # Create categories based on method
            categories <- tryCatch({
                if (method == "interval") {
                    breaks <- seq(min(var_data, na.rm = TRUE), max(var_data, na.rm = TRUE),
                        length.out = n_cat + 1)
                    cut(var_data, breaks = breaks, include.lowest = TRUE,
                        labels = paste("Kategori", 1:n_cat))
                } else if (method == "quantile") {
                    breaks <- quantile(var_data, probs = seq(0, 1, length.out = n_cat + 1), na.rm = TRUE)
                    cut(var_data, breaks = breaks, include.lowest = TRUE,
                        labels = paste("Kategori", 1:n_cat))
                } else if (method == "custom") {
                    req(input$custom_breaks)
                    breaks <- as.numeric(unlist(strsplit(input$custom_breaks, ",")))
                    if (length(breaks) < 2) {
                        showNotification("Minimal 2 breakpoints diperlukan", type = "error")
                        return(NULL)
                    }
                    breaks <- c(-Inf, sort(breaks), Inf)
                    cut(var_data, breaks = breaks, include.lowest = TRUE,
                        labels = paste("Kategori", 1:(length(breaks)-1)))
                }
            }, error = function(e) {
                showNotification(paste("Error dalam kategorisasi:", e$message), type = "error")
                return(NULL)
            })
            
            if (is.null(categories)) return()
            
            # Create result data frame
            new_var_name <- if (input$new_var_name != "") {
                input$new_var_name
            } else {
                paste0(var_name, "_kategori")
            }
            
            result_data <- values$sovi_data %>%
                mutate(!!new_var_name := categories) %>%
                select(DISTRICTCODE, !!sym(var_name), !!sym(new_var_name))
            
            categorized_data(result_data)
            
            # Add to main dataset if name provided
            if (input$new_var_name != "") {
                values$sovi_data[[new_var_name]] <- categories
                values$user_created_vars[[new_var_name]] <- categories
                values$data_update_counter <- values$data_update_counter + 1
                showNotification(paste("Variabel", new_var_name, "berhasil ditambahkan ke dataset!"), type = "message")
            }
        })
        
        # Display categorization results
        output$result_table <- DT::renderDT({
            req(categorized_data())
            DT::datatable(categorized_data(),
                options = list(pageLength = 10, scrollX = TRUE),
                class = "table-striped table-hover"
            )
        })
        
        # Category distribution plot
        output$category_plot <- renderPlotly({
            req(categorized_data())
            
            cat_data <- categorized_data()[[3]]
            cat_counts <- table(cat_data)
            
            plot_ly(x = names(cat_counts), y = as.numeric(cat_counts), type = "bar") %>%
                layout(title = "Distribusi Kategori",
                       xaxis = list(title = "Kategori"),
                       yaxis = list(title = "Frekuensi"))
        })
        
        # =================================================================
        # TAB 3: DERIVED VARIABLES
        # =================================================================
        
        # Available variables table
        output$available_vars_table <- DT::renderDT({
            req(values$sovi_data)
            
            var_info <- data.frame(
                Variable = names(values$sovi_data),
                Type = sapply(values$sovi_data, function(x) class(x)[1]),
                Sample_Values = sapply(names(values$sovi_data), function(x) {
                    vals <- values$sovi_data[[x]]
                    if (is.numeric(vals)) {
                        paste(round(vals[1:3], 2), collapse = ", ")
                    } else {
                        paste(as.character(vals[1:3]), collapse = ", ")
                    }
                })
            )
            
            DT::datatable(var_info,
                options = list(pageLength = 8, dom = "tp"),
                rownames = FALSE,
                class = "table-condensed table-striped"
            )
        })
        
        # Create derived variable
        observeEvent(input$create_derived, {
            req(input$derived_var_name)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return()
            
            new_var_name <- trimws(input$derived_var_name)
            if (new_var_name == "" || !grepl("^[a-zA-Z][a-zA-Z0-9_]*$", new_var_name)) {
                showNotification("Nama variabel harus dimulai dengan huruf dan hanya mengandung huruf, angka, dan underscore.",
                    type = "error", duration = 5)
                return()
            }
            
            # Create derived variable based on operation
            derived_values <- tryCatch({
                if (input$derived_operation == "formula") {
                    req(input$custom_formula)
                    # Evaluate custom formula
                    formula_text <- input$custom_formula
                    with(values$sovi_data, eval(parse(text = formula_text)))
                } else {
                    req(input$derived_var1, input$derived_var2)
                    var1 <- values$sovi_data[[input$derived_var1]]
                    var2 <- values$sovi_data[[input$derived_var2]]
                    
                    switch(input$derived_operation,
                        "sum" = var1 + var2,
                        "subtract" = var1 - var2,
                        "multiply" = var1 * var2,
                        "divide" = var1 / var2,
                        "mean" = (var1 + var2) / 2
                    )
                }
            }, error = function(e) {
                showNotification(paste("Error dalam pembuatan variabel:", e$message), type = "error")
                return(NULL)
            })
            
            if (is.null(derived_values)) return()
            
            # Add to dataset
            values$sovi_data[[new_var_name]] <- derived_values
            values$user_created_vars[[new_var_name]] <- derived_values
            values$data_update_counter <- values$data_update_counter + 1
            
            showNotification(paste("Variabel", new_var_name, "berhasil dibuat!"), type = "success")
        })
        
        # Derived variable preview
        output$derived_preview <- renderPlotly({
            if (is.null(input$derived_var_name) || input$derived_var_name == "") return(NULL)
            
            # Try to create preview of the derived variable
            tryCatch({
                if (input$derived_operation == "formula" && !is.null(input$custom_formula) && input$custom_formula != "") {
                    preview_values <- with(values$sovi_data, eval(parse(text = input$custom_formula)))
                } else if (!is.null(input$derived_var1) && !is.null(input$derived_var2)) {
                    var1 <- values$sovi_data[[input$derived_var1]]
                    var2 <- values$sovi_data[[input$derived_var2]]
                    
                    preview_values <- switch(input$derived_operation,
                        "sum" = var1 + var2,
                        "subtract" = var1 - var2,
                        "multiply" = var1 * var2,
                        "divide" = var1 / var2,
                        "mean" = (var1 + var2) / 2
                    )
                } else {
                    return(NULL)
                }
                
                if (is.numeric(preview_values)) {
                    plot_ly(x = ~preview_values, type = "histogram") %>%
                        layout(title = paste("Preview:", input$derived_var_name),
                               xaxis = list(title = "Values"))
                } else {
                    plot_ly(x = ~table(preview_values), type = "bar") %>%
                        layout(title = paste("Preview:", input$derived_var_name))
                }
            }, error = function(e) {
                return(NULL)
            })
        })
        
        # =================================================================
        # TAB 4: DATA FILTERING
        # =================================================================
        
        # Generate numeric filters
        output$numeric_filters <- renderUI({
            req(values$sovi_data)
            
            numeric_vars <- get_numeric_columns(values$sovi_data)
            
            if (length(numeric_vars) == 0) {
                return(p("Tidak ada variabel numerik tersedia"))
            }
            
            # Create sliders for numeric variables
            lapply(numeric_vars[1:min(6, length(numeric_vars))], function(var_name) {
                var_data <- values$sovi_data[[var_name]]
                var_range <- range(var_data, na.rm = TRUE)
                
                div(
                    h6(var_name),
                    sliderInput(
                        session$ns(paste0("filter_", var_name)),
                        label = NULL,
                        min = var_range[1],
                        max = var_range[2],
                        value = var_range,
                        step = (var_range[2] - var_range[1]) / 100
                    )
                )
            })
        })
        
        # Generate categorical filters
        output$categorical_filters <- renderUI({
            req(values$sovi_data)
            
            cat_vars <- get_categorical_columns(values$sovi_data)
            
            if (length(cat_vars) == 0) {
                return(p("Tidak ada variabel kategorik tersedia"))
            }
            
            # Create checkboxes for categorical variables
            lapply(cat_vars[1:min(4, length(cat_vars))], function(var_name) {
                var_data <- values$sovi_data[[var_name]]
                unique_vals <- unique(var_data)[!is.na(unique(var_data))]
                
                div(
                    h6(var_name),
                    checkboxGroupInput(
                        session$ns(paste0("filter_cat_", var_name)),
                        label = NULL,
                        choices = unique_vals,
                        selected = unique_vals
                    )
                )
            })
        })
        
        # Apply filters
        observeEvent(input$apply_filters, {
            req(values$sovi_data)
            
            filtered <- values$sovi_data
            filter_conditions <- c()
            
            # Apply numeric filters
            numeric_vars <- get_numeric_columns(values$sovi_data)
            for (var_name in numeric_vars[1:min(6, length(numeric_vars))]) {
                filter_input <- input[[paste0("filter_", var_name)]]
                if (!is.null(filter_input)) {
                    filtered <- filtered %>% 
                        filter(!!sym(var_name) >= filter_input[1] & !!sym(var_name) <= filter_input[2])
                    filter_conditions <- c(filter_conditions, 
                        paste(var_name, ":", filter_input[1], "-", filter_input[2]))
                }
            }
            
            # Apply categorical filters
            cat_vars <- get_categorical_columns(values$sovi_data)
            for (var_name in cat_vars[1:min(4, length(cat_vars))]) {
                filter_input <- input[[paste0("filter_cat_", var_name)]]
                if (!is.null(filter_input) && length(filter_input) > 0) {
                    filtered <- filtered %>% filter(!!sym(var_name) %in% filter_input)
                    filter_conditions <- c(filter_conditions, 
                        paste(var_name, ":", paste(filter_input, collapse = ", ")))
                }
            }
            
            filtered_data(filtered)
            
            # Update summary
            output$filter_summary <- renderUI({
                div(
                    class = "alert alert-info",
                    h6("Filter Applied:"),
                    if (length(filter_conditions) > 0) {
                        tags$ul(lapply(filter_conditions, tags$li))
                    } else {
                        p("Tidak ada filter aktif")
                    },
                    p(paste("Hasil:", nrow(filtered), "dari", nrow(values$sovi_data), "baris"))
                )
            })
            
            showNotification(paste("Filter diterapkan:", nrow(filtered), "baris tersisa"), type = "message")
        })
        
        # Reset filters
        observeEvent(input$reset_filters, {
            filtered_data(values$sovi_data)
            
            output$filter_summary <- renderUI({
                div(
                    class = "alert alert-success",
                    p("Filter direset - menampilkan semua data"),
                    p(paste("Total:", nrow(values$sovi_data), "baris"))
                )
            })
            
            showNotification("Filter direset", type = "message")
        })
        
        # Display filtered data
        output$filtered_data_table <- DT::renderDT({
            data_to_show <- filtered_data() %||% values$sovi_data
            
            DT::datatable(data_to_show,
                options = list(
                    pageLength = 10, 
                    scrollX = TRUE,
                    dom = "Bfrtip",
                    buttons = c("copy", "csv", "excel")
                ),
                class = "table-striped table-hover"
            )
        })
        
        # =================================================================
        # TAB 5: MISSING DATA & SUMMARY
        # =================================================================
        
        # Missing data analysis
        output$missing_data_table <- DT::renderDT({
            req(values$sovi_data)
            
            missing_info <- values$sovi_data %>%
                summarise_all(~sum(is.na(.))) %>%
                gather(Variable, Missing_Count) %>%
                mutate(
                    Total_Obs = nrow(values$sovi_data),
                    Missing_Percent = round((Missing_Count / Total_Obs) * 100, 2),
                    Status = case_when(
                        Missing_Count == 0 ~ "Complete",
                        Missing_Percent <= 5 ~ "Good",
                        Missing_Percent <= 15 ~ "Fair", 
                        TRUE ~ "Poor"
                    )
                ) %>%
                arrange(desc(Missing_Count))
            
            DT::datatable(missing_info,
                options = list(pageLength = 10, dom = "tp"),
                rownames = FALSE
            ) %>%
                DT::formatStyle("Status",
                    backgroundColor = DT::styleEqual(
                        c("Complete", "Good", "Fair", "Poor"),
                        c("#d4edda", "#fff3cd", "#f8d7da", "#f5c6cb")
                    )
                )
        })
        
        # Handle missing data
        observeEvent(input$handle_missing, {
            req(input$missing_method)
            
            if (!validate_data(values$sovi_data, "Data SOVI")) return()
            
            processed_data <- tryCatch({
                switch(input$missing_method,
                    "remove" = {
                        na.omit(values$sovi_data)
                    },
                    "impute_central" = {
                        values$sovi_data %>%
                            mutate_if(is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>%
                            mutate_if(is.character, ~ifelse(is.na(.), 
                                names(sort(table(.), decreasing = TRUE))[1], .))
                    },
                    "impute_value" = {
                        req(input$impute_value)
                        values$sovi_data %>%
                            mutate_if(is.numeric, ~ifelse(is.na(.), input$impute_value, .))
                    }
                )
            }, error = function(e) {
                showNotification(paste("Error:", e$message), type = "error")
                return(NULL)
            })
            
            if (!is.null(processed_data)) {
                values$sovi_data <- processed_data
                values$data_update_counter <- values$data_update_counter + 1
                showNotification("Penanganan missing data berhasil!", type = "success")
            }
        })
        
        # Summary statistics
        output$summary_stats_table <- DT::renderDT({
            req(values$sovi_data)
            
            numeric_vars <- get_numeric_columns(values$sovi_data)
            
            if (length(numeric_vars) == 0) {
                return(DT::datatable(data.frame(Message = "Tidak ada variabel numerik")))
            }
            
            summary_stats <- values$sovi_data %>%
                select(all_of(numeric_vars)) %>%
                summarise_all(list(
                    Mean = ~round(mean(., na.rm = TRUE), 3),
                    SD = ~round(sd(., na.rm = TRUE), 3),
                    Min = ~round(min(., na.rm = TRUE), 3),
                    Max = ~round(max(., na.rm = TRUE), 3),
                    Missing = ~sum(is.na(.))
                )) %>%
                gather(Stat_Var, Value) %>%
                separate(Stat_Var, into = c("Variable", "Statistic"), sep = "_(?=[^_]*$)") %>%
                spread(Statistic, Value)
            
            DT::datatable(summary_stats,
                options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE
            )
        })
        
        # Dataset information
        output$data_info <- renderText({
            req(values$sovi_data)
            
            numeric_count <- length(get_numeric_columns(values$sovi_data))
            categorical_count <- length(get_categorical_columns(values$sovi_data))
            total_missing <- sum(is.na(values$sovi_data))
            
            paste0(
                "INFORMASI DATASET\n",
                "=================\n",
                "Total Observasi: ", nrow(values$sovi_data), "\n",
                "Total Variabel: ", ncol(values$sovi_data), "\n",
                "- Numerik: ", numeric_count, "\n",
                "- Kategorik: ", categorical_count, "\n\n",
                "Missing Values: ", total_missing, " (",
                round((total_missing / (nrow(values$sovi_data) * ncol(values$sovi_data))) * 100, 2), "%)\n\n",
                "User-Created Variables: ", length(values$user_created_vars), "\n",
                "Last Updated: ", Sys.time()
            )
        })
        
        # =================================================================
        # DOWNLOAD HANDLERS
        # =================================================================
        
        # Download processed data
        output$download_result <- downloadHandler(
            filename = function() {
                paste("ALIVA_ManajemenData_", Sys.Date(), ".csv", sep = "")
            },
            content = function(file) {
                # Determine which data to download based on active tab
                data_to_download <- values$sovi_data
                
                if (!is.null(transformed_data())) {
                    data_to_download <- transformed_data()
                } else if (!is.null(categorized_data())) {
                    data_to_download <- categorized_data()
                } else if (!is.null(filtered_data())) {
                    data_to_download <- filtered_data()
                }
                
                write.csv(data_to_download, file, row.names = FALSE)
            }
        )
        
        # Download interpretation
        output$download_interpretation <- downloadHandler(
            filename = function() {
                paste("ALIVA_ManajemenData_Interpretasi_", Sys.Date(), ".docx", sep = "")
            },
            content = function(file) {
                temp_rmd <- tempfile(fileext = ".Rmd")
                
                writeLines(c(
                    "---",
                    "title: 'ALIVA Dashboard - Hasil Manajemen Data'",
                    "author: 'ALIVA Analytics'", 
                    "date: '`r Sys.Date()`'",
                    "output: word_document",
                    "---",
                    "",
                    "# Laporan Manajemen Data",
                    "",
                    "## Ringkasan Aktivitas",
                    paste("Dataset diproses pada:", Sys.time()),
                    "",
                    "## Transformasi dan Modifikasi",
                    if (!is.null(transformed_data())) "- Transformasi variabel diterapkan" else "",
                    if (!is.null(categorized_data())) "- Kategorisasi variabel dilakukan" else "",
                    if (!is.null(filtered_data())) "- Filter data diterapkan" else "",
                    "",
                    paste("Total variabel user-defined:", length(values$user_created_vars))
                ), temp_rmd)
                
                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )
        
        # Download PDF report
        output$download_report_pdf <- downloadHandler(
            filename = function() {
                paste("ALIVA_ManajemenData_Report_", Sys.Date(), ".pdf", sep = "")
            },
            content = function(file) {
                temp_rmd <- tempfile(fileext = ".Rmd")
                
                writeLines(c(
                    "---",
                    "title: 'ALIVA Dashboard - Laporan Manajemen Data'",
                    "author: 'ALIVA Analytics'",
                    "date: '`r Sys.Date()`'", 
                    "output: pdf_document",
                    "---",
                    "",
                    "# Laporan Komprehensif Manajemen Data",
                    "",
                    paste("Dataset:", nrow(values$sovi_data), "observasi,", ncol(values$sovi_data), "variabel"),
                    "",
                    "## Fitur yang Digunakan",
                    "- Transformasi Variabel",
                    "- Kategorisasi",
                    "- Variabel Turunan", 
                    "- Filter Data",
                    "- Penanganan Missing Data"
                ), temp_rmd)
                
                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )
        
        # Download Word report  
        output$download_report_word <- downloadHandler(
            filename = function() {
                paste("ALIVA_ManajemenData_Report_", Sys.Date(), ".docx", sep = "")
            },
            content = function(file) {
                temp_rmd <- tempfile(fileext = ".Rmd")
                
                writeLines(c(
                    "---",
                    "title: 'ALIVA Dashboard - Laporan Manajemen Data'",
                    "author: 'ALIVA Analytics'",
                    "date: '`r Sys.Date()`'",
                    "output: word_document", 
                    "---",
                    "",
                    "# Laporan Manajemen Data ALIVA",
                    "",
                    "Laporan ini berisi ringkasan semua aktivitas manajemen data yang dilakukan.",
                    "",
                    paste("Dataset:", nrow(values$sovi_data), "observasi dengan", ncol(values$sovi_data), "variabel"),
                    "",
                    "## Fitur Tersedia:",
                    "1. **Transformasi Variabel**: Log, sqrt, z-score, min-max",
                    "2. **Kategorisasi**: Interval, kuantil, custom breakpoints", 
                    "3. **Variabel Turunan**: Operasi matematika dan formula custom",
                    "4. **Filter Data**: Filter numerik dan kategorik",
                    "5. **Missing Data**: Analisis dan penanganan nilai hilang"
                ), temp_rmd)
                
                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )
    })
}
