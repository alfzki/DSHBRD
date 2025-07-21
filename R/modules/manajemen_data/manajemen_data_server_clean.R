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

            if (!validate_data(values$sovi_data, "Data SOVI")) {
                return()
            }

            var_name <- input$transform_var
            method <- input$transform_method
            var_data <- values$sovi_data[[var_name]]

            # Perform transformation
            transformed_values <- tryCatch(
                {
                    switch(method,
                        "log" = log(var_data + 1), # Add 1 to handle zeros
                        "log10" = log10(var_data + 1),
                        "sqrt" = sqrt(pmax(var_data, 0)), # Ensure non-negative
                        "zscore" = scale(var_data)[, 1],
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
                },
                error = function(e) {
                    showNotification(paste("Error dalam transformasi:", e$message), type = "error")
                    return(NULL)
                }
            )

            if (is.null(transformed_values)) {
                return()
            }

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

        # Download handler for transformations
        output$download_transformed_csv <- downloadHandler(
            filename = function() {
                paste0("transformed_data_", Sys.Date(), ".csv")
            },
            content = function(file) {
                req(transformed_data())
                write.csv(transformed_data(), file, row.names = FALSE)
            }
        )
    })
}
