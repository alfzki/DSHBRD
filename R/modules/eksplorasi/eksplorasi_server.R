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
        # Update variable choices
        observe({
            if (!is.null(values$sovi_data)) {
                numeric_choices <- get_variable_choices(values$sovi_data, "numeric")
                updateSelectInput(session, "select_var", choices = numeric_choices)
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
