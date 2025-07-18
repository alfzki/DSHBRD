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
        # Update variable choices
        observe({
            if (!is.null(values$sovi_data)) {
                numeric_choices <- get_variable_choices(values$sovi_data, "numeric")
                categorical_choices <- get_variable_choices(values$sovi_data, "categorical")

                updateSelectInput(session, "var_normal", choices = numeric_choices)
                updateSelectInput(session, "var_homogen", choices = numeric_choices)
                updateSelectInput(session, "group_homogen", choices = categorical_choices)
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
