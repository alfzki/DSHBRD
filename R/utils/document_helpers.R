# Document Generation Helpers
# ===========================
# Lightweight alternatives to officer and flextable packages
# These functions provide faster startup times for deployment

#' Create Word Document from Text Content
#'
#' Replaces officer package functionality with rmarkdown-based solution
#'
#' @param title Document title
#' @param content Character vector of content paragraphs
#' @param author Author name
#' @param output_file Output file path
create_word_document <- function(title, content, author = "ALIVA Dashboard", output_file) {
    # Create temporary R Markdown file
    temp_rmd <- tempfile(fileext = ".Rmd")

    # YAML header
    yaml_header <- c(
        "---",
        paste0("title: '", title, "'"),
        paste0("author: '", author, "'"),
        paste0("date: '", format(Sys.Date(), "%d %B %Y"), "'"),
        "output:",
        "  word_document:",
        "    reference_docx: NULL",
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
        "```",
        ""
    )

    # Combine header and content
    full_content <- c(yaml_header, content)

    # Write to temporary file
    writeLines(full_content, temp_rmd)

    # Render to Word document
    tryCatch(
        {
            rmarkdown::render(
                input = temp_rmd,
                output_file = output_file,
                quiet = TRUE
            )
        },
        error = function(e) {
            # Fallback: create text file if Word generation fails
            writeLines(c(
                paste("Title:", title),
                paste("Author:", author),
                paste("Date:", format(Sys.Date(), "%d %B %Y")),
                "",
                content
            ), gsub("\\.docx$", ".txt", output_file))

            warning("Word document generation failed, created text file instead: ", e$message)
        }
    )
}

#' Create Interpretation Document
#'
#' Specialized function for statistical interpretation documents
#'
#' @param test_type Type of statistical test
#' @param interpretation_text Main interpretation content
#' @param output_file Output file path
#' @param details Optional list of additional details
create_interpretation_document <- function(test_type, interpretation_text, output_file, details = NULL) {
    # Main content
    content <- c(
        paste("# Interpretasi", test_type),
        "",
        "## Hasil Analisis",
        "",
        interpretation_text,
        ""
    )

    # Add details if provided
    if (!is.null(details)) {
        content <- c(
            content,
            "## Detail Statistik",
            ""
        )

        for (i in seq_along(details)) {
            if (is.list(details)) {
                content <- c(content, paste("**", names(details)[i], ":**", details[[i]]))
            } else {
                content <- c(content, paste("-", details[i]))
            }
        }
        content <- c(content, "")
    }

    # Add recommendations section
    content <- c(
        content,
        "## Rekomendasi",
        "",
        "Berdasarkan hasil analisis di atas, disarankan untuk:",
        "",
        "- Mempertimbangkan konteks penelitian dalam interpretasi hasil",
        "- Melakukan uji lanjutan jika diperlukan",
        "- Memvalidasi temuan dengan data tambahan jika memungkinkan",
        "",
        "---",
        "",
        "*Dokumen ini dibuat otomatis oleh ALIVA Dashboard*"
    )

    create_word_document(
        title = paste("Interpretasi", test_type),
        content = content,
        output_file = output_file
    )
}

#' Format Statistical Results for Display
#'
#' Helper function to format statistical results consistently
#'
#' @param results Named list of statistical results
#' @return Character vector of formatted results
format_statistical_results <- function(results) {
    formatted <- c()

    for (name in names(results)) {
        value <- results[[name]]

        # Format based on value type
        if (is.numeric(value)) {
            if (abs(value) < 0.001) {
                formatted <- c(formatted, paste("**", name, ":**", format(value, scientific = TRUE, digits = 4)))
            } else {
                formatted <- c(formatted, paste("**", name, ":**", round(value, 4)))
            }
        } else {
            formatted <- c(formatted, paste("**", name, ":**", value))
        }
    }

    return(formatted)
}

#' Create Simple Table Markdown
#'
#' Lightweight alternative to flextable for creating markdown tables
#'
#' @param data Data frame to convert
#' @param caption Optional table caption
#' @return Character vector of markdown table
create_markdown_table <- function(data, caption = NULL) {
    if (nrow(data) == 0) {
        return("*Tidak ada data untuk ditampilkan*")
    }

    # Convert all columns to character for consistent formatting
    data_char <- data.frame(lapply(data, function(x) {
        if (is.numeric(x)) {
            ifelse(abs(x) < 0.001 & x != 0,
                format(x, scientific = TRUE, digits = 4),
                format(round(x, 4), nsmall = 4)
            )
        } else {
            as.character(x)
        }
    }), stringsAsFactors = FALSE)

    # Create table lines
    table_lines <- c()

    if (!is.null(caption)) {
        table_lines <- c(table_lines, paste("**", caption, "**"), "")
    }

    # Header
    header <- paste("|", paste(names(data_char), collapse = " | "), "|")
    separator <- paste("|", paste(rep("---", ncol(data_char)), collapse = " | "), "|")
    table_lines <- c(table_lines, header, separator)

    # Data rows
    for (i in 1:nrow(data_char)) {
        row <- paste("|", paste(data_char[i, ], collapse = " | "), "|")
        table_lines <- c(table_lines, row)
    }

    return(table_lines)
}
