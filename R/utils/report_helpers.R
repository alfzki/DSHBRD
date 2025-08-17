# report_helpers.R
# This file contains helper functions for generating reports from R Markdown templates.

#' Render a regression report to various formats
#'
#' This function acts as a centralized wrapper around rmarkdown::render for the
#' regression report template. It simplifies the process of generating reports
#' by preparing the parameters and handling the rendering call.
#'
#' @param model The linear model object (result of lm()).
#' @param assumption_tests A list containing the results of assumption tests.
#' @param dep_var A string with the name of the dependent variable.
#' @param indep_vars A character vector with the names of independent variables.
#' @param output_format The target output format (e.g., 'pdf_document', 'word_document', 'html_fragment').
#' @param output_file The path to the output file.
#' @param output_type The type of content to render ('full_report' or 'interpretation_only').
#' @return The path to the generated report file.
render_regression_report <- function(model, assumption_tests, dep_var, indep_vars,
                                     output_format, output_file, output_type = "full_report") {
    # 1. Prepare parameters for the R Markdown template
    params <- list(
        model = model,
        assumption_tests = assumption_tests,
        dep_var = dep_var,
        indep_vars = indep_vars,
        output_type = output_type
    )

    # 2. Define the input Rmd template
    input_rmd <- here::here("reports", "laporan_regresi.Rmd")

    # 3. Render the R Markdown template
    # We run this in a clean environment to avoid polluting the global env
    rmarkdown::render(
        input = input_rmd,
        output_file = output_file,
        output_format = output_format,
        params = params,
        envir = new.env(parent = globalenv()),
        quiet = TRUE
    )

    # 4. Return the path to the generated file
    return(output_file)
}
