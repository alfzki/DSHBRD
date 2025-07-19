# Unused Code Analysis Report

Generated: Sat Jul 19 09:19:21 UTC 2025

## Summary

- **Total R files:** 0
- **Potentially unused files:** 0
- **Potentially unused functions:** 19

## Potentially Unused Files

- None detected

## Potentially Unused Functions

- `./R/modules/beranda/beranda_server.R:        create_metadata_dataframe <- function() {()`
- `./R/modules/eksplorasi/eksplorasi_server.R:        calculate_extended_statistics <- function(var_data, var_summary) {()`
- `./R/modules/eksplorasi/eksplorasi_server.R:        calculate_outlier_information <- function(var_data, q1_val, q3_val, iqr_val) {()`
- `./R/modules/eksplorasi/eksplorasi_server.R:        create_basic_stats_panel <- function(stats) {()`
- `./R/modules/eksplorasi/eksplorasi_server.R:        create_contextual_panel <- function(var_name, stats) {()`
- `./R/modules/eksplorasi/eksplorasi_server.R:        create_distribution_panel <- function(skewness_interpretation, variability_level, stats, outlier_info) {()`
- `./R/modules/eksplorasi/eksplorasi_server.R:        create_error_map <- function(error_message) {()`
- `./R/modules/eksplorasi/eksplorasi_server.R:        create_fallback_map <- function() {()`
- `./R/modules/eksplorasi/eksplorasi_server.R:        create_practical_implications_panel <- function(var_name, cv_val, skewness_interpretation, outlier_info) {()`
- `./R/modules/eksplorasi/eksplorasi_server.R:        create_spatial_vulnerability_map <- function(geojson_path, selected_var, sovi_data) {()`
- `./R/modules/eksplorasi/eksplorasi_server.R:        determine_skewness <- function(mean_val, median_val) {()`
- `./R/modules/eksplorasi/eksplorasi_server.R:        determine_variability_level <- function(cv_val) {()`
- `./R/modules/eksplorasi/eksplorasi_server.R:        generate_interpretation_ui <- function(var_name, stats) {()`
- `./R/modules/manajemen_data/manajemen_data_server.R:        save_categorized_variable <- function(var_name) {()`
- `generate_clean_report()`
- `interpret_with_recommendations()`
- `render_pdf_safely()`
- `render_word_safely()`
- `server()`

## Recommendations

1. **Manual Review Required:** Each identified item needs manual verification
2. **Testing:** Test the application after any removals
3. **Documentation:** Update documentation if code is removed
4. **CI/CD Integration:** Consider automating this analysis

## Detailed Analysis Files

The following temporary files contain detailed analysis:
- Function definitions: /tmp/unused_code_analysis/function_definitions.txt
- Function calls: /tmp/unused_code_analysis/function_calls.txt
- All R files: /tmp/unused_code_analysis/all_r_files.txt
- Sourced files: /tmp/unused_code_analysis/sourced_files.txt

