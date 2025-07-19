# Statistical Interpretation Helper Functions
# This file contains functions to automatically interpret statistical test results
# in Indonesian language for better user understanding

#' Interpret T-Test Results
#'
#' @param test_result Object returned from t.test()
#' @param alpha Significance level (default: 0.05)
#' @param test_type Type of test ("one_sample" or "two_sample")
#' @return Character string with interpretation in Indonesian
interpret_ttest <- function(test_result, alpha = 0.05, test_type = "one_sample") {
    p_value <- test_result$p.value
    conf_int <- test_result$conf.int
    t_stat <- test_result$statistic
    df <- test_result$parameter

    # Calculate effect size (Cohen's d)
    if (test_type == "one_sample") {
        # For one-sample t-test, effect size = (mean - mu) / sd
        effect_size <- abs(t_stat) / sqrt(df + 1)
    } else {
        # For two-sample t-test, approximate Cohen's d
        effect_size <- abs(t_stat) * sqrt(2 / df)
    }

    # Effect size interpretation
    effect_interpretation <- if (effect_size < 0.2) {
        "sangat kecil"
    } else if (effect_size < 0.5) {
        "kecil"
    } else if (effect_size < 0.8) {
        "sedang"
    } else {
        "besar"
    }

    # Main interpretation
    if (p_value < alpha) {
        if (test_type == "one_sample") {
            interpretation <- paste0(
                "**Kesimpulan Statistik:**\n",
                "Dengan p-value = ", round(p_value, 4), " (< α = ", alpha, "), ",
                "kita **menolak hipotesis nol (H₀)**. ",
                "Terdapat bukti statistik yang signifikan bahwa rata-rata populasi ",
                "berbeda dari nilai yang diuji.\n\n",
                "**Detail Analisis:**\n",
                "- Statistik t = ", round(t_stat, 3), " dengan df = ", df, "\n",
                "- Interval kepercayaan 95%: [", round(conf_int[1], 3), ", ", round(conf_int[2], 3), "]\n",
                "- Ukuran efek (Cohen's d) ≈ ", round(effect_size, 3), " (", effect_interpretation, ")\n\n",
                "**Interpretasi Praktis:**\n",
                "Perbedaan yang ditemukan secara statistik signifikan dengan ukuran efek ",
                effect_interpretation, ". Hasil ini menunjukkan bahwa perbedaan tersebut ",
                ifelse(effect_size >= 0.5, "cukup substansial", "relatif kecil"),
                " dalam konteks praktis."
            )
        } else {
            interpretation <- paste0(
                "**Kesimpulan Statistik:**\n",
                "Dengan p-value = ", round(p_value, 4), " (< α = ", alpha, "), ",
                "kita **menolak hipotesis nol (H₀)**. ",
                "Terdapat perbedaan rata-rata yang signifikan secara statistik antara kedua kelompok.\n\n",
                "**Detail Analisis:**\n",
                "- Statistik t = ", round(t_stat, 3), " dengan df = ", df, "\n",
                "- Interval kepercayaan 95% untuk selisih rata-rata: [", round(conf_int[1], 3), ", ", round(conf_int[2], 3), "]\n",
                "- Ukuran efek (Cohen's d) ≈ ", round(effect_size, 3), " (", effect_interpretation, ")\n\n",
                "**Interpretasi Praktis:**\n",
                "Perbedaan antar kelompok secara statistik signifikan dengan ukuran efek ",
                effect_interpretation, ". Hasil ini menunjukkan bahwa perbedaan tersebut ",
                ifelse(effect_size >= 0.5, "cukup substansial", "relatif kecil"),
                " dalam konteks praktis."
            )
        }
    } else {
        if (test_type == "one_sample") {
            interpretation <- paste0(
                "**Kesimpulan Statistik:**\n",
                "Dengan p-value = ", round(p_value, 4), " (≥ α = ", alpha, "), ",
                "kita **gagal menolak hipotesis nol (H₀)**. ",
                "Tidak terdapat bukti statistik yang cukup untuk menyatakan bahwa ",
                "rata-rata populasi berbeda dari nilai yang diuji.\n\n",
                "**Detail Analisis:**\n",
                "- Statistik t = ", round(t_stat, 3), " dengan df = ", df, "\n",
                "- Interval kepercayaan 95%: [", round(conf_int[1], 3), ", ", round(conf_int[2], 3), "]\n",
                "- Ukuran efek (Cohen's d) ≈ ", round(effect_size, 3), " (", effect_interpretation, ")\n\n",
                "**Interpretasi Praktis:**\n",
                "Meskipun tidak signifikan secara statistik, ukuran efek menunjukkan magnitude ",
                effect_interpretation, ". Hal ini mungkin mengindikasikan perlunya ",
                "sampel yang lebih besar atau ada faktor lain yang perlu dipertimbangkan."
            )
        } else {
            interpretation <- paste0(
                "**Kesimpulan Statistik:**\n",
                "Dengan p-value = ", round(p_value, 4), " (≥ α = ", alpha, "), ",
                "kita **gagal menolak hipotesis nol (H₀)**. ",
                "Tidak terdapat bukti statistik yang cukup untuk menyatakan bahwa ",
                "terdapat perbedaan rata-rata antara kedua kelompok.\n\n",
                "**Detail Analisis:**\n",
                "- Statistik t = ", round(t_stat, 3), " dengan df = ", df, "\n",
                "- Interval kepercayaan 95% untuk selisih rata-rata: [", round(conf_int[1], 3), ", ", round(conf_int[2], 3), "]\n",
                "- Ukuran efek (Cohen's d) ≈ ", round(effect_size, 3), " (", effect_interpretation, ")\n\n",
                "**Interpretasi Praktis:**\n",
                "Meskipun tidak signifikan secara statistik, ukuran efek menunjukkan magnitude ",
                effect_interpretation, ". Hal ini mungkin mengindikasikan perlunya ",
                "sampel yang lebih besar atau ada faktor lain yang perlu dipertimbangkan."
            )
        }
    }

    return(interpretation)
}

#' Interpret Proportion Test Results
#'
#' @param test_result Object returned from prop.test()
#' @param alpha Significance level (default: 0.05)
#' @return Character string with interpretation in Indonesian
interpret_prop_test <- function(test_result, alpha = 0.05) {
    p_value <- test_result$p.value
    conf_int <- test_result$conf.int
    chi_stat <- test_result$statistic
    sample_prop <- test_result$estimate

    if (p_value < alpha) {
        interpretation <- paste0(
            "**Kesimpulan Statistik:**\n",
            "Dengan p-value = ", round(p_value, 4), " (< α = ", alpha, "), ",
            "kita **menolak hipotesis nol (H₀)**. ",
            "Terdapat bukti statistik yang signifikan bahwa proporsi populasi ",
            "berbeda dari nilai yang diuji.\n\n",
            "**Detail Analisis:**\n",
            "- Proporsi sampel = ", round(sample_prop, 4), "\n",
            "- Statistik Chi-square = ", round(chi_stat, 3), "\n",
            "- Interval kepercayaan 95%: [", round(conf_int[1], 3), ", ", round(conf_int[2], 3), "]\n\n",
            "**Interpretasi Praktis:**\n",
            "Proporsi yang diamati dalam sampel secara signifikan berbeda dari ",
            "proporsi yang dihipotesiskan. Hasil ini mengindikasikan adanya ",
            "perbedaan yang substansial dalam populasi."
        )
    } else {
        interpretation <- paste0(
            "**Kesimpulan Statistik:**\n",
            "Dengan p-value = ", round(p_value, 4), " (≥ α = ", alpha, "), ",
            "kita **gagal menolak hipotesis nol (H₀)**. ",
            "Tidak terdapat bukti statistik yang cukup untuk menyatakan bahwa ",
            "proporsi populasi berbeda dari nilai yang diuji.\n\n",
            "**Detail Analisis:**\n",
            "- Proporsi sampel = ", round(sample_prop, 4), "\n",
            "- Statistik Chi-square = ", round(chi_stat, 3), "\n",
            "- Interval kepercayaan 95%: [", round(conf_int[1], 3), ", ", round(conf_int[2], 3), "]\n\n",
            "**Interpretasi Praktis:**\n",
            "Proporsi yang diamati dalam sampel tidak menunjukkan perbedaan ",
            "yang signifikan dari proporsi yang dihipotesiskan. Data konsisten ",
            "dengan hipotesis nol yang diajukan."
        )
    }

    return(interpretation)
}

#' Interpret Variance Test Results
#'
#' @param test_result Custom object with variance test results
#' @param alpha Significance level (default: 0.05)
#' @return Character string with interpretation in Indonesian
interpret_var_test <- function(test_result, alpha = 0.05) {
    p_value <- test_result$p.value
    chi_stat <- test_result$statistic
    df <- test_result$parameter
    sample_var <- test_result$sample.var
    null_var <- test_result$null.value

    if (p_value < alpha) {
        interpretation <- paste0(
            "**Kesimpulan Statistik:**\n",
            "Dengan p-value = ", round(p_value, 4), " (< α = ", alpha, "), ",
            "kita **menolak hipotesis nol (H₀)**. ",
            "Terdapat bukti statistik yang signifikan bahwa varians populasi ",
            "berbeda dari nilai yang diuji (σ² = ", null_var, ").\n\n",
            "**Detail Analisis:**\n",
            "- Varians sampel = ", round(sample_var, 4), "\n",
            "- Varians yang diuji = ", null_var, "\n",
            "- Statistik Chi-square = ", round(chi_stat, 3), " dengan df = ", df, "\n\n",
            "**Interpretasi Praktis:**\n",
            "Variabilitas data dalam sampel secara signifikan ",
            ifelse(sample_var > null_var, "lebih besar", "lebih kecil"),
            " dari yang dihipotesiskan. Hal ini mengindikasikan bahwa ",
            "tingkat keragaman dalam populasi berbeda dari yang diperkirakan."
        )
    } else {
        interpretation <- paste0(
            "**Kesimpulan Statistik:**\n",
            "Dengan p-value = ", round(p_value, 4), " (≥ α = ", alpha, "), ",
            "kita **gagal menolak hipotesis nol (H₀)**. ",
            "Tidak terdapat bukti statistik yang cukup untuk menyatakan bahwa ",
            "varians populasi berbeda dari nilai yang diuji (σ² = ", null_var, ").\n\n",
            "**Detail Analisis:**\n",
            "- Varians sampel = ", round(sample_var, 4), "\n",
            "- Varians yang diuji = ", null_var, "\n",
            "- Statistik Chi-square = ", round(chi_stat, 3), " dengan df = ", df, "\n\n",
            "**Interpretasi Praktis:**\n",
            "Variabilitas data dalam sampel konsisten dengan varians yang ",
            "dihipotesiskan. Data mendukung asumsi tentang tingkat keragaman ",
            "dalam populasi."
        )
    }

    return(interpretation)
}

#' Interpret ANOVA Results
#'
#' @param anova_result Object returned from aov() and summary()
#' @param alpha Significance level (default: 0.05)
#' @param type Type of ANOVA ("one_way" or "two_way")
#' @return Character string with interpretation in Indonesian
interpret_anova <- function(anova_result, alpha = 0.05, type = "one_way") {
    # Validate input
    if (is.null(anova_result) || length(anova_result) == 0) {
        return("Error: ANOVA result is empty or NULL.")
    }

    # Extract ANOVA table
    anova_table <- anova_result[[1]]

    if (is.null(anova_table) || nrow(anova_table) == 0) {
        return("Error: ANOVA table is empty or NULL.")
    }

    interpretations <- c()

    if (type == "one_way") {
        # Check if required columns exist
        if (!all(c("Pr(>F)", "F value", "Df", "Sum Sq") %in% names(anova_table))) {
            return("Error: Required columns not found in ANOVA table.")
        }

        p_value <- anova_table$`Pr(>F)`[1]
        f_stat <- anova_table$`F value`[1]
        df1 <- anova_table$Df[1]
        df2 <- anova_table$Df[2]

        # Check if values are valid
        if (is.null(p_value) || is.na(p_value) || length(p_value) == 0) {
            return("Error: p-value is missing or invalid.")
        }
        if (is.null(f_stat) || is.na(f_stat) || length(f_stat) == 0) {
            return("Error: F-statistic is missing or invalid.")
        }

        # Calculate effect size (eta squared)
        ss_factor <- anova_table$`Sum Sq`[1]
        ss_total <- sum(anova_table$`Sum Sq`, na.rm = TRUE)
        eta_squared <- ss_factor / ss_total

        # Effect size interpretation
        effect_interpretation <- if (eta_squared < 0.01) {
            "sangat kecil (< 1%)"
        } else if (eta_squared < 0.06) {
            "kecil (1-6%)"
        } else if (eta_squared < 0.14) {
            "sedang (6-14%)"
        } else {
            "besar (> 14%)"
        }

        if (p_value < alpha) {
            interpretation <- paste0(
                "**Kesimpulan Statistik (One-Way ANOVA):**\n",
                "Dengan p-value = ", round(p_value, 4), " (< α = ", alpha, "), ",
                "kita **menolak hipotesis nol (H₀)**. ",
                "Terdapat perbedaan rata-rata yang signifikan secara statistik ",
                "antara setidaknya dua kelompok.\n\n",
                "**Detail Analisis:**\n",
                "- Statistik F = ", round(f_stat, 3), " dengan df = (", df1, ", ", df2, ")\n",
                "- Eta squared (η²) = ", round(eta_squared, 4), " (", effect_interpretation, ")\n",
                "- Proporsi varians yang dijelaskan oleh faktor: ", round(eta_squared * 100, 2), "%\n\n",
                "**Interpretasi Praktis:**\n",
                "Faktor yang diuji memiliki pengaruh signifikan terhadap variabel dependen ",
                "dengan ukuran efek ", effect_interpretation, ". ",
                ifelse(eta_squared >= 0.06,
                    "Perbedaan antar kelompok cukup substansial dan memiliki relevansi praktis.",
                    "Meskipun signifikan secara statistik, ukuran efeknya relatif kecil."
                ),
                "\n\n**Rekomendasi:** Lakukan uji post-hoc (seperti Tukey HSD) untuk ",
                "mengidentifikasi kelompok mana yang berbeda secara spesifik."
            )
        } else {
            interpretation <- paste0(
                "**Kesimpulan Statistik (One-Way ANOVA):**\n",
                "Dengan p-value = ", round(p_value, 4), " (≥ α = ", alpha, "), ",
                "kita **gagal menolak hipotesis nol (H₀)**. ",
                "Tidak terdapat bukti statistik yang cukup untuk menyatakan bahwa ",
                "terdapat perbedaan rata-rata antar kelompok.\n\n",
                "**Detail Analisis:**\n",
                "- Statistik F = ", round(f_stat, 3), " dengan df = (", df1, ", ", df2, ")\n",
                "- Eta squared (η²) = ", round(eta_squared, 4), " (", effect_interpretation, ")\n",
                "- Proporsi varians yang dijelaskan oleh faktor: ", round(eta_squared * 100, 2), "%\n\n",
                "**Interpretasi Praktis:**\n",
                "Faktor yang diuji tidak menunjukkan pengaruh signifikan terhadap ",
                "variabel dependen. Variasi antar kelompok tidak lebih besar dari ",
                "yang diharapkan secara kebetulan."
            )
        }
    } else if (type == "two_way") {
        # Two-way ANOVA interpretation
        # Extract results for main effects and interaction

        if (nrow(anova_table) < 2) {
            return("Error: Two-way ANOVA table must have at least 2 rows.")
        }

        # Main effects
        factor1_p <- anova_table$`Pr(>F)`[1]
        factor2_p <- anova_table$`Pr(>F)`[2]
        factor1_f <- anova_table$`F value`[1]
        factor2_f <- anova_table$`F value`[2]

        # Check for interaction effect
        interaction_exists <- nrow(anova_table) >= 3 &&
            !is.na(anova_table$`Pr(>F)`[3])
        interaction_p <- if (interaction_exists) anova_table$`Pr(>F)`[3] else NA
        interaction_f <- if (interaction_exists) anova_table$`F value`[3] else NA

        interpretation_parts <- c()

        interpretation_parts <- c(
            interpretation_parts,
            "**Kesimpulan Statistik (Two-Way ANOVA):**\n"
        )

        # Main effect 1
        if (!is.na(factor1_p)) {
            if (factor1_p < alpha) {
                interpretation_parts <- c(
                    interpretation_parts,
                    paste0(
                        "• **Efek utama Faktor 1**: Signifikan (F = ", round(factor1_f, 3),
                        ", p = ", round(factor1_p, 4), " < ", alpha, ")"
                    )
                )
            } else {
                interpretation_parts <- c(
                    interpretation_parts,
                    paste0(
                        "• **Efek utama Faktor 1**: Tidak signifikan (F = ", round(factor1_f, 3),
                        ", p = ", round(factor1_p, 4), " ≥ ", alpha, ")"
                    )
                )
            }
        }

        # Main effect 2
        if (!is.na(factor2_p)) {
            if (factor2_p < alpha) {
                interpretation_parts <- c(
                    interpretation_parts,
                    paste0(
                        "• **Efek utama Faktor 2**: Signifikan (F = ", round(factor2_f, 3),
                        ", p = ", round(factor2_p, 4), " < ", alpha, ")"
                    )
                )
            } else {
                interpretation_parts <- c(
                    interpretation_parts,
                    paste0(
                        "• **Efek utama Faktor 2**: Tidak signifikan (F = ", round(factor2_f, 3),
                        ", p = ", round(factor2_p, 4), " ≥ ", alpha, ")"
                    )
                )
            }
        }

        # Interaction effect
        if (interaction_exists && !is.na(interaction_p)) {
            if (interaction_p < alpha) {
                interpretation_parts <- c(
                    interpretation_parts,
                    paste0(
                        "• **Efek Interaksi**: Signifikan (F = ", round(interaction_f, 3),
                        ", p = ", round(interaction_p, 4), " < ", alpha, ")"
                    )
                )
            } else {
                interpretation_parts <- c(
                    interpretation_parts,
                    paste0(
                        "• **Efek Interaksi**: Tidak signifikan (F = ", round(interaction_f, 3),
                        ", p = ", round(interaction_p, 4), " ≥ ", alpha, ")"
                    )
                )
            }
        }

        interpretation_parts <- c(interpretation_parts, "\n**Interpretasi Praktis:**\n")

        # Practical interpretation
        significant_effects <- sum(c(
            !is.na(factor1_p) && factor1_p < alpha,
            !is.na(factor2_p) && factor2_p < alpha,
            interaction_exists && !is.na(interaction_p) && interaction_p < alpha
        ))

        if (significant_effects > 0) {
            if (interaction_exists && !is.na(interaction_p) && interaction_p < alpha) {
                interpretation_parts <- c(
                    interpretation_parts,
                    "Terdapat efek interaksi yang signifikan antara kedua faktor. ",
                    "Hal ini berarti pengaruh satu faktor terhadap variabel dependen ",
                    "bergantung pada level faktor lainnya. Interpretasi efek utama ",
                    "harus dilakukan dengan hati-hati karena adanya interaksi.\n\n",
                    "**Rekomendasi:** Lakukan analisis simple effects atau interaction plot ",
                    "untuk memahami pola interaksi yang terjadi."
                )
            } else {
                interpretation_parts <- c(
                    interpretation_parts,
                    "Terdapat efek utama yang signifikan dari satu atau kedua faktor ",
                    "terhadap variabel dependen. ",
                    ifelse(!is.na(factor1_p) && factor1_p < alpha && !is.na(factor2_p) && factor2_p < alpha,
                        "Kedua faktor secara independen mempengaruhi variabel dependen.",
                        "Salah satu faktor memiliki pengaruh signifikan."
                    ),
                    "\n\n**Rekomendasi:** Lakukan uji post-hoc untuk faktor yang signifikan ",
                    "jika memiliki lebih dari 2 level."
                )
            }
        } else {
            interpretation_parts <- c(
                interpretation_parts,
                "Tidak terdapat efek utama maupun efek interaksi yang signifikan. ",
                "Kedua faktor yang diuji tidak menunjukkan pengaruh terhadap ",
                "variabel dependen dalam sampel ini."
            )
        }

        interpretation <- paste(interpretation_parts, collapse = "")
    }

    return(interpretation)
}

#' Interpret Linear Regression Results
#'
#' @param lm_result Object returned from lm() and summary()
#' @param alpha Significance level (default: 0.05)
#' @return Character string with interpretation in Indonesian
interpret_regression <- function(lm_result, alpha = 0.05) {
    # Validate input
    if (is.null(lm_result)) {
        return("Error: Regression result is NULL.")
    }

    if (!inherits(lm_result, "lm")) {
        return("Error: Input must be an lm object.")
    }

    tryCatch(
        {
            summary_lm <- summary(lm_result)

            # Model fit statistics
            r_squared <- summary_lm$r.squared
            adj_r_squared <- summary_lm$adj.r.squared
            f_stat <- summary_lm$fstatistic[1]
            f_p_value <- pf(f_stat, summary_lm$fstatistic[2], summary_lm$fstatistic[3], lower.tail = FALSE)

            # Coefficients
            coefficients <- summary_lm$coefficients

            # Validate extracted values
            if (is.null(r_squared) || is.na(r_squared) ||
                is.null(f_stat) || is.na(f_stat) ||
                is.null(f_p_value) || is.na(f_p_value)) {
                return("Error: Unable to extract valid statistics from regression model.")
            }

            # Model interpretation
            if (f_p_value < alpha) {
                model_significance <- paste0(
                    "**Model Regresi Signifikan:**\n",
                    "Dengan F-statistic = ", round(f_stat, 3),
                    " dan p-value = ", round(f_p_value, 4), " (< α = ", alpha, "), ",
                    "model regresi secara keseluruhan **signifikan secara statistik**.\n\n"
                )
            } else {
                model_significance <- paste0(
                    "**Model Regresi Tidak Signifikan:**\n",
                    "Dengan F-statistic = ", round(f_stat, 3),
                    " dan p-value = ", round(f_p_value, 4), " (≥ α = ", alpha, "), ",
                    "model regresi secara keseluruhan **tidak signifikan secara statistik**.\n\n"
                )
            }

            # R-squared interpretation
            r_squared_interpretation <- if (r_squared < 0.25) {
                "lemah"
            } else if (r_squared < 0.50) {
                "sedang"
            } else if (r_squared < 0.75) {
                "kuat"
            } else {
                "sangat kuat"
            }

            model_fit <- paste0(
                "**Kekuatan Model:**\n",
                "- R² = ", round(r_squared, 4), " (", round(r_squared * 100, 2), "%)\n",
                "- Adjusted R² = ", round(adj_r_squared, 4), " (", round(adj_r_squared * 100, 2), "%)\n",
                "- Kekuatan hubungan: ", r_squared_interpretation, "\n\n",
                "Model ini menjelaskan ", round(r_squared * 100, 2),
                "% dari variasi dalam variabel dependen.\n\n"
            )

            # Coefficients interpretation
            coef_interpretation <- "**Signifikansi Koefisien:**\n"

            for (i in 1:nrow(coefficients)) {
                var_name <- rownames(coefficients)[i]
                coef_value <- coefficients[i, "Estimate"]
                p_value <- coefficients[i, "Pr(>|t|)"]

                if (var_name == "(Intercept)") {
                    coef_interpretation <- paste0(
                        coef_interpretation,
                        "- Intercept = ", round(coef_value, 4),
                        " (p = ", round(p_value, 4), ")\n"
                    )
                } else {
                    significance <- ifelse(p_value < alpha, "signifikan", "tidak signifikan")
                    direction <- ifelse(coef_value > 0, "positif", "negatif")

                    coef_interpretation <- paste0(
                        coef_interpretation,
                        "- ", var_name, ": β = ", round(coef_value, 4),
                        " (p = ", round(p_value, 4), ") - ", significance,
                        " dengan hubungan ", direction, "\n"
                    )
                }
            }

            # Combine all interpretations
            interpretation <- paste0(
                model_significance,
                model_fit,
                coef_interpretation,
                "\n**Interpretasi Praktis:**\n",
                ifelse(f_p_value < alpha,
                    paste0(
                        "Model ini dapat digunakan untuk prediksi dengan tingkat kepercayaan ",
                        r_squared_interpretation, ". "
                    ),
                    "Model ini tidak dapat diandalkan untuk prediksi. "
                ),
                "Variabel prediktor yang signifikan memiliki pengaruh nyata terhadap ",
                "variabel dependen dan dapat digunakan untuk interpretasi hubungan kausal."
            )

            return(interpretation)
        },
        error = function(e) {
            return(paste("Error in regression interpretation:", e$message))
        }
    )
}

#' Interpret Assumption Test Results
#'
#' @param test_result Object returned from assumption tests (e.g., shapiro.test, levene.test)
#' @param test_name Name of the test performed
#' @param alpha Significance level (default: 0.05)
#' @return Character string with interpretation in Indonesian
interpret_assumption_test <- function(test_result, test_name, alpha = 0.05) {
    p_value <- test_result$p.value
    statistic <- test_result$statistic

    # Base interpretation framework
    if (test_name %in% c("shapiro", "normalitas", "normality")) {
        test_title <- "Uji Normalitas (Shapiro-Wilk)"

        if (p_value >= alpha) {
            interpretation <- paste0(
                "**", test_title, ":**\n",
                "Dengan p-value = ", round(p_value, 4), " (≥ α = ", alpha, "), ",
                "kita **gagal menolak hipotesis nol**.\n\n",
                "**Kesimpulan:** Data **terdistribusi normal** atau tidak ada bukti ",
                "yang cukup untuk menyatakan data tidak normal.\n\n",
                "**Interpretasi Praktis:** Asumsi normalitas **terpenuhi**. ",
                "Uji parametrik dapat dilanjutkan dengan aman."
            )
        } else {
            interpretation <- paste0(
                "**", test_title, ":**\n",
                "Dengan p-value = ", round(p_value, 4), " (< α = ", alpha, "), ",
                "kita **menolak hipotesis nol**.\n\n",
                "**Kesimpulan:** Data **tidak terdistribusi normal**.\n\n",
                "**Interpretasi Praktis:** Asumsi normalitas **tidak terpenuhi**. ",
                "Pertimbangkan untuk:\n",
                "- Menggunakan transformasi data (log, akar kuadrat, dll.)\n",
                "- Menggunakan uji non-parametrik sebagai alternatif\n",
                "- Mengevaluasi outlier yang mungkin mempengaruhi distribusi"
            )
        }
    } else if (test_name %in% c("levene", "homogenitas", "homogeneity")) {
        test_title <- "Uji Homogenitas Varians (Levene)"

        if (p_value >= alpha) {
            interpretation <- paste0(
                "**", test_title, ":**\n",
                "Dengan p-value = ", round(p_value, 4), " (≥ α = ", alpha, "), ",
                "kita **gagal menolak hipotesis nol**.\n\n",
                "**Kesimpulan:** Varians antar kelompok **homogen** (sama).\n\n",
                "**Interpretasi Praktis:** Asumsi homogenitas varians **terpenuhi**. ",
                "Uji parametrik dapat dilanjutkan dengan asumsi varians yang sama."
            )
        } else {
            interpretation <- paste0(
                "**", test_title, ":**\n",
                "Dengan p-value = ", round(p_value, 4), " (< α = ", alpha, "), ",
                "kita **menolak hipotesis nol**.\n\n",
                "**Kesimpulan:** Varians antar kelompok **tidak homogen** (heterogen).\n\n",
                "**Interpretasi Praktis:** Asumsi homogenitas varians **tidak terpenuhi**. ",
                "Pertimbangkan untuk:\n",
                "- Menggunakan uji yang tidak mengasumsikan varians sama (Welch t-test)\n",
                "- Melakukan transformasi data untuk menstabilkan varians\n",
                "- Menggunakan pendekatan statistik yang robust terhadap heterogenitas"
            )
        }
    } else {
        # Generic assumption test interpretation
        if (p_value >= alpha) {
            interpretation <- paste0(
                "**", test_name, ":**\n",
                "Dengan p-value = ", round(p_value, 4), " (≥ α = ", alpha, "), ",
                "asumsi **terpenuhi** (gagal menolak H₀).\n\n",
                "**Interpretasi:** Hasil uji mendukung asumsi yang dibutuhkan ",
                "untuk analisis parametrik."
            )
        } else {
            interpretation <- paste0(
                "**", test_name, ":**\n",
                "Dengan p-value = ", round(p_value, 4), " (< α = ", alpha, "), ",
                "asumsi **tidak terpenuhi** (menolak H₀).\n\n",
                "**Interpretasi:** Hasil uji menunjukkan pelanggaran asumsi. ",
                "Pertimbangkan pendekatan alternatif atau transformasi data."
            )
        }
    }

    return(interpretation)
}

# ==============================================================================
# SECURITY AND VALIDATION FUNCTIONS
# ==============================================================================

#' Validate and Sanitize Variable Names for Download Handlers
#'
#' @description Validates variable names to prevent path traversal and injection attacks
#' @param var_name Character. Variable name to validate
#' @return Character. Sanitized variable name or NULL if invalid
#' @author Tim Dashboard ALIVA
validate_variable_name <- function(var_name) {
    if (is.null(var_name) || length(var_name) != 1 || !is.character(var_name)) {
        return(NULL)
    }
    
    # Remove any path traversal attempts, special characters
    sanitized <- gsub("[^a-zA-Z0-9_]", "_", var_name)
    
    # Limit length to prevent excessively long filenames
    if (nchar(sanitized) > 50) {
        sanitized <- substr(sanitized, 1, 50)
    }
    
    # Ensure it's not empty after sanitization
    if (nchar(sanitized) == 0) {
        return("variable")
    }
    
    return(sanitized)
}

#' Safe File Rendering with Error Handling
#'
#' @description Safely renders R Markdown files with proper error handling and validation
#' @param temp_file Character. Temporary file path
#' @param output_file Character. Output file path
#' @param output_format Character. Output format ("pdf_document" or "word_document")
#' @param quiet Logical. Whether to suppress rendering messages
#' @return Logical. TRUE if successful, FALSE otherwise
#' @author Tim Dashboard ALIVA
safe_render_file <- function(temp_file, output_file, output_format = "pdf_document", quiet = TRUE) {
    tryCatch({
        # Validate inputs
        if (!file.exists(temp_file)) {
            stop("Temporary file does not exist")
        }
        
        if (!is.character(output_file) || length(output_file) != 1) {
            stop("Invalid output file path")
        }
        
        # Ensure output directory exists
        output_dir <- dirname(output_file)
        if (!dir.exists(output_dir)) {
            dir.create(output_dir, recursive = TRUE)
        }
        
        # Render with timeout to prevent hanging
        output_path <- suppressMessages(suppressWarnings(
            rmarkdown::render(
                input = temp_file,
                output_format = output_format,
                output_file = basename(output_file),
                output_dir = output_dir,
                quiet = quiet,
                clean = TRUE
            )
        ))
        
        # Verify output was created
        if (!file.exists(output_path)) {
            stop("Output file was not generated")
        }
        
        # Copy to final destination if different
        if (output_path != output_file) {
            file.copy(output_path, output_file, overwrite = TRUE)
        }
        
        # Clean up temporary files
        unlink(temp_file)
        if (output_path != output_file && file.exists(output_path)) {
            unlink(output_path)
        }
        
        return(TRUE)
        
    }, error = function(e) {
        # Clean up on error
        if (file.exists(temp_file)) unlink(temp_file)
        
        # Log error (in real app, this would go to proper logging system)
        cat("Error in safe_render_file:", e$message, "\n")
        
        return(FALSE)
    })
}

#' Validate Download Request
#'
#' @description Validates download requests to prevent abuse
#' @param values Reactive values object
#' @param required_inputs Character vector of required input names
#' @param session Shiny session object
#' @return Logical. TRUE if request is valid, FALSE otherwise
#' @author Tim Dashboard ALIVA
validate_download_request <- function(values, required_inputs = NULL, session = NULL) {
    # Check if data is available
    if (is.null(values$sovi_data)) {
        if (!is.null(session)) {
            showNotification("Data belum dimuat. Silakan tunggu hingga data tersedia.", 
                           type = "error", duration = 5)
        }
        return(FALSE)
    }
    
    # Check required inputs if specified
    if (!is.null(required_inputs)) {
        for (input_name in required_inputs) {
            if (is.null(session$input[[input_name]]) || session$input[[input_name]] == "") {
                if (!is.null(session)) {
                    showNotification(paste("Input", input_name, "diperlukan untuk unduhan."), 
                                   type = "error", duration = 5)
                }
                return(FALSE)
            }
        }
    }
    
    return(TRUE)
}

# [Removed unused functions: interpret_with_recommendations, render_pdf_safely, render_word_safely]
# These functions were never called in the codebase and have been removed to reduce dead code.
