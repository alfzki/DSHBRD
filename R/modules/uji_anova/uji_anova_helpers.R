# Uji ANOVA Helpers
# Fungsi-fungsi bantuan untuk modul uji ANOVA

# Fungsi untuk melakukan One-Way ANOVA
#' Melakukan One-Way ANOVA
#'
#' Fungsi ini melakukan analisis ANOVA satu arah
#'
#' @param data Data frame berisi data
#' @param dep_var Nama variabel dependen (numerik)
#' @param factor1 Nama variabel independen (faktor)
#' @param use_welch Logical, apakah menggunakan Welch's ANOVA (default: FALSE)
#' @return List berisi hasil ANOVA dan informasi terkait
perform_one_way_anova <- function(data, dep_var, factor1, use_welch = FALSE) {
    # Persiapan data
    dep_data <- data[[dep_var]]
    factor_data <- as.factor(data[[factor1]])
    
    # Buat data frame untuk analisis
    anova_data <- data.frame(
        y = dep_data,
        group = factor_data
    )
    
    # Hapus missing values
    anova_data <- anova_data[complete.cases(anova_data), ]
    
    if (nrow(anova_data) < 3) {
        stop("Data tidak cukup untuk ANOVA (minimal 3 observasi)")
    }
    
    # Lakukan ANOVA
    if (use_welch) {
        # Welch's ANOVA (tidak mengasumsikan varians sama)
        anova_result <- oneway.test(y ~ group, data = anova_data, var.equal = FALSE)
        anova_type <- "Welch's One-Way ANOVA"
    } else {
        # ANOVA klasik
        lm_result <- lm(y ~ group, data = anova_data)
        anova_result <- anova(lm_result)
        anova_type <- "One-Way ANOVA"
    }
    
    # Hitung statistik deskriptif per grup
    group_stats <- aggregate(anova_data$y, by = list(anova_data$group), function(x) {
        c(n = length(x), mean = mean(x), sd = sd(x), min = min(x), max = max(x))
    })
    names(group_stats) <- c("Grup", "Statistik")
    
    # Hitung effect size (eta squared untuk ANOVA klasik)
    if (!use_welch) {
        ss_between <- anova_result$"Sum Sq"[1]
        ss_total <- sum(anova_result$"Sum Sq")
        eta_squared <- ss_between / ss_total
    } else {
        eta_squared <- NA
    }
    
    list(
        result = anova_result,
        type = anova_type,
        group_stats = group_stats,
        eta_squared = eta_squared,
        data = anova_data,
        use_welch = use_welch
    )
}

# Fungsi untuk melakukan Two-Way ANOVA
#' Melakukan Two-Way ANOVA
#'
#' Fungsi ini melakukan analisis ANOVA dua arah
#'
#' @param data Data frame berisi data
#' @param dep_var Nama variabel dependen (numerik)
#' @param factor1 Nama faktor pertama
#' @param factor2 Nama faktor kedua
#' @param include_interaction Logical, apakah menyertakan interaksi (default: TRUE)
#' @return List berisi hasil ANOVA dan informasi terkait
perform_two_way_anova <- function(data, dep_var, factor1, factor2, include_interaction = TRUE) {
    # Persiapan data
    dep_data <- data[[dep_var]]
    factor1_data <- as.factor(data[[factor1]])
    factor2_data <- as.factor(data[[factor2]])
    
    # Buat data frame untuk analisis
    anova_data <- data.frame(
        y = dep_data,
        factor1 = factor1_data,
        factor2 = factor2_data
    )
    
    # Hapus missing values
    anova_data <- anova_data[complete.cases(anova_data), ]
    
    if (nrow(anova_data) < 5) {
        stop("Data tidak cukup untuk Two-Way ANOVA (minimal 5 observasi)")
    }
    
    # Buat formula
    if (include_interaction) {
        formula_str <- "y ~ factor1 * factor2"
    } else {
        formula_str <- "y ~ factor1 + factor2"
    }
    
    # Lakukan ANOVA
    lm_result <- lm(as.formula(formula_str), data = anova_data)
    anova_result <- anova(lm_result)
    
    # Hitung statistik deskriptif per kombinasi grup
    group_stats <- aggregate(anova_data$y, 
                           by = list(anova_data$factor1, anova_data$factor2), 
                           function(x) {
                               c(n = length(x), mean = mean(x), sd = sd(x))
                           })
    names(group_stats) <- c(factor1, factor2, "Statistik")
    
    # Hitung effect sizes
    ss_total <- sum(anova_result$"Sum Sq")
    eta_squared <- anova_result$"Sum Sq" / ss_total
    
    list(
        result = anova_result,
        type = "Two-Way ANOVA",
        group_stats = group_stats,
        eta_squared = eta_squared,
        data = anova_data,
        formula = formula_str,
        lm_result = lm_result
    )
}

# Fungsi untuk melakukan uji post-hoc
#' Melakukan Uji Post-Hoc
#'
#' Fungsi ini melakukan berbagai uji post-hoc setelah ANOVA signifikan
#'
#' @param anova_data Data frame hasil ANOVA
#' @param dep_var Nama variabel dependen
#' @param factor_var Nama variabel faktor
#' @param method Metode post-hoc: "tukey", "bonferroni", "holm", "lsd"
#' @return List berisi hasil uji post-hoc
perform_posthoc_tests <- function(anova_data, dep_var = "y", factor_var = "group", method = "tukey") {
    
    # Lakukan uji berdasarkan metode yang dipilih
    if (method == "tukey") {
        # Tukey HSD
        aov_result <- aov(as.formula(paste(dep_var, "~", factor_var)), data = anova_data)
        posthoc_result <- TukeyHSD(aov_result)
        
        result_df <- data.frame(
            Perbandingan = rownames(posthoc_result[[factor_var]]),
            Selisih_Rerata = round(posthoc_result[[factor_var]][, "diff"], 4),
            Batas_Bawah = round(posthoc_result[[factor_var]][, "lwr"], 4),
            Batas_Atas = round(posthoc_result[[factor_var]][, "upr"], 4),
            P_value = round(posthoc_result[[factor_var]][, "p adj"], 6),
            Signifikan = ifelse(posthoc_result[[factor_var]][, "p adj"] < 0.05, "Ya", "Tidak"),
            stringsAsFactors = FALSE
        )
        
    } else {
        # Pairwise t-tests dengan koreksi
        adj_method <- switch(method,
                           "bonferroni" = "bonferroni",
                           "holm" = "holm", 
                           "lsd" = "none",
                           "bonferroni")
        
        pairwise_result <- pairwise.t.test(anova_data[[dep_var]], 
                                         anova_data[[factor_var]], 
                                         p.adjust.method = adj_method)
        
        # Konversi ke format yang mudah dibaca
        p_matrix <- pairwise_result$p.value
        n_groups <- nrow(p_matrix) + 1
        group_names <- c(colnames(p_matrix), rownames(p_matrix)[nrow(p_matrix)])
        
        comparisons <- c()
        p_values <- c()
        
        for (i in 1:(n_groups - 1)) {
            for (j in (i + 1):n_groups) {
                comparisons <- c(comparisons, paste(group_names[j], "-", group_names[i]))
                p_values <- c(p_values, p_matrix[j - 1, i])
            }
        }
        
        result_df <- data.frame(
            Perbandingan = comparisons,
            P_value = round(p_values, 6),
            Signifikan = ifelse(p_values < 0.05, "Ya", "Tidak"),
            stringsAsFactors = FALSE
        )
    }
    
    list(
        method = switch(method,
                       "tukey" = "Tukey HSD",
                       "bonferroni" = "Bonferroni",
                       "holm" = "Holm",
                       "lsd" = "Fisher's LSD"),
        results = result_df,
        raw_result = if (method == "tukey") posthoc_result else pairwise_result
    )
}

# Fungsi untuk membuat plot ANOVA
#' Membuat Plot untuk ANOVA
#'
#' Fungsi ini membuat berbagai jenis plot untuk analisis ANOVA
#'
#' @param anova_data Data frame hasil ANOVA
#' @param dep_var Nama variabel dependen
#' @param factor_var Nama variabel faktor (atau faktor pertama untuk Two-Way)
#' @param factor2_var Nama faktor kedua (untuk Two-Way ANOVA)
#' @param plot_type Jenis plot: "boxplot", "violin", "means", "interaction"
#' @return Objek plotly
create_anova_plot <- function(anova_data, dep_var = "y", factor_var = "group", 
                             factor2_var = NULL, plot_type = "boxplot") {
    
    if (plot_type == "boxplot") {
        if (is.null(factor2_var)) {
            # One-Way ANOVA boxplot
            p <- plot_ly(anova_data, x = ~get(factor_var), y = ~get(dep_var), 
                        type = "box", color = ~get(factor_var)) %>%
                layout(title = paste("Boxplot:", dep_var, "berdasarkan", factor_var),
                       xaxis = list(title = factor_var),
                       yaxis = list(title = dep_var),
                       showlegend = FALSE)
        } else {
            # Two-Way ANOVA boxplot
            p <- plot_ly(anova_data, x = ~get(factor_var), y = ~get(dep_var), 
                        color = ~get(factor2_var), type = "box") %>%
                layout(title = paste("Boxplot:", dep_var, "berdasarkan", factor_var, "dan", factor2_var),
                       xaxis = list(title = factor_var),
                       yaxis = list(title = dep_var))
        }
        
    } else if (plot_type == "violin") {
        if (is.null(factor2_var)) {
            p <- plot_ly(anova_data, x = ~get(factor_var), y = ~get(dep_var), 
                        type = "violin", color = ~get(factor_var)) %>%
                layout(title = paste("Violin Plot:", dep_var, "berdasarkan", factor_var),
                       xaxis = list(title = factor_var),
                       yaxis = list(title = dep_var),
                       showlegend = FALSE)
        } else {
            p <- plot_ly(anova_data, x = ~get(factor_var), y = ~get(dep_var), 
                        color = ~get(factor2_var), type = "violin") %>%
                layout(title = paste("Violin Plot:", dep_var, "berdasarkan", factor_var, "dan", factor2_var),
                       xaxis = list(title = factor_var),
                       yaxis = list(title = dep_var))
        }
        
    } else if (plot_type == "means") {
        # Plot rerata dengan error bars
        if (is.null(factor2_var)) {
            means_data <- aggregate(anova_data[[dep_var]], 
                                  by = list(anova_data[[factor_var]]), 
                                  FUN = function(x) c(mean = mean(x), se = sd(x)/sqrt(length(x))))
            means_df <- data.frame(
                group = means_data$Group.1,
                mean = means_data$x[, "mean"],
                se = means_data$x[, "se"]
            )
            
            p <- plot_ly(means_df, x = ~group, y = ~mean, type = "scatter", mode = "markers+lines",
                        error_y = ~list(array = se, color = "red")) %>%
                layout(title = paste("Rerata", dep_var, "berdasarkan", factor_var),
                       xaxis = list(title = factor_var),
                       yaxis = list(title = paste("Rerata", dep_var)))
        } else {
            # Two-way means plot - akan dibuat interaction plot
            plot_type <- "interaction"
        }
        
    }
    
    if (plot_type == "interaction" && !is.null(factor2_var)) {
        # Interaction plot untuk Two-Way ANOVA
        interaction_data <- aggregate(anova_data[[dep_var]], 
                                    by = list(anova_data[[factor_var]], anova_data[[factor2_var]]), 
                                    FUN = mean)
        names(interaction_data) <- c("factor1", "factor2", "mean")
        
        p <- plot_ly(interaction_data, x = ~factor1, y = ~mean, color = ~factor2, 
                    type = "scatter", mode = "lines+markers") %>%
            layout(title = paste("Interaction Plot:", dep_var),
                   xaxis = list(title = factor_var),
                   yaxis = list(title = paste("Rerata", dep_var)))
    }
    
    return(p)
}

# Fungsi untuk interpretasi hasil ANOVA
#' Interpretasi Hasil ANOVA
#'
#' Fungsi ini memberikan interpretasi otomatis hasil ANOVA
#'
#' @param anova_result Hasil ANOVA dari fungsi perform_*_anova
#' @param alpha Tingkat signifikansi (default: 0.05)
#' @return String berisi interpretasi hasil
interpret_anova_results <- function(anova_result, alpha = 0.05) {
    result_text <- paste0(
        "INTERPRETASI HASIL ", anova_result$type, "\n",
        "=================================================\n\n"
    )
    
    if (anova_result$use_welch) {
        # Welch's ANOVA interpretation
        f_stat <- anova_result$result$statistic
        p_value <- anova_result$result$p.value
        df1 <- anova_result$result$parameter[1]
        df2 <- anova_result$result$parameter[2]
        
        result_text <- paste0(result_text,
            "Uji Statistik: ", round(f_stat, 4), "\n",
            "Derajat Bebas: ", round(df1, 2), " dan ", round(df2, 2), "\n",
            "Nilai P: ", format(p_value, scientific = TRUE), "\n\n"
        )
        
    } else if (anova_result$type == "One-Way ANOVA") {
        # Regular one-way ANOVA
        f_stat <- anova_result$result$"F value"[1]
        p_value <- anova_result$result$"Pr(>F)"[1]
        df1 <- anova_result$result$Df[1]
        df2 <- anova_result$result$Df[2]
        
        result_text <- paste0(result_text,
            "F-statistik: ", round(f_stat, 4), "\n",
            "Derajat Bebas: ", df1, " dan ", df2, "\n",
            "Nilai P: ", format(p_value, scientific = TRUE), "\n"
        )
        
        if (!is.na(anova_result$eta_squared)) {
            result_text <- paste0(result_text,
                "Eta Squared (η²): ", round(anova_result$eta_squared, 4), "\n"
            )
        }
        
    } else {
        # Two-way ANOVA
        effects <- rownames(anova_result$result)
        result_text <- paste0(result_text, "Hasil untuk setiap efek:\n\n")
        
        for (i in 1:length(effects)) {
            if (effects[i] != "Residuals") {
                f_stat <- anova_result$result$"F value"[i]
                p_value <- anova_result$result$"Pr(>F)"[i]
                eta_sq <- anova_result$eta_squared[i]
                
                result_text <- paste0(result_text,
                    effects[i], ":\n",
                    "  F = ", round(f_stat, 4), 
                    ", p = ", format(p_value, scientific = TRUE),
                    ", η² = ", round(eta_sq, 4), "\n"
                )
            }
        }
    }
    
    # Kesimpulan
    result_text <- paste0(result_text, "\n", "KESIMPULAN:\n")
    
    if (anova_result$use_welch) {
        if (p_value < alpha) {
            result_text <- paste0(result_text,
                "✓ Terdapat perbedaan signifikan antar kelompok (p < ", alpha, ")\n",
                "✓ Hipotesis nol DITOLAK\n",
                "✓ Disarankan untuk melakukan uji post-hoc\n"
            )
        } else {
            result_text <- paste0(result_text,
                "✗ Tidak terdapat perbedaan signifikan antar kelompok (p ≥ ", alpha, ")\n",
                "✗ Hipotesis nol GAGAL DITOLAK\n"
            )
        }
    } else {
        main_p <- if (anova_result$type == "One-Way ANOVA") {
            anova_result$result$"Pr(>F)"[1]
        } else {
            min(anova_result$result$"Pr(>F)"[1:(nrow(anova_result$result)-1)], na.rm = TRUE)
        }
        
        if (main_p < alpha) {
            result_text <- paste0(result_text,
                "✓ Terdapat efek signifikan (p < ", alpha, ")\n",
                "✓ Hipotesis nol DITOLAK\n",
                "✓ Disarankan untuk melakukan uji post-hoc\n"
            )
        } else {
            result_text <- paste0(result_text,
                "✗ Tidak terdapat efek signifikan (p ≥ ", alpha, ")\n",
                "✗ Hipotesis nol GAGAL DITOLAK\n"
            )
        }
    }
    
    # Effect size interpretation
    if (!is.na(anova_result$eta_squared) && anova_result$type == "One-Way ANOVA") {
        eta_sq <- anova_result$eta_squared
        effect_size <- ifelse(eta_sq < 0.01, "sangat kecil", 
                            ifelse(eta_sq < 0.06, "kecil",
                                   ifelse(eta_sq < 0.14, "sedang", "besar")))
        
        result_text <- paste0(result_text,
            "\nUKURAN EFEK:\n",
            "Effect size (η²) = ", round(eta_sq, 4), " (", effect_size, ")\n"
        )
    }
    
    return(result_text)
}

# Fungsi untuk membuat konten laporan ANOVA
#' Membuat Konten Laporan ANOVA
#'
#' Fungsi ini membuat template laporan untuk hasil ANOVA
#'
#' @param anova_result Hasil ANOVA
#' @param posthoc_result Hasil post-hoc (optional)
#' @param dep_var Nama variabel dependen
#' @param factors Nama faktor-faktor
#' @return Vector karakter berisi baris-baris R Markdown
create_anova_report_content <- function(anova_result, posthoc_result = NULL, dep_var, factors) {
    content <- c(
        "# Laporan Analisis ANOVA",
        "",
        paste("**Analisis:** ", anova_result$type),
        paste("**Variabel Dependen:** ", dep_var),
        paste("**Faktor:** ", paste(factors, collapse = ", ")),
        paste("**Tanggal Analisis:** ", Sys.Date()),
        "",
        "## Ringkasan Hasil",
        ""
    )
    
    # Tambahkan interpretasi
    interpretation <- interpret_anova_results(anova_result)
    content <- c(content, "```", interpretation, "```", "")
    
    # Tambahkan statistik deskriptif
    content <- c(content,
        "## Statistik Deskriptif",
        "",
        "Ringkasan data per kelompok tersedia dalam output dashboard.",
        ""
    )
    
    # Tambahkan hasil post-hoc jika ada
    if (!is.null(posthoc_result)) {
        content <- c(content,
            "## Uji Post-Hoc",
            "",
            paste("**Metode:** ", posthoc_result$method),
            "",
            "Hasil perbandingan berpasangan tersedia dalam tabel terpisah.",
            ""
        )
    }
    
    # Tambahkan rekomendasi
    content <- c(content,
        "## Rekomendasi",
        "",
        "1. Pastikan asumsi ANOVA telah diuji terlebih dahulu",
        "2. Jika ANOVA signifikan, lakukan uji post-hoc untuk identifikasi perbedaan spesifik",
        "3. Pertimbangkan ukuran efek dalam interpretasi practical significance",
        "4. Periksa plot untuk memahami pola perbedaan antar kelompok",
        "",
        "---",
        "*Laporan ini dihasilkan otomatis oleh ALIVA Dashboard*"
    )
    
    return(content)
}

# Fungsi untuk membuat nama file download ANOVA
#' Membuat Nama File Download ANOVA
#'
#' @param type Jenis file: "interpretation", "plot", "report"
#' @param format Format file: "pdf", "docx", "jpg", "png"
#' @return String nama file dengan timestamp
create_anova_filename <- function(type, format) {
    prefix <- paste0("ALIVA_ANOVA_", stringr::str_to_title(type))
    extension <- paste0(".", format)
    paste0(prefix, "_", Sys.Date(), extension)
}