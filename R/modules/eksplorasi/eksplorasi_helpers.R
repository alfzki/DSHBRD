# Eksplorasi Helpers
# Fungsi-fungsi bantuan untuk modul eksplorasi data

# Fungsi untuk membuat plot univariat berdasarkan jenis yang dipilih
#' Membuat Plot Univariat
#'
#' Fungsi ini membuat berbagai jenis plot untuk analisis univariat
#'
#' @param var_data Vektor data numerik yang akan diplot
#' @param var_name Nama variabel untuk judul plot
#' @param plot_type Jenis plot: "histogram", "density", "boxplot", "qqplot"
#' @param hist_bins Jumlah bins untuk histogram (opsional)
#' @return Objek plotly untuk visualisasi
create_univariate_plot <- function(var_data, var_name, plot_type, hist_bins = 20) {
    switch(plot_type,
        "histogram" = {
            plot_ly(x = ~var_data, type = "histogram", nbinsx = hist_bins) %>%
                layout(title = paste("Histogram:", var_name),
                       xaxis = list(title = var_name),
                       yaxis = list(title = "Frekuensi"))
        },
        "density" = {
            dens <- density(var_data, na.rm = TRUE)
            plot_ly(x = ~dens$x, y = ~dens$y, type = "scatter", mode = "lines", fill = "tozeroy") %>%
                layout(title = paste("Plot Densitas:", var_name),
                       xaxis = list(title = var_name),
                       yaxis = list(title = "Densitas"))
        },
        "boxplot" = {
            plot_ly(y = ~var_data, type = "box", name = var_name) %>%
                layout(title = paste("Boxplot:", var_name),
                       yaxis = list(title = var_name))
        },
        "qqplot" = {
            qqnorm_data <- qqnorm(var_data, plot.it = FALSE)
            plot_ly(x = ~qqnorm_data$x, y = ~qqnorm_data$y, type = "scatter", mode = "markers") %>%
                add_lines(x = ~qqnorm_data$x, y = ~qqnorm_data$x, line = list(color = "red")) %>%
                layout(title = paste("Q-Q Plot:", var_name),
                       xaxis = list(title = "Kuantil Teoretis"),
                       yaxis = list(title = "Kuantil Sampel"))
        }
    )
}

# Fungsi untuk menghitung statistik deskriptif univariat
#' Menghitung Statistik Deskriptif Univariat
#'
#' Fungsi ini menghitung ringkasan statistik untuk satu variabel
#'
#' @param var_data Vektor data numerik
#' @param var_name Nama variabel
#' @return String berisi ringkasan statistik yang diformat
calculate_univariate_summary <- function(var_data, var_name) {
    paste0(
        "STATISTIK DESKRIPTIF: ", var_name, "\n",
        "========================================\n",
        "Rerata: ", round(mean(var_data, na.rm = TRUE), 4), "\n",
        "Median: ", round(median(var_data, na.rm = TRUE), 4), "\n", 
        "Standar Deviasi: ", round(sd(var_data, na.rm = TRUE), 4), "\n",
        "Minimum: ", round(min(var_data, na.rm = TRUE), 4), "\n",
        "Maksimum: ", round(max(var_data, na.rm = TRUE), 4), "\n",
        "Q1: ", round(quantile(var_data, 0.25, na.rm = TRUE), 4), "\n",
        "Q3: ", round(quantile(var_data, 0.75, na.rm = TRUE), 4), "\n",
        "IQR: ", round(IQR(var_data, na.rm = TRUE), 4), "\n",
        "Skewness: ", round((3 * (mean(var_data, na.rm = TRUE) - median(var_data, na.rm = TRUE))) / sd(var_data, na.rm = TRUE), 4), "\n",
        "Missing Values: ", sum(is.na(var_data)), " (", round(sum(is.na(var_data))/length(var_data)*100, 2), "%)"
    )
}

# Fungsi untuk membuat plot bivariat
#' Membuat Plot Bivariat
#'
#' Fungsi ini membuat plot untuk analisis hubungan dua variabel
#'
#' @param x_data Vektor data untuk sumbu X
#' @param y_data Vektor data untuk sumbu Y
#' @param x_name Nama variabel X
#' @param y_name Nama variabel Y
#' @param plot_type Jenis plot: "scatter", "line", "smooth"
#' @return Objek plotly untuk visualisasi
create_bivariate_plot <- function(x_data, y_data, x_name, y_name, plot_type) {
    switch(plot_type,
        "scatter" = {
            plot_ly(x = ~x_data, y = ~y_data, type = "scatter", mode = "markers") %>%
                layout(title = paste("Scatter Plot:", x_name, "vs", y_name),
                       xaxis = list(title = x_name),
                       yaxis = list(title = y_name))
        },
        "line" = {
            plot_ly(x = ~x_data, y = ~y_data, type = "scatter", mode = "lines") %>%
                layout(title = paste("Line Plot:", x_name, "vs", y_name),
                       xaxis = list(title = x_name),
                       yaxis = list(title = y_name))
        },
        "smooth" = {
            plot_ly(x = ~x_data, y = ~y_data, type = "scatter", mode = "markers") %>%
                add_lines(y = ~fitted(loess(y_data ~ x_data)), line = list(color = "red")) %>%
                layout(title = paste("Smooth Plot:", x_name, "vs", y_name),
                       xaxis = list(title = x_name),
                       yaxis = list(title = y_name))
        }
    )
}

# Fungsi untuk menghitung korelasi dan membuat heatmap
#' Membuat Heatmap Korelasi
#'
#' Fungsi ini membuat heatmap korelasi untuk variabel-variabel yang dipilih
#'
#' @param data Data frame berisi variabel numerik
#' @param selected_vars Vektor nama variabel yang dipilih
#' @param method Metode korelasi: "pearson", "spearman", "kendall"
#' @return Objek plotly heatmap korelasi
create_correlation_heatmap <- function(data, selected_vars, method) {
    cor_data <- data[selected_vars]
    cor_matrix <- cor(cor_data, use = "complete.obs", method = method)
    
    plot_ly(
        x = colnames(cor_matrix),
        y = rownames(cor_matrix),
        z = cor_matrix,
        type = "heatmap",
        colorscale = "RdBu",
        zmin = -1,
        zmax = 1,
        text = ~round(cor_matrix, 3),
        texttemplate = "%{text}",
        textfont = list(size = 10)
    ) %>%
    layout(
        title = paste("Matriks Korelasi (", stringr::str_to_title(method), ")"),
        xaxis = list(title = "Variabel"),
        yaxis = list(title = "Variabel")
    )
}

# Fungsi untuk analisis kelompok
#' Membuat Plot Analisis Kelompok
#'
#' Fungsi ini membuat visualisasi untuk analisis berdasarkan kelompok
#'
#' @param data Data frame berisi data
#' @param group_var Nama variabel kategori untuk pengelompokan
#' @param target_var Nama variabel numerik target
#' @param analysis_type Jenis analisis: "boxplot", "violin", "density", "summary"
#' @return Objek plotly atau data frame tergantung jenis analisis
create_group_analysis <- function(data, group_var, target_var, analysis_type) {
    group_data <- data[[group_var]]
    target_data <- data[[target_var]]
    
    switch(analysis_type,
        "boxplot" = {
            plot_ly(x = ~group_data, y = ~target_data, type = "box") %>%
                layout(title = paste("Boxplot:", target_var, "berdasarkan", group_var),
                       xaxis = list(title = group_var),
                       yaxis = list(title = target_var))
        },
        "violin" = {
            plot_ly(x = ~group_data, y = ~target_data, type = "violin") %>%
                layout(title = paste("Violin Plot:", target_var, "berdasarkan", group_var),
                       xaxis = list(title = group_var),
                       yaxis = list(title = target_var))
        },
        "density" = {
            unique_groups <- unique(group_data[!is.na(group_data)])
            p <- plot_ly()
            
            for (group in unique_groups) {
                group_subset <- target_data[group_data == group & !is.na(group_data)]
                if (length(group_subset) > 1) {
                    dens <- density(group_subset, na.rm = TRUE)
                    p <- p %>% add_lines(x = ~dens$x, y = ~dens$y, name = as.character(group))
                }
            }
            
            p %>% layout(title = paste("Density Plot:", target_var, "berdasarkan", group_var),
                         xaxis = list(title = target_var),
                         yaxis = list(title = "Densitas"))
        },
        "summary" = {
            data %>%
                group_by(!!sym(group_var)) %>%
                summarise(
                    N = n(),
                    Rerata = round(mean(!!sym(target_var), na.rm = TRUE), 4),
                    Median = round(median(!!sym(target_var), na.rm = TRUE), 4),
                    SD = round(sd(!!sym(target_var), na.rm = TRUE), 4),
                    Min = round(min(!!sym(target_var), na.rm = TRUE), 4),
                    Max = round(max(!!sym(target_var), na.rm = TRUE), 4),
                    .groups = 'drop'
                )
        }
    )
}

# Fungsi untuk membuat ringkasan data tabel
#' Membuat Ringkasan Data Tabel
#'
#' Fungsi ini membuat ringkasan statistik untuk data yang ditampilkan dalam tabel
#'
#' @param data Data frame yang akan diringkas
#' @return String berisi informasi ringkasan data
create_table_summary <- function(data) {
    paste0(
        "RINGKASAN DATA\n",
        "==============\n",
        "Total Baris: ", nrow(data), "\n",
        "Total Kolom: ", ncol(data), "\n",
        "Kolom Numerik: ", sum(sapply(data, is.numeric)), "\n",
        "Kolom Kategorik: ", sum(sapply(data, function(x) is.character(x) || is.factor(x))), "\n",
        "Total Missing Values: ", sum(is.na(data)), "\n",
        "Penggunaan Memori: ", round(object.size(data) / 1024^2, 2), " MB"
    )
}

# Fungsi untuk membuat placeholder peta tematik
#' Membuat Placeholder Peta Tematik
#'
#' Fungsi ini membuat visualisasi placeholder untuk peta tematik
#'
#' @param data Data frame berisi data
#' @param map_var Nama variabel untuk pemetaan
#' @return Objek plotly scatter plot sebagai placeholder
create_thematic_map_placeholder <- function(data, map_var) {
    # Placeholder untuk peta tematik - dalam implementasi nyata akan menggunakan shapefile
    var_data <- data[[map_var]]
    
    plot_ly(
        x = ~sample(1:100, length(var_data), replace = TRUE),
        y = ~sample(1:100, length(var_data), replace = TRUE),
        z = ~var_data,
        type = "scatter",
        mode = "markers",
        color = ~var_data,
        colorscale = "Viridis",
        text = ~paste("Nilai:", round(var_data, 2)),
        hovertemplate = "%{text}<extra></extra>"
    ) %>%
    layout(
        title = paste("Peta Tematik:", map_var, "(Placeholder)"),
        xaxis = list(title = "Longitude (Simulasi)"),
        yaxis = list(title = "Latitude (Simulasi)"),
        showlegend = FALSE
    )
}

# Fungsi untuk membuat konten laporan R Markdown
#' Membuat Konten Dasar Laporan R Markdown
#'
#' Fungsi ini membuat template dasar untuk laporan eksplorasi
#'
#' @param data Data frame untuk analisis
#' @param report_type Jenis laporan: "interpretation", "pdf", "word"
#' @return Vector karakter berisi baris-baris R Markdown
create_report_content <- function(data = NULL, report_type = "interpretation") {
    base_content <- c(
        "# ALIVA Dashboard - Hasil Eksplorasi Data",
        "",
        "Dashboard ALIVA menyediakan berbagai tools untuk eksplorasi data komprehensif dengan fokus pada analisis kerentanan sosial Indonesia.",
        "",
        "## Ringkasan Dataset"
    )
    
    if (!is.null(data)) {
        data_summary <- c(
            paste("- **Jumlah Observasi:** ", nrow(data)),
            paste("- **Jumlah Variabel:** ", ncol(data)),
            paste("- **Variabel Numerik:** ", sum(sapply(data, is.numeric))),
            paste("- **Variabel Kategorik:** ", sum(sapply(data, function(x) is.character(x) || is.factor(x)))),
            ""
        )
        base_content <- c(base_content, data_summary)
    }
    
    analysis_content <- c(
        "## Fitur Eksplorasi Tersedia",
        "",
        "### 1. Analisis Univariat",
        "- Histogram untuk distribusi data",
        "- Density plot untuk pola kontinyu",
        "- Boxplot untuk deteksi outlier",
        "- Q-Q plot untuk uji normalitas",
        "",
        "### 2. Analisis Bivariat", 
        "- Scatter plot untuk hubungan linier",
        "- Line plot untuk trend temporal",
        "- Smooth plot dengan curve fitting",
        "",
        "### 3. Analisis Korelasi",
        "- Matriks korelasi Pearson, Spearman, Kendall",
        "- Heatmap visualisasi interaktif",
        "",
        "### 4. Analisis Kelompok",
        "- Boxplot perbandingan antar grup",
        "- Violin plot untuk distribusi grup", 
        "- Density plot bertumpuk",
        "- Ringkasan statistik per grup",
        "",
        "### 5. Eksplorasi Data Interaktif",
        "- Filter data dinamis",
        "- Tabel yang dapat disortir dan dicari",
        "",
        "### 6. Peta Tematik",
        "- Visualisasi geografis Indonesia",
        "- Gradasi warna berdasarkan nilai variabel"
    )
    
    c(base_content, analysis_content)
}

# Fungsi untuk membuat nama file download
#' Membuat Nama File Download Dengan Timestamp
#'
#' Fungsi ini membuat nama file unik untuk download dengan timestamp
#'
#' @param prefix Prefix nama file
#' @param extension Ekstensi file (dengan titik, misal: ".csv")
#' @return String nama file dengan timestamp
create_download_filename <- function(prefix, extension) {
    paste0(prefix, "_", Sys.Date(), extension)
}