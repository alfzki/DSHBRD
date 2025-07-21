# Beranda Helpers
# Fungsi-fungsi bantuan untuk modul beranda dashboard

# Fungsi untuk membuat konten selamat datang
#' Membuat Konten Selamat Datang
#'
#' Fungsi ini membuat konten HTML untuk halaman beranda dashboard
#'
#' @return Objek tagList berisi konten HTML
create_welcome_content <- function() {
    tagList(
        h3("ALIVA: Alif Vulnerability Analytics Dashboard"),
        p("Selamat datang di ALIVA Dashboard! Aplikasi ini dirancang khusus untuk analisis komprehensif
          data kerentanan sosial Indonesia dengan pendekatan statistik yang rigorous dan metodologi yang
          terstandarisasi."),
        div(
            class = "alert alert-info", role = "alert",
            strong("Objektif Utama Dashboard:"), br(),
            "Menyediakan platform analisis statistik terintegrasi untuk memahami pola kerentanan sosial
             di Indonesia menggunakan data SUSENAS 2017 dengan fokus pada analisis deskriptif, inferensia,
             dan modeling prediktif."
        ),
        hr(),
        h4("Metodologi Analisis Statistik:"),
        create_methodology_content(),
        hr(),
        h4("Sumber Data dan Validasi:"),
        create_data_source_info(),
        h5("Struktur Dataset:"),
        create_dataset_structure()
    )
}

# Fungsi untuk membuat konten metodologi
#' Membuat Konten Metodologi
#'
#' @return Objek HTML untuk bagian metodologi
create_methodology_content <- function() {
    tags$div(
        class = "row",
        tags$div(
            class = "col-md-6",
            h5(icon("chart-bar"), "Exploratory Data Analysis (EDA)"),
            tags$ul(
                tags$li("Statistik deskriptif univariat dan multivariat"),
                tags$li("Visualisasi distribusi dan hubungan antar variabel"),
                tags$li("Deteksi outlier dan pola spasial"),
                tags$li("Analisis korelasi dan clustering")
            )
        ),
        tags$div(
            class = "col-md-6",
            h5(icon("calculator"), "Statistical Inference"),
            tags$ul(
                tags$li("Uji asumsi normalitas (Shapiro-Wilk, Anderson-Darling)"),
                tags$li("Uji homogenitas varians (Levene, Bartlett)"),
                tags$li("T-tests (satu sampel, dua sampel, berpasangan)"),
                tags$li("ANOVA satu arah dan dua arah dengan post-hoc tests")
            )
        )
    )

    tags$div(
        class = "row",
        tags$div(
            class = "col-md-6",
            h5(icon("chart-line"), "Regression Modeling"),
            tags$ul(
                tags$li("Multiple Linear Regression dengan seleksi variabel"),
                tags$li("Diagnostik residual komprehensif"),
                tags$li("Uji multikolinearitas (VIF)"),
                tags$li("Model validation dan goodness-of-fit assessment")
            )
        ),
        tags$div(
            class = "col-md-6",
            h5(icon("map-marked"), "Spatial Analysis"),
            tags$ul(
                tags$li("Visualisasi peta tematik interaktif"),
                tags$li("Analisis pola geografis kerentanan"),
                tags$li("Perbandingan antar wilayah (provinsi, pulau)"),
                tags$li("Integrasi dengan data jarak antar kabupaten/kota")
            )
        )
    )
}

# Fungsi untuk membuat informasi sumber data
#' Membuat Informasi Sumber Data
#'
#' @return Objek div berisi informasi sumber data
create_data_source_info <- function() {
    div(
        class = "alert alert-success", role = "alert",
        p(strong("Primary Dataset: "), "SUSENAS (Survei Sosial Ekonomi Nasional) 2017"),
        p(strong("Sumber: "), "BPS-Statistics Indonesia"),
        p(strong("Cakupan: "), "511 kabupaten/kota di seluruh Indonesia"),
        p(strong("Validasi: "), "Data telah melalui quality control dan standardization")
    )
}

# Fungsi untuk membuat struktur dataset
#' Membuat Struktur Dataset
#'
#' @return Objek div berisi struktur dataset
create_dataset_structure <- function() {
    tags$div(
        class = "row",
        tags$div(
            class = "col-md-4",
            strong("Demografi & Sosial:"),
            tags$ul(
                style = "font-size: 0.9em;",
                tags$li("CHILDREN: Persentase balita (< 5 tahun)"),
                tags$li("FEMALE: Persentase populasi wanita"),
                tags$li("ELDERLY: Persentase lansia (≥ 65 tahun)"),
                tags$li("FHEAD: KK berjenis kelamin wanita")
            )
        ),
        tags$div(
            class = "col-md-4",
            strong("Ekonomi & Pendidikan:"),
            tags$ul(
                style = "font-size: 0.9em;",
                tags$li("POVERTY: Persentase penduduk miskin"),
                tags$li("ILLITERATE: Persentase buta huruf"),
                tags$li("LOWEDU: Pendidikan rendah"),
                tags$li("FAMILYSIZE: Rata-rata anggota RT")
            )
        ),
        tags$div(
            class = "col-md-4",
            strong("Infrastruktur & Lingkungan:"),
            tags$ul(
                style = "font-size: 0.9em;",
                tags$li("NOELECTRIC: RT tanpa listrik"),
                tags$li("TAPWATER: RT air ledeng/pipa"),
                tags$li("NOSEWER: RT tanpa drainase"),
                tags$li("DPRONE: Daerah rawan bencana")
            )
        )
    )
}

# Fungsi untuk membuat informasi dataset
#' Membuat Informasi Dataset
#'
#' @param sovi_data Data frame berisi data SOVI
#' @return Objek tagList berisi informasi dataset
create_dataset_info <- function(sovi_data) {
    if (is.null(sovi_data)) {
        return(div(
            class = "alert alert-warning",
            icon("exclamation-triangle"),
            " Data sedang dimuat..."
        ))
    }

    tagList(
        div(
            class = "info-box bg-blue",
            div(class = "info-box-icon", icon("database")),
            div(
                class = "info-box-content",
                span(class = "info-box-text", "Dataset SOVI"),
                span(class = "info-box-number", nrow(sovi_data), " observasi")
            )
        ),
        br(),
        div(
            class = "info-box bg-green",
            div(class = "info-box-icon", icon("list")),
            div(
                class = "info-box-content",
                span(class = "info-box-text", "Total Variabel"),
                span(class = "info-box-number", ncol(sovi_data))
            )
        ),
        br(),
        create_variable_type_info(sovi_data),
        br(),
        create_data_quality_info(sovi_data)
    )
}

# Fungsi untuk membuat informasi jenis variabel
#' Membuat Informasi Jenis Variabel
#'
#' @param sovi_data Data frame berisi data SOVI
#' @return Objek div berisi informasi jenis variabel
create_variable_type_info <- function(sovi_data) {
    numeric_count <- sum(sapply(sovi_data, is.numeric))
    categorical_count <- sum(sapply(sovi_data, function(x) is.character(x) || is.factor(x)))

    tags$div(
        class = "row",
        tags$div(
            class = "col-md-6",
            div(
                class = "info-box bg-yellow",
                div(class = "info-box-icon", icon("calculator")),
                div(
                    class = "info-box-content",
                    span(class = "info-box-text", "Variabel Numerik"),
                    span(class = "info-box-number", numeric_count)
                )
            )
        ),
        tags$div(
            class = "col-md-6",
            div(
                class = "info-box bg-purple",
                div(class = "info-box-icon", icon("tags")),
                div(
                    class = "info-box-content",
                    span(class = "info-box-text", "Variabel Kategorik"),
                    span(class = "info-box-number", categorical_count)
                )
            )
        )
    )
}

# Fungsi untuk membuat informasi kualitas data
#' Membuat Informasi Kualitas Data
#'
#' @param sovi_data Data frame berisi data SOVI
#' @return Objek div berisi informasi kualitas data
create_data_quality_info <- function(sovi_data) {
    missing_count <- sum(is.na(sovi_data))
    completeness_rate <- round((1 - missing_count / (nrow(sovi_data) * ncol(sovi_data))) * 100, 2)

    tags$div(
        class = "row",
        tags$div(
            class = "col-md-6",
            div(
                class = "info-box bg-red",
                div(class = "info-box-icon", icon("exclamation")),
                div(
                    class = "info-box-content",
                    span(class = "info-box-text", "Missing Values"),
                    span(class = "info-box-number", missing_count)
                )
            )
        ),
        tags$div(
            class = "col-md-6",
            div(
                class = if (completeness_rate > 95) "info-box bg-green" else if (completeness_rate > 85) "info-box bg-yellow" else "info-box bg-red",
                div(class = "info-box-icon", icon("check-circle")),
                div(
                    class = "info-box-content",
                    span(class = "info-box-text", "Kelengkapan Data"),
                    span(class = "info-box-number", paste0(completeness_rate, "%"))
                )
            )
        )
    )
}

# Fungsi untuk membuat tabel metadata SOVI
#' Membuat Tabel Metadata SOVI
#'
#' @param sovi_data Data frame berisi data SOVI
#' @return Objek DT::datatable berisi metadata variabel
create_sovi_metadata_table <- function(sovi_data) {
    if (is.null(sovi_data)) {
        return(NULL)
    }

    # Buat metadata untuk setiap variabel
    metadata <- data.frame(
        Variabel = names(sovi_data),
        Tipe = sapply(sovi_data, function(x) {
            if (is.numeric(x)) {
                "Numerik"
            } else if (is.character(x) || is.factor(x)) {
                "Kategorik"
            } else {
                "Lainnya"
            }
        }),
        Missing.Values = sapply(sovi_data, function(x) sum(is.na(x))),
        Persen.Missing = round(sapply(sovi_data, function(x) sum(is.na(x)) / length(x) * 100), 2),
        stringsAsFactors = FALSE
    )

    # Tambahkan deskripsi untuk variabel numerik
    metadata$Min <- ifelse(metadata$Tipe == "Numerik",
        round(sapply(sovi_data, function(x) if (is.numeric(x)) min(x, na.rm = TRUE) else NA), 4),
        NA
    )
    metadata$Max <- ifelse(metadata$Tipe == "Numerik",
        round(sapply(sovi_data, function(x) if (is.numeric(x)) max(x, na.rm = TRUE) else NA), 4),
        NA
    )
    metadata$Rerata <- ifelse(metadata$Tipe == "Numerik",
        round(sapply(sovi_data, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else NA), 4),
        NA
    )

    DT::datatable(metadata,
        options = list(
            pageLength = 20,
            scrollX = TRUE,
            dom = "Bfrtip",
            buttons = c("copy", "csv", "excel")
        ),
        caption = "Metadata Dataset SOVI - Social Vulnerability Index Indonesia",
        rownames = FALSE
    ) %>%
        DT::formatStyle(c("Persen.Missing"),
            backgroundColor = DT::styleInterval(c(5, 10), c("lightgreen", "yellow", "lightcoral"))
        )
}

# Fungsi untuk membuat tabel metadata distance
#' Membuat Tabel Metadata Distance
#'
#' @param distance_data Data frame berisi data distance
#' @return Objek DT::datatable berisi informasi matriks jarak
create_distance_metadata_table <- function(distance_data) {
    if (is.null(distance_data)) {
        return(NULL)
    }

    # Informasi dasar matriks jarak
    info_df <- data.frame(
        Atribut = c(
            "Dimensi Matriks", "Total Elemen", "Tipe Data",
            "Jarak Minimum", "Jarak Maksimum", "Jarak Rata-rata",
            "Jarak Median", "Missing Values", "Simetri Matriks"
        ),
        Nilai = c(
            paste(nrow(distance_data), "x", ncol(distance_data)),
            nrow(distance_data) * ncol(distance_data),
            "Numerik (km)",
            round(min(distance_data, na.rm = TRUE), 2),
            round(max(distance_data, na.rm = TRUE), 2),
            round(mean(as.matrix(distance_data), na.rm = TRUE), 2),
            round(median(as.matrix(distance_data), na.rm = TRUE), 2),
            sum(is.na(distance_data)),
            if (isSymmetric(as.matrix(distance_data))) "Ya" else "Tidak"
        ),
        stringsAsFactors = FALSE
    )

    DT::datatable(info_df,
        options = list(
            pageLength = 10,
            scrollX = TRUE,
            dom = "t",
            searching = FALSE,
            paging = FALSE,
            info = FALSE
        ),
        caption = "Informasi Matriks Jarak Antar Kabupaten/Kota Indonesia",
        rownames = FALSE
    )
}

# Fungsi untuk membuat konten laporan gabungan
#' Membuat Konten Laporan Gabungan
#'
#' @param sovi_data Data frame berisi data SOVI
#' @param report_type Jenis laporan ("info", "pdf", "word")
#' @return Vector karakter berisi baris-baris R Markdown
create_combined_report_content <- function(sovi_data, report_type = "info") {
    base_content <- c(
        "# ALIVA Dashboard - Laporan Komprehensif",
        "",
        "Dashboard ALIVA menyediakan analisis statistik komprehensif untuk data kerentanan sosial Indonesia.",
        "",
        "## Ringkasan Dataset"
    )

    if (!is.null(sovi_data)) {
        data_summary <- c(
            paste("- **Jumlah Observasi:** ", nrow(sovi_data), " kabupaten/kota"),
            paste("- **Jumlah Variabel:** ", ncol(sovi_data), " indikator"),
            paste("- **Variabel Numerik:** ", sum(sapply(sovi_data, is.numeric))),
            paste("- **Variabel Kategorik:** ", sum(sapply(sovi_data, function(x) is.character(x) || is.factor(x)))),
            paste("- **Sumber Data:** SUSENAS 2017, BPS-Statistics Indonesia"),
            ""
        )
        base_content <- c(base_content, data_summary)
    }

    features_content <- c(
        "## Fitur Dashboard Tersedia",
        "",
        "### 1. Manajemen Data",
        "- Transformasi variabel (log, sqrt, z-score)",
        "- Kategorisasi data numerik",
        "- Filter dan seleksi data",
        "",
        "### 2. Eksplorasi Data",
        "- Statistik deskriptif lengkap",
        "- Visualisasi univariat dan bivariat",
        "- Matriks korelasi interaktif",
        "- Analisis kelompok",
        "",
        "### 3. Uji Asumsi",
        "- Uji normalitas (Shapiro-Wilk, Anderson-Darling, Kolmogorov-Smirnov)",
        "- Uji homogenitas varians (Levene, Bartlett, Fligner-Killeen)",
        "- Uji independensi (Durbin-Watson)",
        "",
        "### 4. Statistik Inferensia",
        "- T-test (satu sampel, dua sampel independen, berpasangan)",
        "- ANOVA satu arah dan dua arah",
        "- Uji proporsi dan varians",
        "",
        "### 5. Regresi Linear Berganda",
        "- Model prediktif multivariat",
        "- Seleksi variabel otomatis",
        "- Diagnostik model komprehensif",
        ""
    )

    if (report_type == "info") {
        additional_content <- c(
            "## Data Information",
            if (!is.null(sovi_data)) {
                paste("Dataset berisi", nrow(sovi_data), "observasi dengan", ncol(sovi_data), "variabel.")
            } else {
                "Data sedang dimuat..."
            },
            "",
            "---",
            "Untuk laporan detail dari setiap modul, silakan gunakan fitur download di masing-masing tab."
        )
    } else {
        additional_content <- c(
            "## Metodologi Analisis",
            "Setiap analisis dalam dashboard mengikuti protokol statistik yang ketat:",
            "- Validasi asumsi sebelum melakukan uji statistik",
            "- Interpretasi hasil dengan confidence intervals",
            "- Effect size calculations untuk practical significance",
            "- Multiple comparison corrections untuk analisis simultan",
            "",
            "---",
            "*Laporan ini dihasilkan secara otomatis dari ALIVA Dashboard*"
        )
    }

    c(base_content, features_content, additional_content)
}

# Fungsi untuk membuat nama file download dengan prefix
#' Membuat Nama File Download Dashboard
#'
#' @param type Jenis file ("info", "pdf", "word")
#' @return String nama file dengan timestamp
create_dashboard_filename <- function(type) {
    prefix <- switch(type,
        "info" = "ALIVA_Dashboard_Info",
        "pdf" = "ALIVA_Complete_Dashboard_Report",
        "word" = "ALIVA_Complete_Dashboard_Report",
        "ALIVA_Dashboard_Report"
    )

    extension <- switch(type,
        "info" = ".txt",
        "pdf" = ".pdf",
        "word" = ".docx",
        ".pdf"
    )

    paste0(prefix, "_", Sys.Date(), extension)
}
