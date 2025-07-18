# Beranda Server Module
# Server logic for the home page of ALIVA Dashboard

#' Beranda Server Module
#'
#' Server logic for the home/landing page functionality
#'
#' @param id Module ID for namespacing
#' @param values Reactive values object containing shared data
beranda_server <- function(id, values) {
    moduleServer(id, function(input, output, session) {
        # Welcome content
        output$welcome_content <- renderUI({
            tagList(
                h3("ALIVA: Alif Vulnerability Analytics Dashboard"),
                p("Selamat datang di ALIVA Dashboard! Aplikasi ini dirancang untuk membantu analisis statistik
          data kerentanan sosial Indonesia dengan fitur-fitur yang komprehensif dan mudah digunakan."),
                hr(),
                h4("Fitur Utama:"),
                tags$ul(
                    tags$li("Manajemen Data: Transformasi variabel kontinu menjadi kategorik"),
                    tags$li("Eksplorasi Data: Statistik deskriptif dan visualisasi interaktif"),
                    tags$li("Uji Asumsi: Uji normalitas dan homogenitas varians"),
                    tags$li("Statistik Inferensia: Uji beda rata-rata, proporsi, varians, dan ANOVA"),
                    tags$li("Regresi Linear Berganda: Model prediktif dengan uji asumsi lengkap"),
                    tags$li("Unduhan: Semua hasil dapat diunduh dalam format PDF, Word, atau CSV")
                ),
                hr(),
                h4("Sumber Data:"),
                p("Data yang digunakan dalam dashboard ini berasal dari SUSENAS (Survei Sosial Ekonomi Nasional)
          2017 yang diterbitkan oleh BPS-Statistics Indonesia. Data mencakup indikator kerentanan sosial
          dari 511 kabupaten/kota di Indonesia dengan 17 variabel meliputi:"),
                tags$ul(
                    tags$li("CHILDREN: Persentase populasi berusia di bawah lima tahun"),
                    tags$li("FEMALE: Persentase populasi perempuan"),
                    tags$li("ELDERLY: Persentase populasi berusia 65 tahun ke atas"),
                    tags$li("POVERTY: Persentase penduduk miskin"),
                    tags$li("ILLITERATE: Persentase populasi yang buta huruf"),
                    tags$li("NOELECTRIC: Persentase rumah tangga tanpa akses listrik"),
                    tags$li("TAPWATER: Persentase rumah tangga yang menggunakan air ledeng/pipa"),
                    tags$li("Dan 10 indikator kerentanan sosial lainnya")
                )
            )
        })

        # Dataset information
        output$dataset_info <- renderUI({
            if (is.null(values$sovi_data)) {
                return(p("Data sedang dimuat..."))
            }

            tagList(
                h5("Informasi Dataset SOVI:"),
                p(paste("Jumlah observasi:", nrow(values$sovi_data))),
                p(paste("Jumlah variabel:", ncol(values$sovi_data))),
                p(paste("Variabel numerik:", length(get_numeric_columns(values$sovi_data)))),
                p(paste("Variabel kategorik:", length(get_categorical_columns(values$sovi_data)))),
                hr(),
                h5("Informasi Dataset Distance:"),
                p(paste("Jumlah observasi:", nrow(values$distance_data))),
                p(paste("Jumlah variabel:", ncol(values$distance_data)))
            )
        })

        # Metadata table
        output$metadata_table <- DT::renderDT({
            metadata <- data.frame(
                "Nama Variabel di Dashboard" = c(
                    "Region", "Pulau", "Provinsi", "Nama Kabupaten/Kota",
                    "Persentase Populasi Balita", "Persentase Populasi Wanita", "Persentase Populasi Lansia",
                    "Persentase Kepala Keluarga Wanita", "Rata-rata Anggota Keluarga", "Persentase Tanpa Listrik",
                    "Persentase Pendidikan Rendah", "Pertumbuhan Populasi", "Tingkat Kemiskinan",
                    "Tingkat Buta Huruf", "Tanpa Pelatihan Bencana", "Tinggal di Area Rawan Bencana",
                    "Status Kepemilikan Rumah (Sewa)", "Tanpa Saluran Pembuangan", "Akses Air Pipa",
                    "Jumlah Populasi"
                ),
                "Nama Asli" = c(
                    "region", "island", "province", "district",
                    "CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", "NOELECTRIC",
                    "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING", "DPRONE",
                    "RENTED", "NOSEWER", "TAPWATER", "POPULATION"
                ),
                "Deskripsi" = c(
                    "Wilayah geografis besar (Barat/Timur)", "Nama pulau utama", "Nama provinsi", "Nama kabupaten/kota",
                    "Persentase populasi berusia di bawah lima tahun", "Persentase populasi perempuan", "Persentase populasi berusia 65 tahun ke atas",
                    "Persentase rumah tangga dengan kepala keluarga perempuan", "Rata-rata jumlah anggota rumah tangga", "Persentase rumah tangga tanpa akses listrik sebagai sumber penerangan",
                    "Persentase populasi 15 tahun ke atas dengan pendidikan rendah", "Persentase perubahan (pertumbuhan) populasi", "Persentase penduduk miskin",
                    "Persentase populasi yang buta huruf", "Persentase rumah tangga yang tidak pernah mendapat pelatihan bencana", "Persentase rumah tangga yang tinggal di area rawan bencana",
                    "Persentase rumah tangga yang menyewa rumah", "Persentase rumah tangga tanpa sistem drainase/saluran pembuangan", "Persentase rumah tangga yang menggunakan air ledeng/pipa",
                    "Jumlah total populasi"
                ),
                "Satuan" = c(
                    "-", "-", "-", "-",
                    "%", "%", "%", "%", "Orang", "%",
                    "%", "%", "%", "%", "%", "%",
                    "%", "%", "%", "Jiwa"
                ),
                "Sumber" = rep("SUSENAS 2017, BPS-Statistics Indonesia", 20),
                check.names = FALSE
            )

            DT::datatable(metadata,
                options = list(pageLength = 10, scrollX = TRUE),
                class = "table-striped table-hover"
            )
        })

        # Download handler for dashboard info
        output$download_info <- downloadHandler(
            filename = "ALIVA_Info.pdf",
            content = function(file) {
                # Create a temporary R Markdown file
                temp_rmd <- tempfile(fileext = ".Rmd")

                rmd_content <- '---
title: "ALIVA Dashboard - Informasi Lengkap"
output: pdf_document
date: "`r Sys.Date()`"
---

# Tentang ALIVA Dashboard

ALIVA adalah dashboard interaktif untuk analisis kerentanan sosial dan statistik Indonesia yang dikembangkan menggunakan R Shiny.

## Fitur Utama

1. **Manajemen Data**: Transformasi variabel kontinu menjadi kategorik
2. **Eksplorasi Data**: Statistik deskriptif dan visualisasi
3. **Uji Asumsi**: Uji normalitas dan homogenitas
4. **Statistik Inferensia**: Berbagai uji statistik
5. **Regresi Linear Berganda**: Model prediktif lengkap

## Sumber Data

Data berasal dari SUSENAS 2017, BPS-Statistics Indonesia, mencakup 511 kabupaten/kota dengan 17 variabel kerentanan sosial.

## Metadata Variabel

```{r echo=FALSE}
metadata <- data.frame(
  Variabel = c("CHILDREN", "FEMALE", "ELDERLY", "POVERTY", "ILLITERATE", "NOELECTRIC"),
  Deskripsi = c("Persentase Populasi Balita", "Persentase Populasi Perempuan",
                "Persentase Populasi Lansia ≥65 tahun", "Persentase Penduduk Miskin",
                "Persentase Populasi Buta Huruf", "Persentase Rumah Tangga Tanpa Listrik"),
  Satuan = c("%", "%", "%", "%", "%", "%")
)
knitr::kable(metadata)
```
'

                writeLines(rmd_content, temp_rmd)
                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )

        # Combined download handler for all menus (PDF)
        output$download_combined_pdf <- downloadHandler(
            filename = function() {
                paste0("ALIVA_Laporan_Gabungan_", Sys.Date(), ".pdf")
            },
            content = function(file) {
                # Create comprehensive combined report
                temp_rmd <- tempfile(fileext = ".Rmd")

                rmd_content <- '---
title: "ALIVA Dashboard - Laporan Gabungan Semua Menu"
subtitle: "Analisis Kerentanan Sosial Indonesia"
author: "ALIVA: Alif\'s Vulnerability Analytics Dashboard"
date: "`r format(Sys.Date(), \'%d %B %Y\')`"
output:
  pdf_document:
    latex_engine: xelatex
    keep_tex: false
    toc: true
    toc_depth: 3
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
```

# Executive Summary

ALIVA Dashboard merupakan aplikasi analisis statistik komprehensif untuk data kerentanan sosial Indonesia. Dashboard ini menyediakan berbagai fitur analisis mulai dari eksplorasi data dasar hingga analisis regresi lanjutan.

## Dataset Overview

Dataset yang digunakan berasal dari SUSENAS 2017 dengan 511 observasi kabupaten/kota di Indonesia dan 17 variabel indikator kerentanan sosial.

# 1. Beranda - Informasi Dashboard

## Tentang ALIVA
ALIVA: Alif Vulnerability Analytics Dashboard adalah platform analisis statistik yang dirancang khusus untuk menganalisis data kerentanan sosial Indonesia.

## Fitur Utama
- Manajemen Data: Transformasi variabel kontinu ke kategorik
- Eksplorasi Data: Statistik deskriptif dan visualisasi
- Uji Asumsi: Normalitas dan homogenitas varians
- Statistik Inferensia: Uji t, ANOVA, uji proporsi dan varians
- Regresi Linear Berganda: Model prediktif dengan uji asumsi

# 2. Manajemen Data

Fitur manajemen data memungkinkan transformasi variabel kontinu menjadi kategorik menggunakan dua metode:
- **Interval Sama**: Pembagian berdasarkan rentang nilai yang sama
- **Kuantil**: Pembagian berdasarkan persentil untuk jumlah observasi yang sama

**Interpretasi**: Kategorisasi membantu dalam analisis eksploratori dan memudahkan interpretasi pola data untuk analisis statistik lanjutan.

# 3. Eksplorasi Data

## Statistik Deskriptif
Analisis deskriptif mencakup:
- Measures of central tendency (mean, median, mode)
- Measures of variability (std dev, variance, range)
- Measures of shape (skewness, kurtosis)

## Visualisasi
- **Histogram**: Menampilkan distribusi frekuensi variabel
- **Peta Interaktif**: Visualisasi spasial distribusi variabel per kabupaten/kota

**Interpretasi**: Eksplorasi data memberikan pemahaman awal tentang karakteristik dataset sebelum melakukan analisis inferensia.

# 4. Uji Asumsi

## Uji Normalitas (Shapiro-Wilk)
Menguji apakah data berdistribusi normal - prerequisite untuk uji parametrik.

**Hipotesis**:
- H₀: Data berdistribusi normal
- H₁: Data tidak berdistribusi normal

## Uji Homogenitas (Levene Test)
Menguji kesamaan varians antar grup - asumsi penting untuk ANOVA.

**Hipotesis**:
- H₀: Varians antar grup homogen
- H₁: Varians antar grup tidak homogen

**Interpretasi**: Pengujian asumsi memastikan validitas analisis statistik selanjutnya.

# 5. Statistik Inferensia

## 5.1 Uji Beda Rata-Rata
### Uji t Satu Sampel
Membandingkan rata-rata sampel dengan nilai hipotesis tertentu.

### Uji t Dua Sampel
Membandingkan rata-rata dua kelompok independen.

**Interpretasi**: Uji t membantu menentukan apakah perbedaan yang diamati signifikan secara statistik.

## 5.2 Uji Proporsi dan Varians
### Uji Proporsi Satu Sampel
Menguji apakah proporsi sampel berbeda dari nilai hipotesis.

### Uji Varians Satu Sampel
Menguji apakah varians sampel berbeda dari nilai hipotesis.

**Interpretasi**: Uji ini penting untuk validasi asumsi dan analisis karakteristik populasi.

## 5.3 ANOVA (Analysis of Variance)
### One-Way ANOVA
Membandingkan rata-rata lebih dari dua kelompok independen.

### Two-Way ANOVA
Menganalisis pengaruh dua faktor sekaligus, termasuk efek interaksi.

**Interpretasi**: ANOVA memungkinkan analisis pengaruh multiple factors terhadap variabel outcome.

# 6. Regresi Linear Berganda

## Model Regresi
Analisis hubungan antara satu variabel dependen dengan multiple variabel independen.

## Uji Asumsi Klasik
- **Multikolinearitas (VIF)**: Mengecek korelasi tinggi antar prediktor
- **Heteroskedastisitas (Breusch-Pagan)**: Mengecek homogenitas varians residual
- **Normalitas Residual (Shapiro-Wilk)**: Mengecek distribusi normal residual

## Interpretasi Model
- **R-squared**: Proporsi varians yang dijelaskan model
- **Adjusted R-squared**: R-squared yang disesuaikan dengan jumlah prediktor
- **F-test**: Signifikansi model secara keseluruhan
- **t-test**: Signifikansi masing-masing koefisien

**Interpretasi**: Regresi berganda memberikan model prediktif dan understanding tentang faktor-faktor yang mempengaruhi variabel outcome.

# Kesimpulan

ALIVA Dashboard menyediakan workflow analisis statistik yang komprehensif dari eksplorasi data hingga modeling prediktif. Setiap fitur dilengkapi dengan interpretasi yang memudahkan pengguna memahami hasil analisis.

## Rekomendasi Penggunaan
1. Mulai dengan eksplorasi data untuk memahami karakteristik dataset
2. Lakukan uji asumsi sebelum analisis inferensia
3. Pilih uji statistik yang sesuai berdasarkan jenis data dan tujuan analisis
4. Validasi model regresi dengan pengujian asumsi klasik
5. Interpretasikan hasil dalam konteks domain knowledge

---

*Laporan ini dihasilkan secara otomatis oleh ALIVA Dashboard pada `r Sys.Date()`*
'

                writeLines(rmd_content, temp_rmd)
                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )

        # Combined download handler for all menus (Word)
        output$download_combined_word <- downloadHandler(
            filename = function() {
                paste0("ALIVA_Laporan_Gabungan_", Sys.Date(), ".docx")
            },
            content = function(file) {
                # Create Word version of combined report
                temp_rmd <- tempfile(fileext = ".Rmd")

                rmd_content <- '---
title: "ALIVA Dashboard - Laporan Gabungan Semua Menu"
subtitle: "Analisis Kerentanan Sosial Indonesia"
author: "ALIVA: Alif Vulnerability Analytics Dashboard"
date: "`r format(Sys.Date(), \"%d %B %Y\")`"
output:
  word_document:
    reference_docx: NULL
    toc: true
    toc_depth: 3
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
```

# Executive Summary

ALIVA Dashboard merupakan aplikasi analisis statistik komprehensif untuk data kerentanan sosial Indonesia. Dashboard ini menyediakan berbagai fitur analisis mulai dari eksplorasi data dasar hingga analisis regresi lanjutan.

## Dataset Overview

Dataset yang digunakan berasal dari SUSENAS 2017 dengan 511 observasi kabupaten/kota di Indonesia dan 17 variabel indikator kerentanan sosial.

# 1. Beranda - Informasi Dashboard

## Tentang ALIVA
ALIVA: Alif Vulnerability Analytics Dashboard adalah platform analisis statistik yang dirancang khusus untuk menganalisis data kerentanan sosial Indonesia.

## Fitur Utama
- Manajemen Data: Transformasi variabel kontinu ke kategorik
- Eksplorasi Data: Statistik deskriptif dan visualisasi
- Uji Asumsi: Normalitas dan homogenitas varians
- Statistik Inferensia: Uji t, ANOVA, uji proporsi dan varians
- Regresi Linear Berganda: Model prediktif dengan uji asumsi

# 2. Manajemen Data

Fitur manajemen data memungkinkan transformasi variabel kontinu menjadi kategorik menggunakan dua metode:

**Metode Interval Sama**: Pembagian berdasarkan rentang nilai yang sama
**Metode Kuantil**: Pembagian berdasarkan persentil untuk jumlah observasi yang sama

**Interpretasi**: Kategorisasi membantu dalam analisis eksploratori dan memudahkan interpretasi pola data untuk analisis statistik lanjutan.

# 3. Eksplorasi Data

## Statistik Deskriptif
Analisis deskriptif mencakup:
- Measures of central tendency (mean, median, mode)
- Measures of variability (std dev, variance, range)
- Measures of shape (skewness, kurtosis)

## Visualisasi
- **Histogram**: Menampilkan distribusi frekuensi variabel
- **Peta Interaktif**: Visualisasi spasial distribusi variabel per kabupaten/kota

**Interpretasi**: Eksplorasi data memberikan pemahaman awal tentang karakteristik dataset sebelum melakukan analisis inferensia.

# 4. Uji Asumsi

## Uji Normalitas (Shapiro-Wilk)
Menguji apakah data berdistribusi normal - prerequisite untuk uji parametrik.

**Hipotesis**:
- H₀: Data berdistribusi normal
- H₁: Data tidak berdistribusi normal

## Uji Homogenitas (Levene Test)
Menguji kesamaan varians antar grup - asumsi penting untuk ANOVA.

**Hipotesis**:
- H₀: Varians antar grup homogen
- H₁: Varians antar grup tidak homogen

**Interpretasi**: Pengujian asumsi memastikan validitas analisis statistik selanjutnya.

# 5. Statistik Inferensia

## 5.1 Uji Beda Rata-Rata

### Uji t Satu Sampel
Membandingkan rata-rata sampel dengan nilai hipotesis tertentu.

### Uji t Dua Sampel
Membandingkan rata-rata dua kelompok independen.

**Interpretasi**: Uji t membantu menentukan apakah perbedaan yang diamati signifikan secara statistik.

## 5.2 Uji Proporsi dan Varians

### Uji Proporsi Satu Sampel
Menguji apakah proporsi sampel berbeda dari nilai hipotesis.

### Uji Varians Satu Sampel
Menguji apakah varians sampel berbeda dari nilai hipotesis.

**Interpretasi**: Uji ini penting untuk validasi asumsi dan analisis karakteristik populasi.

## 5.3 ANOVA (Analysis of Variance)

### One-Way ANOVA
Membandingkan rata-rata lebih dari dua kelompok independen.

### Two-Way ANOVA
Menganalisis pengaruh dua faktor sekaligus, termasuk efek interaksi.

**Interpretasi**: ANOVA memungkinkan analisis pengaruh multiple factors terhadap variabel outcome.

# 6. Regresi Linear Berganda

## Model Regresi
Analisis hubungan antara satu variabel dependen dengan multiple variabel independen.

## Uji Asumsi Klasik
- **Multikolinearitas (VIF)**: Mengecek korelasi tinggi antar prediktor
- **Heteroskedastisitas (Breusch-Pagan)**: Mengecek homogenitas varians residual
- **Normalitas Residual (Shapiro-Wilk)**: Mengecek distribusi normal residual

## Interpretasi Model
- **R-squared**: Proporsi varians yang dijelaskan model
- **Adjusted R-squared**: R-squared yang disesuaikan dengan jumlah prediktor
- **F-test**: Signifikansi model secara keseluruhan
- **t-test**: Signifikansi masing-masing koefisien

**Interpretasi**: Regresi berganda memberikan model prediktif dan understanding tentang faktor-faktor yang mempengaruhi variabel outcome.

# Kesimpulan

ALIVA Dashboard menyediakan workflow analisis statistik yang komprehensif dari eksplorasi data hingga modeling prediktif. Setiap fitur dilengkapi dengan interpretasi yang memudahkan pengguna memahami hasil analisis.

## Rekomendasi Penggunaan
1. Mulai dengan eksplorasi data untuk memahami karakteristik dataset
2. Lakukan uji asumsi sebelum analisis inferensia
3. Pilih uji statistik yang sesuai berdasarkan jenis data dan tujuan analisis
4. Validasi model regresi dengan pengujian asumsi klasik
5. Interpretasikan hasil dalam konteks domain knowledge

---

*Laporan ini dihasilkan secara otomatis oleh ALIVA Dashboard pada `r Sys.Date()`*
'

                writeLines(rmd_content, temp_rmd)
                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )
    })
}
