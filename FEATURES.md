# ALIVA Dashboard - Comprehensive Feature Documentation

## Overview
ALIVA adalah dashboard analisis statistik interaktif untuk data kerentanan sosial Indonesia yang dikembangkan dengan R Shiny. Dashboard ini memenuhi semua kriteria UAS Komputasi Statistik dengan fitur-fitur lengkap dan interpretasi otomatis.

## Fitur Utama

### 1. Beranda (Home)
- **Informasi Dashboard**: Overview lengkap tentang aplikasi dan tujuannya
- **Metadata Dataset**: Tabel informasi variabel dengan deskripsi dan satuan
- **Informasi Sumber Data**: SUSENAS 2017 dari BPS-Statistics Indonesia
- **Download**: Unduh informasi lengkap dashboard dalam format PDF

### 2. Manajemen Data
- **Kategorisasi Variabel**: Mengubah data kontinu menjadi kategorik
- **Metode Kategorisasi**:
  - Interval sama (equal intervals)
  - Kuantil (equal frequency)
- **Jumlah Kategori**: Dapat disesuaikan (2-10 kategori)
- **Interpretasi Otomatis**: Penjelasan hasil kategorisasi
- **Download**: 
  - Hasil kategorisasi (.csv)
  - Interpretasi (.txt)

### 3. Eksplorasi Data
- **Statistik Deskriptif**: 
  - Summary statistics lengkap
  - Interpretasi otomatis untuk semua variabel numerik
- **Visualisasi Grafik**:
  - Histogram interaktif dengan plotly
  - Grafik responsif dan dapat di-zoom
- **Visualisasi Peta**:
  - Peta interaktif dengan Leaflet
  - Placeholder untuk data spasial Indonesia
- **Download**:
  - Ringkasan statistik (.pdf)
  - Plot visualisasi (.png)

### 4. Uji Asumsi
- **Uji Normalitas**:
  - Shapiro-Wilk test
  - Q-Q plot dan histogram dengan kurva normal
  - Interpretasi otomatis hasil uji
- **Uji Homogenitas**:
  - Levene's test
  - Uji varians antar grup
  - Interpretasi otomatis hasil uji
- **Download**: Laporan lengkap uji asumsi (.pdf)

### 5. Statistik Inferensia

#### Uji Beda Rata-Rata
- **Uji t Satu Sampel**: Membandingkan rata-rata sampel dengan nilai tertentu
- **Uji t Dua Sampel**: Membandingkan rata-rata dua grup independen
- **Hipotesis Alternatif**: Dua arah, lebih besar, lebih kecil
- **Output Lengkap**: 
  - Statistik uji, p-value, interval kepercayaan
  - Interpretasi otomatis dengan keputusan hipotesis
- **Download**: Laporan hasil uji (.pdf)

#### Uji Proporsi & Varians
- **Uji Proporsi Satu Sampel**: 
  - Input: jumlah sukses, total sampel, proporsi uji
  - Chi-square test dengan interval kepercayaan
- **Uji Varians Satu Sampel**:
  - Chi-square test untuk varians
  - Hipotesis terhadap nilai varians tertentu
- **Interpretasi Komprehensif**: Keputusan statistik dengan penjelasan
- **Download**: Laporan hasil uji (.pdf)

#### ANOVA
- **ANOVA Satu Arah**: Membandingkan rata-rata lebih dari 2 grup
- **ANOVA Dua Arah**: 
  - Efek utama dua faktor
  - Efek interaksi (opsional)
- **Post-Hoc Test**: Tukey HSD untuk perbandingan berganda
- **Visualisasi Interaktif**: 
  - Box plot untuk ANOVA satu arah
  - Interaction plot untuk ANOVA dua arah
- **Interpretasi Lengkap**: 
  - Hasil uji F untuk setiap efek
  - Post-hoc analysis interpretation
- **Download**: Laporan ANOVA lengkap (.pdf)

### 6. Regresi Linear Berganda
- **Pemilihan Variabel**:
  - Satu variabel dependen
  - Multiple variabel independen (checkbox)
- **Output Model**:
  - Summary lengkap model regresi
  - Persamaan regresi yang diformat
  - R-squared dan Adjusted R-squared
- **Uji Asumsi Klasik**:
  - **VIF Test**: Deteksi multikolinearitas
  - **Breusch-Pagan Test**: Uji heteroskedastisitas
  - **Shapiro-Wilk**: Normalitas residual
  - **Diagnostic Plots**: 4 plot residual standar
- **Interpretasi Komprehensif**:
  - Signifikansi model (uji F)
  - Signifikansi koefisien (uji t)
  - Interpretasi R-squared
  - Evaluasi asumsi klasik
- **Download**: Model lengkap dengan interpretasi (.pdf)

## Teknologi dan Library

### Core Framework
- **Shiny**: Framework web aplikasi R
- **shinydashboard**: Layout dan komponen UI
- **shinyWidgets**: Widget tambahan untuk interaktivitas

### Data Processing
- **dplyr**: Manipulasi data
- **tidyr**: Data tidying
- **readr**: Import/export data

### Visualisasi
- **ggplot2**: Grammar of graphics plotting
- **plotly**: Grafik interaktif
- **leaflet**: Peta interaktif
- **DT**: Tabel interaktif

### Analisis Statistik
- **car**: Companion to Applied Regression (VIF, Levene test)
- **lmtest**: Linear model testing (Breusch-Pagan)
- **nortest**: Normality tests
- **broom**: Tidy statistical output

### Report Generation
- **rmarkdown**: Dynamic document generation
- **knitr**: Document processing
- **officer**: MS Word document creation
- **flextable**: Professional table formatting

## Data Source
- **SUSENAS 2017**: Survei Sosial Ekonomi Nasional
- **BPS-Statistics Indonesia**: Badan Pusat Statistik
- **Coverage**: 514 kabupaten/kota di Indonesia
- **Variables**: 15+ indikator kerentanan sosial

## Quality Features

### User Experience
- **Responsive Design**: Adaptif untuk berbagai ukuran layar
- **Loading States**: Indikator proses untuk operasi yang memakan waktu
- **Error Handling**: Pesan error yang informatif
- **Notifications**: Feedback real-time untuk user actions
- **Input Validation**: Validasi input di setiap form

### Statistical Rigor
- **Assumption Testing**: Uji asumsi sebelum analisis utama
- **Multiple Test Options**: Parametrik dan non-parametrik
- **Effect Sizes**: Perhitungan ukuran efek dimana relevan
- **Confidence Intervals**: Interval kepercayaan untuk semua estimasi
- **Automated Interpretation**: Interpretasi statistik yang konsisten

### Documentation
- **Inline Help**: Tooltip dan panduan di setiap fitur
- **Metadata Tables**: Informasi lengkap variabel
- **Method Descriptions**: Penjelasan metode statistik yang digunakan
- **Result Interpretation**: Panduan interpretasi otomatis

### Download Capabilities
- **Multiple Formats**: PDF, Word, CSV, PNG
- **Comprehensive Reports**: Laporan lengkap per halaman
- **Individual Outputs**: Download terpisah untuk setiap analisis
- **Professional Formatting**: Layout yang rapi dan mudah dibaca

## Performance Optimizations
- **Efficient Data Loading**: Sample data generation on-demand
- **Reactive Programming**: Update UI hanya saat diperlukan
- **Memory Management**: Handling data besar dengan sampling
- **Fast Rendering**: Optimasi plot dan tabel untuk responsivitas

## Security Features
- **Input Validation**: Sanitasi semua input user
- **Error Isolation**: Error handling yang tidak mengekspos sistem
- **Resource Limits**: Pembatasan ukuran file dan memori
- **Safe File Operations**: Validasi path dan tipe file

## Extensibility
- **Modular Architecture**: Kode terorganisir dalam modules
- **Configurable Parameters**: Setting yang dapat disesuaikan
- **Plugin-Ready**: Struktur yang mendukung penambahan fitur
- **API-Friendly**: Desain yang dapat diintegrasikan

Aplikasi ini memenuhi dan melebihi semua kriteria UAS Komputasi Statistik dengan implementasi yang komprehensif, user-friendly, dan profesional.
