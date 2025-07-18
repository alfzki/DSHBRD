# 🇮🇩 ALIVA Dashboard - Analisis Kerentanan Sosial Indonesia

[![R Shiny](https://img.shields.io/badge/R-Shiny-blue?style=for-the-badge&logo=r)](https://shiny.rstudio.com/)
[![Status](https://img.shields.io/badge/Status-Production%20Ready-brightgreen?style=for-the-badge)]()
[![License](https://img.shields.io/badge/License-Academic%20Use-orange?style=for-the-badge)]()

Dashboard interaktif untuk analisis statistik data kerentanan sosial Indonesia yang dikembangkan untuk memenuhi kriteria **Ujian Akhir Semester (UAS) Komputasi Statistik**.

## 🚀 Quick Start

### Menjalankan Aplikasi
```r
# 1. Clone repository atau download project
# 2. Buka R/RStudio dan set working directory ke folder Dashboard
setwd("path/to/Dashboard")

# 3. Install packages dan jalankan aplikasi
source("global.R")  # Install dependencies
source("app.R")     # Launch dashboard
```

**Akses aplikasi di**: `http://127.0.0.1:3838`

## 📊 Fitur Utama

### 🏠 Dashboard Lengkap
- **Beranda**: Overview dan metadata dataset
- **Manajemen Data**: Kategorisasi variabel kontinu
- **Eksplorasi Data**: Statistik deskriptif dan visualisasi
- **Uji Asumsi**: Normalitas dan homogenitas
- **Statistik Inferensia**: t-test, ANOVA, uji proporsi/varians
- **Regresi Linear Berganda**: Model prediktif dengan uji asumsi

### 📈 Analisis Statistik Komprehensif
- ✅ **Uji t**: Satu sampel dan dua sampel independen
- ✅ **ANOVA**: Satu arah dan dua arah dengan post-hoc test
- ✅ **Uji Proporsi**: Chi-square test untuk proporsi
- ✅ **Uji Varians**: Chi-square test untuk varians
- ✅ **Regresi**: Multiple linear regression dengan diagnostik lengkap
- ✅ **Uji Asumsi**: Normalitas, homogenitas, multikolinearitas

### 🎨 Visualisasi Interaktif
- 📊 Histogram dan boxplot dengan **Plotly**
- 🗺️ Peta interaktif dengan **Leaflet**
- 📋 Tabel dinamis dengan **DT**
- 📈 Plot diagnostik regresi

### 📄 Export & Reporting
- 📑 **PDF Reports**: Laporan lengkap per analisis
- 📊 **CSV Data**: Export data dan hasil
- 🖼️ **PNG Plots**: Grafik high-resolution
- 📝 **Word Documents**: Laporan profesional

### 📊 **Regresi Linear Berganda**
- Model regresi dengan multiple variabel independen
- Uji asumsi regresi:
  - Multikolinearitas (VIF)
  - Heteroskedastisitas (Breusch-Pagan)
  - Normalitas residual (Shapiro-Wilk)
- Interpretasi komprehensif model
- Persamaan regresi otomatis

## Struktur Proyek

```
Dashboard/
├── app.R                    # File utama aplikasi Shiny
├── global.R                 # Konfigurasi global dan fungsi utilitas
├── data/                    # Direktori data
│   ├── sovi_data.csv       # Data kerentanan sosial (sample)
│   └── distance.csv        # Data jarak (sample)
├── R/                      # Modul R
│   ├── ui_modules.R        # Modul UI
│   ├── server_modules.R    # Modul server utama
│   └── additional_server_modules.R  # Modul server tambahan
├── www/                    # Aset web
│   └── custom.css          # Styling kustom
├── reports/                # Direktori laporan
└── README.md              # Dokumentasi ini
```

## Instalasi dan Penggunaan

### Persyaratan Sistem
- R version 4.0.0 atau lebih baru
- RStudio (disarankan)

### Instalasi Paket

Dashboard ini akan secara otomatis menginstal paket yang diperlukan saat pertama kali dijalankan. Paket yang diperlukan meliputi:

```r
# Paket utama Shiny
shiny, shinydashboard, shinyWidgets, shinythemes, shinyjs

# Manipulasi data
dplyr, tidyr, readr, stringr, lubridate

# Visualisasi
ggplot2, plotly, leaflet, DT, htmlwidgets

# Analisis statistik
car, lmtest, nortest, broom, psych

# Pembuatan laporan
rmarkdown, knitr, pagedown, officer, flextable

# Utilitas tambahan
here, glue, scales, RColorBrewer, viridis
```

### Menjalankan Dashboard

1. **Clone atau unduh repositori ini**
2. **Buka RStudio dan navigasi ke direktori proyek**
3. **Jalankan aplikasi:**
   ```r
   # Metode 1: Langsung dari app.R
   shiny::runApp("app.R")
   
   # Metode 2: Dari direktori proyek
   shiny::runApp()
   ```

### Data Sample

Jika file data asli (`sovi_data.csv` dan `distance.csv`) tidak tersedia, dashboard akan otomatis membuat data sample dengan karakteristik yang realistis untuk keperluan demonstrasi.

## Fitur Unduhan

Setiap analisis dalam dashboard dapat diunduh dalam berbagai format:
- **PDF**: Laporan lengkap dengan interpretasi
- **CSV**: Data hasil analisis
- **TXT**: Interpretasi dalam format teks
- **PNG**: Visualisasi grafik

## Teknologi yang Digunakan

- **R Shiny**: Framework web untuk R
- **shinydashboard**: UI framework untuk dashboard
- **ggplot2 & plotly**: Visualisasi data interaktif
- **DT**: Tabel data interaktif
- **leaflet**: Peta interaktif
- **rmarkdown**: Pembuatan laporan dinamis

## Struktur Kode

### Modularitas
Dashboard ini menggunakan pendekatan modular dengan:
- **UI Modules**: Komponen antarmuka yang dapat digunakan kembali
- **Server Modules**: Logika server yang terorganisir
- **Global Functions**: Fungsi utilitas yang dapat diakses di seluruh aplikasi

### Validasi Data
Setiap modul dilengkapi dengan validasi data untuk memastikan:
- Data telah dimuat dengan benar
- Input pengguna valid
- Persyaratan statistik terpenuhi

### Error Handling
Implementasi try-catch yang komprehensif untuk menangani:
- Error loading data
- Error dalam perhitungan statistik
- Error dalam generasi laporan

## Pengembangan Lebih Lanjut

### Fitur yang Dapat Ditambahkan
1. **Analisis Time Series**
2. **Machine Learning Models**
3. **Analisis Cluster**
4. **Export to Excel dengan formatting**
5. **Integrasi dengan database**
6. **Visualisasi peta dengan data spasial real**

### Kontribusi
Untuk berkontribusi pada proyek ini:
1. Fork repositori
2. Buat branch fitur baru
3. Commit perubahan
4. Push ke branch
5. Buat Pull Request

## Lisensi

Proyek ini dikembangkan untuk keperluan akademis (UAS Komputasi Statistik).

## Kontak

Dashboard ini dikembangkan oleh AI Agent Pemrograman R untuk UAS Komputasi Statistik.

---

**Catatan**: Dashboard ini dikembangkan untuk keperluan pendidikan dan penelitian. Pastikan untuk memverifikasi hasil analisis statistik dengan sumber terpercaya sebelum digunakan untuk pengambilan keputusan penting.

---

*Terakhir diperbarui: Juli 2025*
