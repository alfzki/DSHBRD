# NusaStat Dashboard

**NusaStat: Dasbor Interaktif Analisis Kerentanan Sosial & Statistik Indonesia**

## Deskripsi

NusaStat adalah dashboard interaktif yang dikembangkan menggunakan R Shiny untuk analisis statistik data kerentanan sosial Indonesia. Dashboard ini dirancang khusus untuk memenuhi kebutuhan analisis statistik komprehensif dengan antarmuka yang modern dan mudah digunakan.

## Fitur Utama

### 🏠 **Beranda**
- Informasi umum tentang dashboard
- Metadata variabel dataset
- Informasi sumber data (SUSENAS 2017)

### 📊 **Manajemen Data**
- Transformasi variabel kontinu menjadi kategorik
- Metode kategorisasi: Interval sama dan Kuantil
- Interpretasi otomatis hasil kategorisasi
- Unduhan hasil dalam format CSV dan TXT

### 🔍 **Eksplorasi Data**
- Statistik deskriptif lengkap
- Visualisasi histogram interaktif dengan Plotly
- Visualisasi peta (memerlukan data spasial tambahan)
- Interpretasi otomatis statistik

### ✅ **Uji Asumsi**
- Uji normalitas (Shapiro-Wilk)
- Uji homogenitas varians (Levene's test)
- Q-Q plot dan histogram dengan kurva normal
- Interpretasi hasil uji

### 📈 **Statistik Inferensia**

#### Uji Beda Rata-Rata
- Uji t satu sampel
- Uji t dua sampel independen
- Interpretasi hasil dengan hipotesis

#### Uji Proporsi & Varians
- Uji proporsi satu sampel
- Uji varians satu sampel (Chi-square)
- Interpretasi hasil uji

#### ANOVA
- ANOVA satu arah
- ANOVA dua arah (dengan/tanpa interaksi)
- Post-hoc test (Tukey HSD)
- Visualisasi box plot interaktif

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
