# ALIVA Dashboard

## Deskripsi
ALIVA Dashboard adalah aplikasi dashboard interaktif berbasis R yang dirancang untuk analisis kerentanan sosial dan statistik Indonesia. Aplikasi ini memanfaatkan data SUSENAS 2017 dan menyediakan fitur analisis deskriptif, inferensia, modeling prediktif, serta visualisasi data spasial secara komprehensif. Proyek ini menggunakan arsitektur modular untuk memudahkan pengembangan dan pemeliharaan.

## Struktur Folder
```
├── app.R
├── global.R
├── launch_dashboard.R
├── README.md
├── data/
│   ├── distance.csv
│   ├── indonesia_kabkota.geojson
│   ├── sovi_data.csv
│   └── metadata.md
├── R/
│   ├── load_modules.R
│   └── modules/
│       ├── beranda/
│       ├── manajemen_data/
│       ├── eksplorasi/
│       ├── regresi/
│       ├── uji_anova/
│       ├── uji_asumsi/
│       ├── uji_prop_var/
│       ├── uji_rata/
│       └── utils/
├── reports/
│   ├── laporan_anova.Rmd
│   ├── laporan_eksplorasi.Rmd
│   ├── laporan_regresi.Rmd
│   └── laporan_uji_rata.Rmd
├── www/
│   └── custom.css
```

## Cara Menjalankan
1. Pastikan R dan semua package yang dibutuhkan telah terinstal.
2. Jalankan perintah berikut di terminal R untuk mode produksi:
   ```r
   Rscript launch_dashboard.R
   ```
   Untuk mode pengembangan:
   ```r
   Rscript launch_dashboard.R --dev
   ```
3. Dashboard akan berjalan dan dapat diakses melalui browser.

## Dependensi
- R (versi terbaru)
- Package utama:
  - shiny
  - shinydashboard
  - plotly
  - leaflet
  - DT
  - rmarkdown
  - dplyr, tidyr, readr, stringr, lubridate, dan package statistik/spasial lainnya
- File data: `data/sovi_data.csv`, `data/indonesia_kabkota.geojson`, dll.

## Kontribusi
Kontribusi sangat terbuka! Untuk berkontribusi:
1. Fork repository ini.
2. Buat branch baru untuk fitur atau perbaikan.
3. Lakukan perubahan dan pastikan kode sudah teruji.
4. Ajukan pull request dengan deskripsi perubahan yang jelas.

## Lisensi
Proyek ini belum dilisensikan.

---

Jika ada pertanyaan atau saran, silakan hubungi pengembang melalui halaman issues di repository ini.
