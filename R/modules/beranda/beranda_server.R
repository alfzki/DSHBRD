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
                    "Persentase Kepala Keluarga Wanita", "Rata-rata Ukuran Keluarga", "Persentase Rumah Tangga Tanpa Listrik",
                    "Persentase Pendidikan Rendah", "Persentase Pertumbuhan Populasi", "Persentase Penduduk Miskin",
                    "Persentase Populasi Buta Huruf", "Persentase Tanpa Pelatihan Bencana", "Persentase Daerah Rawan Bencana",
                    "Persentase Rumah Sewa", "Persentase Tanpa Sistem Saluran Pembuangan", "Persentase Menggunakan Air Ledeng", "Total Populasi"
                ),
                "Nama Variabel Asli" = c(
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
                    "Persentase populasi yang buta huruf", "Persentase rumah tangga tanpa pelatihan bencana", "Persentase rumah tangga di daerah rawan bencana",
                    "Persentase rumah tangga yang menyewa", "Persentase rumah tangga tanpa sistem saluran pembuangan", "Persentase rumah tangga yang menggunakan air ledeng/pipa", "Total populasi"
                ),
                "Tipe Data" = c(
                    "Kategorik", "Kategorik", "Kategorik", "Kategorik",
                    rep("Numerik", 16)
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
            filename = function() {
                paste("ALIVA_Dashboard_Info_", Sys.Date(), ".pdf", sep = "")
            },
            content = function(file) {
                # Create a temporary Rmd file
                temp_rmd <- tempfile(fileext = ".Rmd")

                # Write the content to the Rmd file
                writeLines(c(
                    "---",
                    "title: 'ALIVA Dashboard - Informasi Lengkap'",
                    "author: 'Tim Dashboard ALIVA'",
                    "date: '`r Sys.Date()`'",
                    "output: pdf_document",
                    "---",
                    "",
                    "# ALIVA: Alif's Vulnerability Analytics Dashboard",
                    "",
                    "## Ringkasan",
                    "",
                    "ALIVA (Alif's Vulnerability Analytics) adalah dashboard interaktif yang dirancang khusus untuk analisis kerentanan sosial Indonesia. Dashboard ini menyediakan platform komprehensif untuk eksplorasi, analisis, dan visualisasi data kerentanan sosial dengan menggunakan metodologi statistik yang rigorous.",
                    "",
                    "## Fitur Utama",
                    "",
                    "### 1. Manajemen Data",
                    "- Transformasi variabel kontinu menjadi kategorik",
                    "- Kategorisasi berbasis quartiles dan cut-points",
                    "- Validasi dan pembersihan data otomatis",
                    "",
                    "### 2. Eksplorasi Data",
                    "- Statistik deskriptif komprehensif",
                    "- Visualisasi distribusi (histogram, boxplot)",
                    "- Peta spasial interaktif untuk analisis geografis",
                    "- Matriks korelasi dan scatter plots",
                    "",
                    "### 3. Uji Asumsi Statistik",
                    "- Uji normalitas (Shapiro-Wilk, Kolmogorov-Smirnov)",
                    "- Uji homogenitas varians (Levene's test)",
                    "- Interpretasi otomatis dan rekomendasi tindak lanjut",
                    "",
                    "### 4. Statistik Inferensia",
                    "",
                    "#### Uji Beda Rata-rata:",
                    "- One-sample t-test",
                    "- Independent two-sample t-test",
                    "- Paired t-test",
                    "- Welch's t-test untuk varians tidak sama",
                    "",
                    "#### Uji Proporsi & Varians:",
                    "- One-sample proportion test",
                    "- Two-sample proportion test",
                    "- Chi-square goodness of fit",
                    "- F-test untuk perbandingan varians",
                    "",
                    "#### ANOVA:",
                    "- One-way ANOVA",
                    "- Two-way ANOVA",
                    "- Post-hoc tests (Tukey HSD)",
                    "- Analisis efek interaksi",
                    "",
                    "### 5. Regresi Linear Berganda",
                    "- Model regresi dengan multiple predictors",
                    "- Uji asumsi regresi lengkap:",
                    "  - Linearitas",
                    "  - Independensi residual",
                    "  - Homoskedastisitas",
                    "  - Normalitas residual",
                    "  - Multikolinearitas (VIF)",
                    "- Model selection dan validation",
                    "- Interpretasi koefisien dan significance testing",
                    "",
                    "### 6. Sistem Ekspor dan Pelaporan",
                    "- Export hasil dalam format PDF",
                    "- Export dalam format Microsoft Word",
                    "- Export data dalam format CSV",
                    "- Template laporan yang dapat dikustomisasi",
                    "",
                    "## Sumber Data",
                    "",
                    "Dashboard ini menggunakan data dari SUSENAS (Survei Sosial Ekonomi Nasional) 2017 yang diterbitkan oleh BPS-Statistics Indonesia. Dataset mencakup:",
                    "",
                    "- **Cakupan Geografis**: 511 kabupaten/kota di seluruh Indonesia",
                    "- **Jumlah Variabel**: 17 indikator kerentanan sosial",
                    "- **Periode**: Data tahun 2017",
                    "",
                    "### Variabel yang Tersedia:",
                    "",
                    "1. **CHILDREN**: Persentase populasi berusia di bawah lima tahun",
                    "2. **FEMALE**: Persentase populasi perempuan",
                    "3. **ELDERLY**: Persentase populasi berusia 65 tahun ke atas",
                    "4. **FHEAD**: Persentase rumah tangga dengan kepala keluarga perempuan",
                    "5. **FAMILYSIZE**: Rata-rata jumlah anggota rumah tangga",
                    "6. **NOELECTRIC**: Persentase rumah tangga tanpa akses listrik",
                    "7. **LOWEDU**: Persentase populasi dengan pendidikan rendah",
                    "8. **GROWTH**: Persentase pertumbuhan populasi",
                    "9. **POVERTY**: Persentase penduduk miskin",
                    "10. **ILLITERATE**: Persentase populasi yang buta huruf",
                    "11. **NOTRAINING**: Persentase rumah tangga tanpa pelatihan bencana",
                    "12. **DPRONE**: Persentase rumah tangga di daerah rawan bencana",
                    "13. **RENTED**: Persentase rumah tangga yang menyewa",
                    "14. **NOSEWER**: Persentase rumah tangga tanpa sistem saluran pembuangan",
                    "15. **TAPWATER**: Persentase rumah tangga menggunakan air ledeng",
                    "16. **POPULATION**: Total populasi",
                    "17. **DISTRICTCODE**: Kode kabupaten/kota (identifikasi)",
                    "",
                    "## Metodologi Analisis",
                    "",
                    "### Pendekatan Statistik",
                    "Dashboard menggunakan pendekatan statistik yang komprehensif dengan emphasis pada:",
                    "",
                    "1. **Exploratory Data Analysis (EDA)**",
                    "2. **Confirmatory Statistical Testing**",
                    "3. **Model Building and Validation**",
                    "4. **Spatial Analysis**",
                    "",
                    "### Validasi dan Quality Assurance",
                    "- Semua uji statistik menggunakan alpha = 0.05 sebagai default",
                    "- Automatic assumption checking sebelum analisis",
                    "- Robust methods untuk data yang tidak memenuhi asumsi",
                    "- Multiple comparison corrections tersedia",
                    "",
                    "## Cara Penggunaan",
                    "",
                    "### 1. Navigasi Dashboard",
                    "Gunakan menu sidebar untuk mengakses berbagai fitur:",
                    "- **Beranda**: Informasi umum dan metadata",
                    "- **Manajemen Data**: Transformasi dan kategorisasi variabel",
                    "- **Eksplorasi Data**: Analisis deskriptif dan visualisasi",
                    "- **Uji Asumsi**: Validasi asumsi untuk analisis lanjutan",
                    "- **Statistik Inferensia**: Uji hipotesis berbagai jenis",
                    "- **Regresi**: Analisis regresi linear berganda",
                    "",
                    "### 2. Workflow yang Disarankan",
                    "1. Mulai dengan **Eksplorasi Data** untuk memahami karakteristik dataset",
                    "2. Gunakan **Uji Asumsi** untuk memvalidasi persyaratan analisis",
                    "3. Lakukan **Manajemen Data** jika diperlukan transformasi",
                    "4. Pilih uji yang appropriate di **Statistik Inferensia**",
                    "5. Untuk analisis hubungan kompleks, gunakan **Regresi**",
                    "6. Export hasil untuk dokumentasi dan sharing",
                    "",
                    "### 3. Interpretasi Hasil",
                    "Dashboard menyediakan interpretasi otomatis untuk semua analisis dalam bahasa Indonesia yang mudah dipahami, termasuk:",
                    "- Kesimpulan statistik",
                    "- Practical significance",
                    "- Rekomendasi tindak lanjut",
                    "- Peringatan tentang limitasi",
                    "",
                    "## Technical Specifications",
                    "",
                    "### Platform dan Dependencies",
                    "- **R version**: 4.0 atau lebih tinggi",
                    "- **Shiny framework**: untuk interactive web applications",
                    "- **Statistical packages**: car, lmtest, nortest, broom",
                    "- **Visualization**: ggplot2, plotly, leaflet",
                    "- **Data manipulation**: dplyr, tidyr",
                    "",
                    "### System Requirements",
                    "- RAM minimum: 4GB",
                    "- Browser: Chrome, Firefox, Safari (versi terbaru)",
                    "- Koneksi internet untuk beberapa fitur mapping",
                    "",
                    "## Kontribusi dan Development",
                    "",
                    "### Tim Pengembang",
                    "ALIVA Dashboard dikembangkan oleh tim yang terdiri dari:",
                    "- Statistician",
                    "- Data Scientists",
                    "- R/Shiny Developers",
                    "- UI/UX Specialists",
                    "",
                    "### Roadmap",
                    "Pengembangan berkelanjutan mencakup:",
                    "- Penambahan metode analisis multivariat",
                    "- Machine learning capabilities",
                    "- Real-time data integration",
                    "- Mobile-responsive design improvements",
                    "",
                    "## Referensi dan Sitasi",
                    "",
                    "### Data Source",
                    "BPS-Statistics Indonesia. (2017). SUSENAS: Survei Sosial Ekonomi Nasional 2017.",
                    "",
                    "### Statistical Methods References",
                    "- Kutner, M. H., Nachtsheim, C. J., Neter, J., & Li, W. (2005). Applied Linear Statistical Models.",
                    "- Field, A. (2013). Discovering Statistics Using IBM SPSS Statistics.",
                    "- R Core Team (2023). R: A Language and Environment for Statistical Computing.",
                    "",
                    "## Kontak dan Support",
                    "",
                    "Untuk pertanyaan, bug reports, atau feature requests:",
                    "- Email: support@aliva-dashboard.id",
                    "- Documentation: https://aliva-dashboard.github.io",
                    "- Issues: https://github.com/aliva-dashboard/issues",
                    "",
                    "---",
                    "",
                    "*Generated on `r Sys.Date()` by ALIVA Dashboard Information System*"
                ), temp_rmd)

                # Render the Rmd file to PDF
                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )

        # Combined download handlers for all dashboard reports
        output$download_combined_pdf <- downloadHandler(
            filename = function() {
                paste("ALIVA_Complete_Dashboard_Report_", Sys.Date(), ".pdf", sep = "")
            },
            content = function(file) {
                # This would combine reports from all modules
                # For now, just provide the dashboard info
                temp_rmd <- tempfile(fileext = ".Rmd")

                writeLines(c(
                    "---",
                    "title: 'ALIVA Dashboard - Laporan Lengkap Semua Fitur'",
                    "author: 'Tim Dashboard ALIVA'",
                    "date: '`r Sys.Date()`'",
                    "output: pdf_document",
                    "---",
                    "",
                    "# Laporan Komprehensif ALIVA Dashboard",
                    "",
                    "Laporan ini berisi ringkasan dari semua fitur dan analisis yang tersedia di ALIVA Dashboard.",
                    "",
                    "## 1. Informasi Dashboard",
                    "ALIVA Dashboard menyediakan workflow analisis statistik yang komprehensif dari eksplorasi data hingga regresi linear berganda.",
                    "",
                    "## 2. Dataset Information",
                    if (!is.null(values$sovi_data)) {
                        paste("- Jumlah observasi:", nrow(values$sovi_data))
                    } else {
                        "- Data sedang dimuat..."
                    },
                    if (!is.null(values$sovi_data)) {
                        paste("- Jumlah variabel:", ncol(values$sovi_data))
                    } else {
                        ""
                    },
                    "",
                    "## 3. Fitur yang Tersedia",
                    "- Manajemen Data: Transformasi variabel",
                    "- Eksplorasi Data: Statistik deskriptif dan visualisasi",
                    "- Uji Asumsi: Normalitas dan homogenitas",
                    "- Statistik Inferensia: T-test, ANOVA, uji proporsi",
                    "- Regresi Linear Berganda: Model prediktif",
                    "",
                    "---",
                    "Untuk laporan detail dari setiap modul, silakan gunakan fitur download di masing-masing tab."
                ), temp_rmd)

                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )

        output$download_combined_word <- downloadHandler(
            filename = function() {
                paste("ALIVA_Complete_Dashboard_Report_", Sys.Date(), ".docx", sep = "")
            },
            content = function(file) {
                temp_rmd <- tempfile(fileext = ".Rmd")

                writeLines(c(
                    "---",
                    "title: 'ALIVA Dashboard - Laporan Lengkap'",
                    "author: 'Tim Dashboard ALIVA'",
                    "date: '`r Sys.Date()`'",
                    "output: word_document",
                    "---",
                    "",
                    "# ALIVA Dashboard - Laporan Komprehensif",
                    "",
                    "Dashboard ALIVA menyediakan analisis statistik komprehensif untuk data kerentanan sosial Indonesia.",
                    "",
                    "## Ringkasan Fitur",
                    "1. **Eksplorasi Data**: Analisis deskriptif lengkap",
                    "2. **Uji Asumsi**: Validasi persyaratan statistik",
                    "3. **Analisis Inferensia**: Berbagai uji hipotesis",
                    "4. **Regresi**: Model prediktif multivariat",
                    "",
                    "## Data Information",
                    if (!is.null(values$sovi_data)) {
                        paste("Dataset berisi", nrow(values$sovi_data), "observasi dengan", ncol(values$sovi_data), "variabel.")
                    } else {
                        "Data sedang dimuat..."
                    },
                    "",
                    "Sumber: SUSENAS 2017, BPS-Statistics Indonesia"
                ), temp_rmd)

                rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
            }
        )
    })
}