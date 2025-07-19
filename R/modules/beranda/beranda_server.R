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
        # Welcome content with expanded methodology
        output$welcome_content <- renderUI({
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
                ),
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
                ),
                hr(),
                h4("Sumber Data dan Validasi:"),
                div(
                    class = "alert alert-success", role = "alert",
                    p(strong("Primary Dataset: "), "SUSENAS (Survei Sosial Ekonomi Nasional) 2017"),
                    p(strong("Sumber: "), "BPS-Statistics Indonesia"),
                    p(strong("Cakupan: "), "511 kabupaten/kota di seluruh Indonesia"),
                    p(strong("Validasi: "), "Data telah melalui quality control dan standardization")
                ),
                h5("Struktur Dataset:"),
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
            )
        })

        # Enhanced dataset information
        output$dataset_info <- renderUI({
            if (is.null(values$sovi_data)) {
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
                        span(class = "info-box-number", nrow(values$sovi_data), " observasi")
                    )
                ),
                br(),
                div(
                    class = "info-box bg-green",
                    div(class = "info-box-icon", icon("list")),
                    div(
                        class = "info-box-content",
                        span(class = "info-box-text", "Total Variabel"),
                        span(class = "info-box-number", ncol(values$sovi_data))
                    )
                ),
                br(),
                div(
                    class = "info-box bg-yellow",
                    div(class = "info-box-icon", icon("hashtag")),
                    div(
                        class = "info-box-content",
                        span(class = "info-box-text", "Variabel Numerik"),
                        span(class = "info-box-number", length(get_numeric_columns(values$sovi_data)))
                    )
                ),
                br(),
                div(
                    class = "info-box bg-red",
                    div(class = "info-box-icon", icon("tags")),
                    div(
                        class = "info-box-content",
                        span(class = "info-box-text", "Variabel Kategorik"),
                        span(class = "info-box-number", length(get_categorical_columns(values$sovi_data)))
                    )
                ),
                hr(),
                if (!is.null(values$distance_data)) {
                    tagList(
                        h5(icon("ruler"), " Dataset Distance"),
                        p(paste("Observasi:", nrow(values$distance_data))),
                        p(paste("Variabel:", ncol(values$distance_data)))
                    )
                } else {
                    p(class = "text-muted", "Dataset distance belum dimuat")
                },
                hr(),
                div(
                    class = "alert alert-info", style = "font-size: 0.85em;",
                    strong("Kualitas Data:"), br(),
                    "✓ Data telah divalidasi BPS-Statistics Indonesia", br(),
                    "✓ Cakupan nasional (seluruh provinsi)", br(),
                    "✓ Representatif tingkat kabupaten/kota"
                )
            )
        })

        # Geographic context map with major provincial capitals
        output$indonesia_map <- renderLeaflet({
            # Major provincial capitals of Indonesia with accurate coordinates
            provincial_capitals <- data.frame(
                city = c(
                    "Jakarta", "Medan", "Palembang", "Bandar Lampung", "Bandung", "Semarang",
                    "Yogyakarta", "Surabaya", "Denpasar", "Mataram", "Pontianak", "Palangkaraya",
                    "Samarinda", "Makassar", "Palu", "Manado", "Ambon", "Jayapura", "Manokwari"
                ),
                province = c(
                    "DKI Jakarta", "Sumatera Utara", "Sumatera Selatan", "Lampung", "Jawa Barat",
                    "Jawa Tengah", "D.I. Yogyakarta", "Jawa Timur", "Bali", "Nusa Tenggara Barat",
                    "Kalimantan Barat", "Kalimantan Tengah", "Kalimantan Timur", "Sulawesi Selatan",
                    "Sulawesi Tengah", "Sulawesi Utara", "Maluku", "Papua", "Papua Barat"
                ),
                lng = c(
                    106.8456, 98.6785, 104.7458, 105.2677, 107.6191, 110.4203,
                    110.3650, 112.7521, 115.2192, 116.1158, 109.3200, 113.9213,
                    117.1364, 119.4221, 119.8707, 124.8420, 128.1777, 140.7181, 134.0640
                ),
                lat = c(
                    -6.2088, 3.5952, -2.9167, -5.4500, -6.9175, -6.9930,
                    -7.7972, -7.2575, -8.6705, -8.5833, -0.0263, -2.2090,
                    -0.5017, -5.1477, -0.8917, 1.4748, -3.6954, -2.5337, -0.8650
                ),
                region = c(
                    "Jawa", "Sumatera", "Sumatera", "Sumatera", "Jawa", "Jawa",
                    "Jawa", "Jawa", "Nusa Tenggara", "Nusa Tenggara", "Kalimantan", "Kalimantan",
                    "Kalimantan", "Sulawesi", "Sulawesi", "Sulawesi", "Maluku", "Papua", "Papua"
                )
            )

            # Create color palette for regions
            region_colors <- c(
                "Jawa" = "blue", "Sumatera" = "red", "Kalimantan" = "green",
                "Sulawesi" = "orange", "Nusa Tenggara" = "purple",
                "Maluku" = "brown", "Papua" = "pink"
            )

            leaflet() %>%
                addTiles() %>%
                setView(lng = 118.0148634, lat = -2.548926, zoom = 5) %>%
                addMarkers(
                    lng = provincial_capitals$lng,
                    lat = provincial_capitals$lat,
                    popup = paste(
                        "<b>", provincial_capitals$city, "</b><br/>",
                        "Provinsi: ", provincial_capitals$province, "<br/>",
                        "Wilayah: ", provincial_capitals$region
                    ),
                    label = paste(provincial_capitals$city, "-", provincial_capitals$province),
                    clusterOptions = markerClusterOptions()
                ) %>%
                addCircleMarkers(
                    data = provincial_capitals,
                    lng = ~lng,
                    lat = ~lat,
                    radius = 6,
                    color = ~ region_colors[region],
                    fillColor = ~ region_colors[region],
                    fillOpacity = 0.7,
                    stroke = TRUE,
                    weight = 2,
                    popup = ~ paste(
                        "<b>", city, "</b><br/>",
                        "Provinsi: ", province, "<br/>",
                        "Wilayah: ", region, "<br/>",
                        "Koordinat: ", round(lat, 3), ", ", round(lng, 3)
                    ),
                    label = ~ paste(city, "(", province, ")")
                ) %>%
                addLegend(
                    position = "bottomright",
                    colors = unique(region_colors),
                    labels = names(region_colors),
                    title = "Wilayah Indonesia",
                    opacity = 0.8
                )
        })

        # SOVI dataset metadata table
        output$sovi_metadata_table <- DT::renderDT({
            sovi_metadata <- data.frame(
                "Variabel" = c(
                    "DISTRICTCODE", "CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE",
                    "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING",
                    "DPRONE", "RENTED", "NOSEWER", "TAPWATER", "POPULATION",
                    "district", "region", "island", "province"
                ),
                "Deskripsi" = c(
                    "Kode unik kabupaten/kota (numerik)",
                    "Persentase populasi berusia < 5 tahun",
                    "Persentase populasi berjenis kelamin perempuan",
                    "Persentase populasi berusia ≥ 65 tahun",
                    "Persentase RT dengan kepala keluarga perempuan",
                    "Rata-rata jumlah anggota per rumah tangga",
                    "Persentase RT tanpa akses listrik PLN",
                    "Persentase populasi pendidikan ≤ SD",
                    "Persentase pertumbuhan populasi tahunan",
                    "Persentase penduduk di bawah garis kemiskinan",
                    "Persentase populasi tidak bisa baca tulis",
                    "Persentase RT tanpa pelatihan mitigasi bencana",
                    "Persentase RT di kawasan rawan bencana",
                    "Persentase RT status tempat tinggal sewa",
                    "Persentase RT tanpa akses sistem drainase",
                    "Persentase RT menggunakan air ledeng/PDAM",
                    "Jumlah total populasi (jiwa)",
                    "Nama kabupaten/kota (dibangkitkan otomatis)",
                    "Wilayah regional Indonesia (4 kelompok)",
                    "Kelompok pulau utama (7 kategori)",
                    "Nama provinsi berdasarkan kode wilayah"
                ),
                "Tipe" = c(
                    "Integer", rep("Numeric (0-100)", 15), "Integer",
                    rep("Character/Factor", 4)
                ),
                "Satuan" = c(
                    "Kode", rep("Persen (%)", 15), "Jiwa",
                    "Nama", "Kategori", "Kategori", "Nama"
                ),
                "Sumber" = c(
                    rep("SUSENAS 2017 (BPS)", 17),
                    "Generated", "Generated", "Generated", "Generated"
                ),
                "Rentang" = c(
                    "1101-9471", "0.8-18.2", "45.2-55.8", "2.1-15.4", "8.7-35.2", "2.1-6.8",
                    "0.0-73.4", "8.9-78.6", "-15.8-12.4", "2.4-55.8", "0.1-35.7", "14.2-100.0",
                    "0.0-100.0", "2.1-89.4", "0.0-97.8", "0.2-94.6", "12,047-10,374,235",
                    "511 kategori", "4 kategori", "7 kategori", "34 provinsi"
                ),
                check.names = FALSE
            )

            DT::datatable(sovi_metadata,
                options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    columnDefs = list(list(width = "200px", targets = 1)),
                    dom = "Bfrtip",
                    buttons = c("copy", "csv", "excel")
                ),
                class = "table-striped table-hover compact",
                rownames = FALSE,
                caption = "Metadata lengkap dataset SOVI dengan 511 kabupaten/kota Indonesia"
            ) %>%
                DT::formatStyle(columns = 1:6, fontSize = "90%")
        })

        # Distance dataset metadata table
        output$distance_metadata_table <- DT::renderDT({
            if (is.null(values$distance_data)) {
                return(DT::datatable(
                    data.frame("Info" = "Dataset distance belum dimuat"),
                    options = list(dom = "t"),
                    rownames = FALSE
                ))
            }

            # Analyze distance data structure
            distance_cols <- names(values$distance_data)
            distance_metadata <- data.frame(
                "Variabel" = distance_cols,
                "Deskripsi" = sapply(distance_cols, function(x) {
                    if (grepl("^(from|origin)", tolower(x))) {
                        "Kode/ID kabupaten/kota asal"
                    } else if (grepl("^(to|dest)", tolower(x))) {
                        "Kode/ID kabupaten/kota tujuan"
                    } else if (grepl("distance", tolower(x))) {
                        "Jarak antar kabupaten/kota"
                    } else if (grepl("time", tolower(x))) {
                        "Waktu tempuh antar lokasi"
                    } else if (grepl("(lat|lng|long)", tolower(x))) {
                        "Koordinat geografis (latitude/longitude)"
                    } else if (grepl("name", tolower(x))) {
                        "Nama kabupaten/kota"
                    } else {
                        "Informasi geografis atau identifikasi wilayah"
                    }
                }),
                "Tipe" = sapply(values$distance_data[distance_cols], function(col) {
                    if (is.numeric(col)) {
                        if (all(col == floor(col), na.rm = TRUE)) {
                            "Integer"
                        } else {
                            "Numeric"
                        }
                    } else if (is.character(col) || is.factor(col)) {
                        "Character/Factor"
                    } else {
                        class(col)[1]
                    }
                }),
                "Contoh_Nilai" = sapply(distance_cols, function(x) {
                    col_data <- values$distance_data[[x]]
                    if (is.numeric(col_data)) {
                        paste("Range:", round(min(col_data, na.rm = TRUE), 2), "-", round(max(col_data, na.rm = TRUE), 2))
                    } else {
                        paste("Misal:", paste(head(unique(col_data), 2), collapse = ", "))
                    }
                }),
                check.names = FALSE
            )

            DT::datatable(distance_metadata,
                options = list(
                    pageLength = 8,
                    scrollX = TRUE,
                    dom = "Bfrtip",
                    buttons = c("copy", "csv")
                ),
                class = "table-striped table-hover compact",
                rownames = FALSE,
                caption = paste("Struktur dataset distance dengan", nrow(values$distance_data), "baris data")
            ) %>%
                DT::formatStyle(columns = 1:4, fontSize = "90%")
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
