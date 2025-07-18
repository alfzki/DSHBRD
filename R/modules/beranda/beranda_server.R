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
                h3("ALIVA: Alif's Vulnerability Analytics Dashboard"),
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
    })
}
