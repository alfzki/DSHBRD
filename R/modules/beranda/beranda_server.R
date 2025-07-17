# Beranda Server Module
# Server logic for the home page of NusaStat Dashboard

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
                h3("NusaStat: Dasbor Interaktif Analisis Kerentanan Sosial & Statistik Indonesia"),
                p("Selamat datang di NusaStat Dashboard! Aplikasi ini dirancang untuk membantu analisis statistik
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
          dari 514 kabupaten/kota di Indonesia.")
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
                Variabel = c(
                    "kepadatan_penduduk", "rasio_ketergantungan", "persentase_lansia",
                    "persentase_balita", "persentase_disabilitas", "persentase_buta_huruf",
                    "persentase_kemiskinan", "persentase_pengangguran", "indeks_kesehatan",
                    "indeks_pendidikan", "indeks_ekonomi", "akses_listrik", "akses_air_bersih",
                    "akses_sanitasi", "sovi_score"
                ),
                Deskripsi = c(
                    "Kepadatan penduduk per km²", "Rasio ketergantungan (%)",
                    "Persentase populasi lansia", "Persentase populasi balita",
                    "Persentase populasi disabilitas", "Persentase buta huruf",
                    "Persentase kemiskinan", "Persentase pengangguran",
                    "Indeks kesehatan", "Indeks pendidikan", "Indeks ekonomi",
                    "Persentase akses listrik", "Persentase akses air bersih",
                    "Persentase akses sanitasi", "Skor kerentanan sosial"
                ),
                Satuan = c(
                    "jiwa/km²", "%", "%", "%", "%", "%", "%", "%",
                    "0-100", "0-100", "0-100", "%", "%", "%", "0-100"
                ),
                Sumber = rep("SUSENAS 2017", 15)
            )

            DT::datatable(metadata,
                options = list(pageLength = 10, scrollX = TRUE),
                class = "table-striped table-hover"
            )
        })

        # Download handler for dashboard info
        output$download_info <- downloadHandler(
            filename = "NusaStat_Info.pdf",
            content = function(file) {
                # Create a temporary R Markdown file
                temp_rmd <- tempfile(fileext = ".Rmd")

                rmd_content <- '---
title: "NusaStat Dashboard - Informasi Lengkap"
output: pdf_document
date: "`r Sys.Date()`"
---

# Tentang NusaStat Dashboard

NusaStat adalah dashboard interaktif untuk analisis kerentanan sosial dan statistik Indonesia yang dikembangkan menggunakan R Shiny.

## Fitur Utama

1. **Manajemen Data**: Transformasi variabel kontinu menjadi kategorik
2. **Eksplorasi Data**: Statistik deskriptif dan visualisasi
3. **Uji Asumsi**: Uji normalitas dan homogenitas
4. **Statistik Inferensia**: Berbagai uji statistik
5. **Regresi Linear Berganda**: Model prediktif lengkap

## Sumber Data

Data berasal dari SUSENAS 2017, BPS-Statistics Indonesia, mencakup 514 kabupaten/kota.

## Metadata Variabel

```{r echo=FALSE}
metadata <- data.frame(
  Variabel = c("kepadatan_penduduk", "rasio_ketergantungan", "persentase_lansia"),
  Deskripsi = c("Kepadatan penduduk per km²", "Rasio ketergantungan (%)", "Persentase populasi lansia"),
  Satuan = c("jiwa/km²", "%", "%")
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
