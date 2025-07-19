# ==============================================================================
# APLIKASI UTAMA DASHBOARD ALIVA
# ==============================================================================
#
# Tujuan: Aplikasi utama untuk Dashboard Analisis Kerentanan Sosial Indonesia
# Penulis: Tim Dashboard ALIVA
# Terakhir Diperbarui: Juli 2025
# Versi: 3.0 - Implementasi Arsitektur Modular
#
# Deskripsi:
# ALIVA adalah dashboard interaktif untuk analisis kerentanan sosial dan
# statistik Indonesia yang dikembangkan menggunakan R Shiny dengan
# arsitektur modular.
#
# Fitur Utama:
# - Manajemen data dengan kategorisasi variabel
# - Eksplorasi data dengan visualisasi interaktif
# - Uji asumsi statistik (normalitas & homogenitas)
# - Uji statistik inferensia (t-test, ANOVA, proporsi, varians)
# - Analisis regresi linear berganda
# - Export laporan dalam berbagai format
# ==============================================================================

# Memuat pustaka dan konfigurasi global yang diperlukan
source("global.R")

# Memuat komponen modular
source("R/load_modules.R")
load_all_modules()

# Definisi antarmuka pengguna (UI)
ui <- dashboardPage(
  title = "ALIVA Dashboard",
  skin = "blue",

  # Header
  dashboardHeader(
    title = tags$span(
      icon("chart-line"),
      "ALIVA Dashboard",
      style = "font-size: 18px; font-weight: bold;"
    ),
    titleWidth = 280
  ),

  # Sidebar
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "sidebar",
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen_data", icon = icon("database")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-bar")),
      menuItem("Uji Asumsi", tabName = "uji_asumsi", icon = icon("check-square")),
      menuItem("Statistik Inferensia",
        icon = icon("calculator"), startExpanded = FALSE,
        menuSubItem("Uji Beda Rata-Rata", tabName = "uji_rata", icon = icon("equals")),
        menuSubItem("Uji Proporsi & Varians", tabName = "uji_prop_var", icon = icon("percentage")),
        menuSubItem("ANOVA", tabName = "uji_anova", icon = icon("layer-group"))
      ),
      menuItem("Regresi Linear Berganda", tabName = "regresi", icon = icon("line-chart"))
    )
  ),

  # Body
  dashboardBody(
    # Custom CSS for modern styling
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .main-header .logo {
          background-color: #2c3e50 !important;
        }
        .main-header .navbar {
          background-color: #2c3e50 !important;
        }
        .main-sidebar {
          background-color: #34495e !important;
        }
        .box {
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          border-radius: 8px;
        }
        .box-header {
          border-bottom: 1px solid #e9ecef;
        }
      "))
    ),

    # Tab content
    tabItems(
      beranda_ui("beranda"),
      manajemen_data_ui("manajemen_data"),
      eksplorasi_ui("eksplorasi"),
      uji_asumsi_ui("uji_asumsi"),
      uji_rata_ui("uji_rata"),
      uji_prop_var_ui("uji_prop_var"),
      uji_anova_ui("uji_anova"),
      regresi_ui("regresi")
    )
  )
)

# Definisi logika server
server <- function(input, output, session) {
  # Inisialisasi nilai reaktif untuk berbagi data antar modul
  values <- reactiveValues(
    sovi_data = NULL,
    distance_data = NULL,
    processed_data = NULL,
    data_update_counter = 0, # Penghitung untuk melacak perubahan struktur data
    data_initialized = FALSE, # Flag untuk mencegah inisialisasi ulang
    user_created_vars = list() # Menyimpan variabel buatan pengguna
  )

  # Memuat data saat startup HANYA SEKALI - menggunakan trigger yang lebih handal
  observe({
    if (is.null(values$sovi_data) && !values$data_initialized) {
      cat("APLIKASI UTAMA: ===== PEMUATAN DATA AWAL DIMULAI =====\n")
      values$sovi_data <- load_sovi_data()
      values$distance_data <- load_distance_data()
      values$processed_data <- values$sovi_data
      values$data_initialized <- TRUE
      cat("APLIKASI UTAMA: ===== PEMUATAN DATA AWAL SELESAI =====\n")
    } else if (values$data_initialized) {
      cat("APLIKASI UTAMA: Flag inisialisasi data sudah diatur, mencegah reload\n")
    } else {
      cat("APLIKASI UTAMA: Data sudah dimuat, melewati reload\n")
    }
  })

  # PENTING: Pemulihan otomatis variabel pengguna jika data dimuat ulang
  observe({
    current_data <- values$sovi_data
    if (!is.null(current_data) && length(values$user_created_vars) > 0) {
      # Periksa apakah ada variabel pengguna yang hilang dari data saat ini
      missing_vars <- setdiff(names(values$user_created_vars), names(current_data))
      if (length(missing_vars) > 0) {
        cat("APLIKASI UTAMA: Pemulihan otomatis", length(missing_vars), "variabel pengguna yang hilang\n")
        for (var_name in missing_vars) {
          current_data[[var_name]] <- values$user_created_vars[[var_name]]
          cat("APLIKASI UTAMA: Variabel dipulihkan:", var_name, "\n")
        }
        values$sovi_data <- current_data
        values$data_update_counter <- values$data_update_counter + 1
        cat("APLIKASI UTAMA: Pemulihan otomatis selesai, counter bertambah menjadi:", values$data_update_counter, "\n")
      }
    }
  })

  # Pemanggilan server modul
  beranda_server("beranda", values)
  manajemen_data_server("manajemen_data", values)
  eksplorasi_server("eksplorasi", values)
  uji_asumsi_server("uji_asumsi", values)
  uji_rata_server("uji_rata", values)
  uji_prop_var_server("uji_prop_var", values)
  uji_anova_server("uji_anova", values)
  regresi_server("regresi", values)
}

# Menjalankan aplikasi
shinyApp(ui = ui, server = server)
