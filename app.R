# NusaStat: Dasbor Interaktif Analisis Kerentanan Sosial & Statistik Indonesia
# Developed for UAS Komputasi Statistik - Enhanced Version
# Author: AI Agent Pemrograman R
# Date: July 17, 2025
# Version: 3.0 - Modular Architecture Implementation

# Load required libraries and global configurations
source("global.R")

# Load modular components
source("R/load_modules.R")
load_all_modules()

# Define UI
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

# Define server logic
server <- function(input, output, session) {
  # Initialize reactive values
  values <- reactiveValues(
    sovi_data = NULL,
    distance_data = NULL,
    processed_data = NULL,
    data_update_counter = 0,  # Counter to track data structure changes
    data_initialized = FALSE, # Flag to prevent reinitialization
    user_created_vars = list() # Store user-created variables
  )

  # Load data on startup ONCE - using a more reliable trigger
  observe({
    if (is.null(values$sovi_data) && !values$data_initialized) {
      cat("MAIN APP: ===== INITIAL DATA LOADING TRIGGERED =====\n")
      values$sovi_data <- load_sovi_data()
      values$distance_data <- load_distance_data()
      values$processed_data <- values$sovi_data
      values$data_initialized <- TRUE
      cat("MAIN APP: ===== INITIAL DATA LOADING COMPLETED =====\n")
    } else if (values$data_initialized) {
      cat("MAIN APP: Data initialization flag set, preventing reload\n")
    } else {
      cat("MAIN APP: Data already loaded, skipping reload\n")
    }
  })
  
  # CRITICAL: Auto-restore user variables if data gets reloaded
  observe({
    current_data <- values$sovi_data
    if (!is.null(current_data) && length(values$user_created_vars) > 0) {
      # Check if any user variables are missing from current data
      missing_vars <- setdiff(names(values$user_created_vars), names(current_data))
      if (length(missing_vars) > 0) {
        cat("MAIN APP: Auto-restoring", length(missing_vars), "missing user variables\n")
        for (var_name in missing_vars) {
          current_data[[var_name]] <- values$user_created_vars[[var_name]]
          cat("MAIN APP: Restored variable:", var_name, "\n")
        }
        values$sovi_data <- current_data
        values$data_update_counter <- values$data_update_counter + 1
        cat("MAIN APP: Auto-restore complete, counter incremented to:", values$data_update_counter, "\n")
      }
    }
  })

  # Call server modules
  beranda_server("beranda", values)
  manajemen_data_server("manajemen_data", values)
  eksplorasi_server("eksplorasi", values)
  uji_asumsi_server("uji_asumsi", values)
  uji_rata_server("uji_rata", values)
  uji_prop_var_server("uji_prop_var", values)
  uji_anova_server("uji_anova", values)
  regresi_server("regresi", values)
}

# Run the application
shinyApp(ui = ui, server = server)
