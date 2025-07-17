# NusaStat: Dasbor Interaktif Analisis Kerentanan Sosial & Statistik Indonesia
# Developed for UAS Komputasi Statistik - Enhanced Version
# Author: AI Agent Pemrograman R
# Date: July 17, 2025
# Version: 2.0 - Comprehensive Implementation

# Load required libraries and global configurations
source("global.R")

# Load UI modules
source("R/ui_modules.R")

# Load server modules
source("R/server_modules.R")
source("R/additional_server_modules.R")

# Define UI
ui <- dashboardPage(
    skin = "blue",

    # Header
    dashboardHeader(
        title = tags$span(
            icon("chart-line"),
            "NusaStat Dashboard",
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
        processed_data = NULL
    )

    # Load data on startup
    observe({
        values$sovi_data <- load_sovi_data()
        values$distance_data <- load_distance_data()
        values$processed_data <- values$sovi_data
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
