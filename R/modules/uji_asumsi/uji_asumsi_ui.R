# Uji Asumsi UI Module
# User interface for assumption testing functionality

#' Uji Asumsi UI Module
#'
#' Creates the user interface for statistical assumption testing
#'
#' @param id Module ID for namespacing
#' @return UI elements for the uji_asumsi tab
uji_asumsi_ui <- function(id) {
    ns <- NS(id)
    tabItem(
        tabName = "uji_asumsi",
        fluidRow(
            column(
                width = 3,
                # Settings for Normality Test
                box(
                    id = ns("normality_settings"),
                    title = tags$span(icon("bell"), "Pengaturan Uji Normalitas"),
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    selectInput(ns("var_normal"), "Pilih Variabel:",
                        choices = NULL, width = "100%"
                    )
                ),
                
                # Settings for Homogeneity Test
                box(
                    id = ns("homogeneity_settings"),
                    title = tags$span(icon("balance-scale"), "Pengaturan Uji Homogenitas"),
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    style = "display: none;",
                    selectInput(ns("var_homogen"), "Variabel Dependen:",
                        choices = NULL, width = "100%"
                    ),
                    selectInput(ns("group_homogen"), "Variabel Grup:",
                        choices = NULL, width = "100%"
                    )
                ),
                
                # Download section
                box(
                    title = tags$span(icon("download"), "Unduh Laporan"),
                    status = "info",
                    solidHeader = TRUE,
                    width = NULL,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    h6("Interpretasi Individual"),
                    downloadButton(ns("download_interpretation"), "Unduh Interpretasi (.docx)",
                        class = "btn-outline-info btn-sm", icon = icon("file-word"), width = "100%"
                    ),
                    br(), br(),
                    h6("Laporan Lengkap"),
                    downloadButton(ns("download_report_pdf"), "Unduh Laporan PDF",
                        class = "btn-primary", icon = icon("file-pdf"), width = "100%"
                    ),
                    br(), br(),
                    downloadButton(ns("download_report_word"), "Unduh Laporan Word",
                        class = "btn-success", icon = icon("file-word"), width = "100%"
                    )
                )
            ),
            column(
                width = 9,
                # Add JavaScript for tab switching
                tags$script(HTML(paste0("
                    $(document).ready(function() {
                        // Function to show/hide settings based on active tab
                        function updateSettings(activeTab) {
                            if (activeTab.includes('Normalitas')) {
                                $('#", ns("normality_settings"), "').show();
                                $('#", ns("homogeneity_settings"), "').hide();
                            } else if (activeTab.includes('Homogenitas')) {
                                $('#", ns("normality_settings"), "').hide();
                                $('#", ns("homogeneity_settings"), "').show();
                            }
                        }
                        
                        // Handle tab clicks with more specific selector
                        $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function(e) {
                            var activeTab = $(e.target).text().trim();
                            updateSettings(activeTab);
                        });
                        
                        // Also handle clicks on the tab directly
                        $(document).on('click', 'a[data-toggle=\"tab\"]', function(e) {
                            setTimeout(function() {
                                var activeTab = $('.nav-tabs .active a').text().trim();
                                updateSettings(activeTab);
                            }, 100);
                        });
                        
                        // Initialize with first tab after a short delay to ensure DOM is ready
                        setTimeout(function() {
                            updateSettings('Uji Normalitas');
                        }, 500);
                    });
                "))),
                tabsetPanel(
                    tabPanel(
                        "Uji Normalitas",
                        icon = icon("bell"),
                        br(),
                        verbatimTextOutput(ns("normality_test")),
                        plotOutput(ns("normality_plot"), height = "400px"),
                        br(),
                        uiOutput(ns("interpretation_normal"))
                    ),
                    tabPanel(
                        "Uji Homogenitas",
                        icon = icon("balance-scale"),
                        br(),
                        verbatimTextOutput(ns("homogeneity_test")),
                        br(),
                        uiOutput(ns("interpretation_homogen"))
                    )
                )
            )
        )
    )
}
