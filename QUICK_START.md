# ALIVA Dashboard - Quick Start Guide

## 🚀 Quick Start

### Prerequisites
- R version 4.0.0 or higher
- RStudio (recommended) or VS Code with R extension

### Step 1: Open the Project
1. Open the `Dashboard` folder in VS Code or RStudio
2. The project structure should look like this:
```
Dashboard/
├── app.R                    # Main application
├── global.R                 # Global configurations
├── data/                    # Data directory
├── R/                      # R modules
├── www/                    # Web assets
└── README.md               # Documentation
```

### Step 2: Install Required Packages
The dashboard will automatically install missing packages when you run it. You can also pre-install them by running:

**Option 1: Automatic Installation (Recommended)**
```r
source("global.R")
```

**Option 2: Manual Installation**
```r
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets", "shinythemes", "shinyjs",
  "dplyr", "tidyr", "readr", "stringr", "lubridate",
  "ggplot2", "plotly", "leaflet", "DT", "htmlwidgets",
  "car", "lmtest", "nortest", "broom", "psych",
  "rmarkdown", "knitr", "pagedown", "officer", "flextable",
  "here", "glue", "scales", "RColorBrewer", "viridis"
))
```

### Step 3: Launch the Dashboard

**Using VS Code:**
1. Open Command Palette (Ctrl+Shift+P)
2. Type "Tasks: Run Task"
3. Select "Run ALIVA Shiny Dashboard"

**Using R Console:**
```r
shiny::runApp("app.R")
```

**Using RStudio:**
1. Open `app.R`
2. Click "Run App" button

### Step 4: Access the Dashboard
Once running, the dashboard will be available at:
- Local: http://127.0.0.1:3838
- Network: http://0.0.0.0:3838 (if accessible from other devices)

## 📊 Dashboard Features

### 1. Beranda (Home)
- Welcome message and project overview
- Dataset metadata and information
- Download dashboard information

### 2. Manajemen Data (Data Management)
- Convert continuous variables to categorical
- Choose categorization method (equal intervals or quantiles)
- Download results and interpretations

### 3. Eksplorasi Data (Data Exploration)
- Descriptive statistics
- Interactive visualizations with Plotly
- Map visualization (requires spatial data)
- Download plots and summaries

### 4. Uji Asumsi (Assumption Testing)
- Normality tests (Shapiro-Wilk)
- Homogeneity tests (Levene's test)
- Q-Q plots and visual diagnostics

### 5. Statistik Inferensia (Inferential Statistics)

#### Uji Beda Rata-Rata (Mean Difference Tests)
- One-sample t-test
- Two-sample t-test
- Automatic interpretation

#### Uji Proporsi & Varians (Proportion & Variance Tests)
- One-sample proportion test
- One-sample variance test (Chi-square)
- Comprehensive results and interpretation

#### ANOVA
- One-way ANOVA
- Two-way ANOVA (with/without interaction)
- Post-hoc tests (Tukey HSD)
- Interactive visualizations

### 6. Regresi Linear Berganda (Multiple Linear Regression)
- Multiple predictor variables
- Assumption testing:
  - Multicollinearity (VIF)
  - Heteroscedasticity (Breusch-Pagan)
  - Normality of residuals
- Comprehensive model interpretation
- Automatic equation generation

## 🔧 Troubleshooting

### Common Issues

**1. Package Installation Errors**
```r
# Try installing packages one by one
install.packages("shiny")
install.packages("shinydashboard")
# ... continue with other packages
```

**2. Port Already in Use**
```r
# Use a different port
shiny::runApp("app.R", port = 3839)
```

**3. Data Loading Issues**
```r
# Check if data files exist
file.exists("data/sovi_data.csv")
file.exists("data/distance.csv")
# Jika data tidak ada, silakan hubungi pengelola untuk mendapatkan data yang benar.
```

**4. Memory Issues with Large Datasets**
- The dashboard automatically samples large datasets for certain tests
- For very large datasets, consider preprocessing the data

### Development Mode

For development with auto-reload:
```r
options(shiny.autoreload = TRUE)
shiny::runApp("app.R", launch.browser = TRUE)
```

## 📁 File Structure Explained

- **`app.R`**: Main application file that combines UI and server
- **`global.R`**: Package loading, data loading, and utility functions
- **`R/ui_modules.R`**: UI module definitions for each dashboard section
- **`R/server_modules.R`**: Server logic for main modules
- **`R/additional_server_modules.R`**: Additional server modules for statistical tests
- **`www/custom.css`**: Custom styling for modern appearance
- **`data/`**: Directory for CSV data files
- **`.vscode/tasks.json`**: VS Code tasks for running the application

## 🎯 Usage Tips

1. **Start with Data Exploration**: Get familiar with your data using the exploration module
2. **Check Assumptions**: Always test statistical assumptions before running tests
3. **Use Downloads**: All results can be downloaded in multiple formats (PDF, CSV, TXT)
4. **Read Interpretations**: The dashboard provides automated interpretations for all statistical tests
5. **Try Different Variables**: Experiment with different variable combinations to understand relationships

## 🆘 Getting Help

1. **Built-in Help**: Each module includes instructions and interpretations
2. **Error Messages**: The dashboard provides user-friendly error messages
3. **Validation**: Input validation prevents common errors
4. **Sample Data**: Instruksi penggunaan sample data dihapus. Silakan gunakan data asli.

## 🔄 Updates and Maintenance

To update the dashboard:
1. Check for new package versions
2. Update data files as needed
3. Modify modules in the `R/` directory
4. Test changes with sample data before using real data

---

**Happy Analyzing! 📈**

For more detailed information, see the complete README.md file.
