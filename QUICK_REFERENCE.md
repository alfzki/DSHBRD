# 🚀 ALIVA Dashboard - Quick Reference

## Essential Commands

### Launch Dashboard
```r
# Set working directory to Dashboard folder
setwd("path/to/Dashboard")

# Install all required packages
source("global.R")

# Launch the application
source("app.R")

# Access at: http://127.0.0.1:3838
```

### Validation Test
```r
# Run comprehensive validation
source("validate_dashboard.R")
```

## 📁 Documentation Files

| File                   | Purpose                          | When to Use                    |
| ---------------------- | -------------------------------- | ------------------------------ |
| `README.md`            | Main overview and quick start    | First-time setup               |
| `USER_MANUAL.md`       | Step-by-step tutorials           | Learning how to use features   |
| `FEATURES.md`          | Complete technical documentation | Understanding all capabilities |
| `TROUBLESHOOTING.md`   | Problem resolution guide         | When encountering issues       |
| `COMPLETION_REPORT.md` | Project completion summary       | UAS submission reference       |

## 🎯 Menu Quick Guide

| Menu                       | Purpose              | Key Features                              |
| -------------------------- | -------------------- | ----------------------------------------- |
| 🏠 **Beranda**              | Overview & Info      | Metadata, dataset info, PDF download      |
| 🗄️ **Manajemen Data**       | Data transformation  | Continuous → categorical conversion       |
| 📊 **Eksplorasi Data**      | Descriptive analysis | Summary stats, histograms, maps           |
| ✅ **Uji Asumsi**           | Assumption testing   | Normality, homogeneity tests              |
| 🧮 **Statistik Inferensia** | Hypothesis testing   | t-tests, ANOVA, proportion/variance tests |
| 📈 **Regresi**              | Predictive modeling  | Multiple regression with diagnostics      |

## ⚡ Common Tasks

### Quick Data Exploration
1. Go to **Eksplorasi Data**
2. Select variable → View stats & histogram
3. Download summary (PDF) or plot (PNG)

### Statistical Test
1. Check assumptions in **Uji Asumsi** first
2. Go to appropriate test in **Statistik Inferensia**
3. Configure parameters → Run test → Download results

### Regression Analysis
1. **Regresi Linear Berganda** menu
2. Select dependent & independent variables
3. Run model → Check assumptions → Interpret results

## 🆘 Quick Fixes

| Problem            | Solution                                       |
| ------------------ | ---------------------------------------------- |
| App won't start    | Run `source("global.R")` first                 |
| Missing packages   | Check internet connection, update R            |
| Data not loading   | Restart R session, re-run `source("global.R")` |
| PDF download fails | Install pandoc or use RStudio                  |
| Slow performance   | Close other R sessions, restart app            |

## 📊 Data Information

- **Source**: SUSENAS 2017 (BPS-Statistics Indonesia)
- **Coverage**: 514 Indonesian districts/cities
- **Variables**: 15+ social vulnerability indicators
- **File**: `data/sovi_data.csv` (auto-generated)

## 📥 Report Generation

### Individual Module Reports
Each module provides dedicated download options:

- **CSV Export**: Raw processed data (Manajemen Data)
- **PNG/JPG**: High-quality plot exports (Eksplorasi)
- **PDF Reports**: Comprehensive analysis reports (all modules)
- **Word Documents**: Editable interpretation documents
- **Individual Interpretations**: Module-specific insights

### Combined Reports (Beranda)
Access comprehensive reports covering all modules:

```r
# Available from Beranda menu:
# - Combined PDF Report: All modules in single document
# - Combined Word Report: Editable comprehensive analysis
```

### Security Features
Enhanced download handlers include:

- **Input Validation**: Sanitized filenames prevent path traversal
- **Error Handling**: Graceful fallbacks for failed operations
- **Resource Protection**: Safe rendering with timeouts
- **Data Validation**: Required input checks before processing

## 🎓 UAS Submission Checklist

- ✅ All features working correctly
- ✅ Documentation complete
- ✅ Validation test passed
- ✅ Screenshots/demo ready
- ✅ Code well-commented
- ✅ Error handling robust
- ✅ Security validation implemented
- ✅ Dynamic report generation functional

## 🔗 Quick Links

- **Launch**: `source("app.R")`
- **Test**: `source("validate_dashboard.R")`
- **Help**: See `USER_MANUAL.md`
- **Issues**: See `TROUBLESHOOTING.md`

---
**Ready for UAS Submission** ✅
