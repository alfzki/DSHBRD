# NusaStat Dashboard - Troubleshooting Guide

## Quick Start Checklist

### Prerequisites
- [ ] R version 4.0 or later installed
- [ ] RStudio (recommended but optional)
- [ ] Internet connection for package installation

### Installation Steps
1. **Clone/Download** the project to your local machine
2. **Navigate** to the Dashboard directory
3. **Install packages** by running: `source('global.R')`
4. **Launch app** by running: `source('app.R')`

## Common Issues and Solutions

### 1. Package Installation Errors

**Problem**: Error installing packages
```
Error: package 'xyz' is not available
```

**Solutions**:
- Update R to latest version
- Set CRAN mirror: `options(repos = "https://cran.rstudio.com/")`
- Install packages individually:
```r
install.packages(c("shiny", "shinydashboard", "dplyr", "ggplot2", "plotly"))
```

### 2. App Won't Launch

**Problem**: Error when running `source('app.R')`
```
Error: object 'dashboardPage' not found
```

**Solutions**:
- Ensure all packages are loaded: `source('global.R')`
- Check R working directory: `getwd()`
- Restart R session and try again

### 3. Data Loading Issues

**Problem**: "Data belum dimuat" error messages

**Solutions**:
- Check if `data/` folder exists
- Verify `sovi_data.csv` and `distance.csv` are present
- Regenerate data: delete CSV files and restart app

### 4. Statistics Function Errors

**Problem**: Error in statistical tests
```
Error in shapiro.test(): sample size must be between 3 and 5000
```

**Solutions**:
- Check if selected variable has enough data points
- Remove missing values from dataset
- For large datasets, app automatically samples 5000 points

### 5. Plot Rendering Issues

**Problem**: Plots don't display or show blank

**Solutions**:
- Ensure variable is selected
- Check if variable has numeric data
- Clear browser cache if using web interface
- Try different browser

### 6. Download Functionality Problems

**Problem**: PDF downloads fail
```
Error: pandoc not found
```

**Solutions**:
- Install pandoc: https://pandoc.org/installing.html
- Use RStudio which includes pandoc
- Alternative: install `tinytex` package

### 7. Memory Issues

**Problem**: App becomes slow or crashes with large data

**Solutions**:
- Close other R sessions
- Increase memory limit: `memory.limit(size = 8000)` (Windows)
- Use data sampling features in the app
- Restart R session periodically

## Performance Optimization Tips

### 1. Data Management
- Keep datasets under 100MB for optimal performance
- Use categorical variables with reasonable number of levels (<20)
- Clean data before import (remove missing values where appropriate)

### 2. Browser Optimization
- Use modern browsers (Chrome, Firefox, Edge)
- Clear browser cache regularly
- Close unnecessary browser tabs
- Disable browser extensions that might interfere

### 3. R Environment
- Use latest R version
- Keep packages updated: `update.packages()`
- Restart R session periodically: `.rs.restartR()` (RStudio)

## Feature-Specific Troubleshooting

### Beranda (Home Page)
- If metadata table doesn't load: refresh page
- If download fails: check write permissions in project folder

### Manajemen Data
- **Categorization fails**: Ensure selected variable is numeric
- **No variables shown**: Check if data is loaded properly
- **Categories have 0 observations**: Reduce number of categories

### Eksplorasi Data
- **Summary stats show NA**: Check for missing values in variable
- **Plot doesn't update**: Select different variable and re-select
- **Map doesn't show**: Requires spatial data (currently placeholder)

### Uji Asumsi
- **Normality test fails**: 
  - Check sample size (need 3-5000 observations)
  - Remove infinite or extreme outlier values
- **Homogeneity test error**: Ensure grouping variable has 2+ categories

### Statistical Tests
- **t-test errors**: 
  - One sample: check if variable has sufficient data
  - Two sample: ensure grouping variable has exactly 2 groups
- **ANOVA errors**: Each group must have at least 2 observations
- **Regression errors**: 
  - Remove perfect multicollinearity
  - Ensure dependent variable is numeric

## Advanced Troubleshooting

### 1. Enable Debugging
```r
options(shiny.error = browser)  # Interactive debugging
options(shiny.trace = TRUE)     # Detailed logging
```

### 2. Check Package Versions
```r
sessionInfo()  # Shows all loaded packages and versions
```

### 3. Test Individual Components
```r
# Test data loading
source('global.R')
data <- load_sovi_data()
head(data)

# Test specific function
source('R/server_modules.R')
```

### 4. Memory and Performance Monitoring
```r
# Check memory usage
pryr::mem_used()

# Profile performance
profvis::profvis({
  # Your code here
})
```

## Getting Help

### 1. Check Log Output
- Look at R console for error messages
- Check browser developer console (F12) for JavaScript errors

### 2. Minimal Reproducible Example
When reporting issues:
1. Specify your R version: `R.version.string`
2. List package versions: `packageVersion("shiny")`
3. Provide exact error message
4. Describe steps to reproduce

### 3. Common Error Patterns

**"Object not found"**: Usually means package not loaded or function not sourced
**"Subscript out of bounds"**: Check data dimensions and variable names
**"Non-numeric argument"**: Ensure variables are correct type for analysis
**"Cannot allocate memory"**: Reduce data size or restart R session

### 4. Performance Benchmarks
Normal loading times:
- App startup: 5-15 seconds
- Data loading: 1-3 seconds
- Statistical tests: 1-5 seconds
- PDF generation: 5-15 seconds

If operations take significantly longer, check system resources and network connection.

## Best Practices

### 1. Regular Maintenance
- Update R and packages monthly
- Clear R workspace before major analyses
- Save important results before experimenting

### 2. Data Preparation
- Always backup original data
- Document any data transformations
- Use consistent variable naming

### 3. Analysis Workflow
- Start with data exploration
- Check assumptions before tests
- Interpret results in context
- Document methodological choices

### 4. Sharing Results
- Export both raw results and interpretations
- Include methodology in reports
- Verify reproducibility of analyses

For additional support, refer to the package documentation and R help system:
```r
?shiny::runApp
help(package = "shiny")
```
