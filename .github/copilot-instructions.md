<!-- Use this file to provide workspace-specific custom instructions to Copilot. For more details, visit https://code.visualstudio.com/docs/copilot/copilot-customization#_use-a-githubcopilotinstructionsmd-file -->

# NusaStat Dashboard - Copilot Instructions

## Project Overview

This is an R Shiny dashboard project called "NusaStat" for Indonesian social vulnerability statistical analysis. The dashboard provides comprehensive statistical analysis capabilities including data management, exploratory analysis, assumption testing, inferential statistics, and multiple linear regression.

## Code Style and Standards

### R Coding Standards

- Use camelCase for variable names and snake_case for function names
- Include comprehensive comments for complex statistical procedures
- Use modular approach with separate files for UI and server logic
- Implement proper error handling with try-catch blocks
- Validate data inputs before processing
- Use reactive programming patterns appropriately

### Shiny Best Practices

- Use Shiny modules for reusable components
- Implement proper reactive patterns (reactive, observe, observeEvent)
- Use namespacing (NS) for module identification
- Implement proper session management
- Use validate() and req() for input validation
- Handle user notifications appropriately

### Statistical Analysis Standards

- Always validate statistical assumptions before applying tests
- Provide clear interpretations of statistical results
- Include proper hypothesis statements (H0 and H1)
- Use appropriate significance levels (α = 0.05 default)
- Implement proper degrees of freedom calculations
- Provide confidence intervals where applicable

## File Structure Guidelines

### Main Files

- `app.R`: Main application file that sources modules and defines UI/server
- `global.R`: Global configurations, package loading, and utility functions
- `R/ui_modules.R`: All UI module definitions
- `R/server_modules.R`: Main server module implementations
- `R/additional_server_modules.R`: Additional server modules for statistical tests

### Data Management

- Store data files in `data/` directory
- Use CSV format for data files
- Implement data validation functions
- Create sample data when real data is not available
- Use proper data types and handle missing values

### Styling and Assets

- Custom CSS in `www/custom.css`
- Use modern, responsive design principles
- Implement consistent color schemes
- Ensure accessibility compliance
- Use proper typography and spacing

## Statistical Implementation Guidelines

### Data Exploration

- Implement descriptive statistics with proper formatting
- Create interactive visualizations using plotly
- Provide automated interpretations of results
- Include distribution analysis and outlier detection

### Hypothesis Testing

- Implement proper test selection based on data characteristics
- Check assumptions before applying parametric tests
- Provide non-parametric alternatives when needed
- Include effect size calculations where appropriate
- Generate automated interpretations with decision rules

### Regression Analysis

- Implement comprehensive assumption testing (VIF, Breusch-Pagan, Shapiro-Wilk)
- Provide model equation generation
- Include residual analysis and diagnostic plots
- Implement proper variable selection procedures
- Generate comprehensive model interpretations

## User Interface Guidelines

### Dashboard Design

- Use shinydashboard framework for consistent layout
- Implement responsive design for different screen sizes
- Use appropriate box layouts and status colors
- Include proper navigation and menu structure
- Implement loading states and progress indicators

### User Experience

- Provide clear instructions and help text
- Implement input validation with user feedback
- Use appropriate input controls for different data types
- Include download capabilities for all results
- Implement proper error messages and notifications

## Download and Reporting

### Report Generation

- Use R Markdown for dynamic report generation
- Include both results and interpretations in reports
- Support multiple output formats (PDF, Word, HTML)
- Implement proper formatting and styling
- Include metadata and timestamp information

### Data Export

- Support CSV export for processed data
- Include proper column names and data types
- Implement data validation before export
- Provide metadata files where appropriate

## Testing and Validation

### Data Validation

- Implement input validation for all user inputs
- Check data types and ranges
- Validate statistical assumptions
- Handle missing data appropriately
- Provide informative error messages

### Statistical Validation

- Verify statistical test implementations
- Check calculation accuracy
- Validate interpretation logic
- Test edge cases and boundary conditions
- Implement proper significance testing

## Performance Considerations

### Data Processing

- Use efficient data manipulation with dplyr
- Implement proper memory management
- Use sampling for large datasets when appropriate
- Cache computed results where beneficial
- Optimize database queries if applicable

### UI Performance

- Use reactive programming efficiently
- Implement proper event handling
- Minimize unnecessary computations
- Use appropriate plot rendering methods
- Implement lazy loading where beneficial

## Security and Privacy

### Data Security

- Validate all user inputs to prevent injection attacks
- Use parameterized queries for database operations
- Implement proper file upload validation
- Handle sensitive data appropriately
- Use secure session management

### Privacy Considerations

- Avoid storing sensitive user data
- Implement proper data anonymization
- Use secure connections for data transmission
- Comply with data protection regulations
- Provide clear data usage policies

## Documentation Standards

### Code Documentation

- Include comprehensive function documentation
- Use roxygen2 format for function documentation
- Document complex statistical procedures
- Include usage examples for key functions
- Maintain up-to-date README files

### User Documentation

- Provide clear user instructions
- Include statistical methodology explanations
- Document data requirements and formats
- Provide troubleshooting guidance
- Include interpretation guidelines

## When providing suggestions or generating code for this project:

1. **Statistical Accuracy**: Ensure all statistical implementations are mathematically correct
2. **User-Friendly Interface**: Prioritize intuitive and accessible design
3. **Comprehensive Validation**: Include proper input validation and error handling
4. **Clear Interpretations**: Provide automated, clear explanations of statistical results
5. **Modular Architecture**: Maintain the modular structure for maintainability
6. **Performance Optimization**: Consider performance implications of suggestions
7. **Documentation**: Include appropriate comments and documentation
8. **Indonesian Context**: Consider the Indonesian statistical context and terminology where appropriate

## Common Patterns to Follow

- Use `validate_data()` function before processing data
- Implement `interpret_p_value()` for consistent interpretation
- Use `format_number()` for consistent number formatting
- Follow the box layout pattern for UI consistency
- Implement download handlers for all analysis results
- Use proper reactive patterns for data flow
- Include progress notifications for user feedback
