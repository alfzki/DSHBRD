#!/bin/bash
# ==============================================================================
# IMPROVED UNUSED CODE ANALYSIS FOR ALIVA DASHBOARD
# ==============================================================================

echo "=============================================="
echo "ALIVA Dashboard - Comprehensive Code Analysis"
echo "=============================================="
echo ""

# Create analysis directory
ANALYSIS_DIR="/tmp/code_analysis"
mkdir -p "$ANALYSIS_DIR"

echo "🔍 Step 1: Analyzing file structure..."

# List all R files with full paths
find . -name "*.R" -type f > "$ANALYSIS_DIR/all_files.txt"
TOTAL_FILES=$(cat "$ANALYSIS_DIR/all_files.txt" | wc -l)
echo "Found $TOTAL_FILES R files"

echo ""
echo "📋 All R files in the repository:"
cat "$ANALYSIS_DIR/all_files.txt" | sort

echo ""
echo "🔍 Step 2: Checking file references and usage..."

# Check which files are actually referenced
echo "Checking file usage patterns..."

# Function to check if a file is used
check_file_usage() {
    local file="$1"
    local basename_file=$(basename "$file")
    local name_without_ext="${basename_file%.R}"
    
    echo "Analyzing: $file"
    
    # Check if sourced directly
    local sourced=$(grep -r "source.*$basename_file" . --include="*.R" --exclude="$basename_file" 2>/dev/null | wc -l)
    
    # Check if referenced in load_modules.R
    local in_modules=0
    if [[ "$file" == *"modules/"* ]]; then
        if grep -q "$name_without_ext" "R/load_modules.R" 2>/dev/null; then
            in_modules=1
        fi
    fi
    
    # Check if it's a main file (app.R, global.R, etc.)
    local is_main=0
    if [[ "$basename_file" == "app.R" ]] || [[ "$basename_file" == "global.R" ]] || [[ "$basename_file" == "validate_dashboard.R" ]]; then
        is_main=1
    fi
    
    # Special check for launch_dashboard.R
    local is_launcher=0
    if [[ "$basename_file" == "launch_dashboard.R" ]]; then
        # Check if referenced in documentation or other files
        local doc_refs=$(grep -r "launch_dashboard" . --include="*.md" --include="*.txt" 2>/dev/null | wc -l)
        if [[ $doc_refs -gt 0 ]]; then
            is_launcher=1
        fi
    fi
    
    # Calculate usage score
    local total_usage=$((sourced + in_modules + is_main + is_launcher))
    
    echo "  - Sourced directly: $sourced times"
    echo "  - Referenced in modules: $in_modules"
    echo "  - Is main file: $is_main"
    echo "  - Is launcher with docs: $is_launcher"
    echo "  - Total usage score: $total_usage"
    
    if [[ $total_usage -eq 0 ]]; then
        echo "  ❌ POTENTIALLY UNUSED"
        echo "$file" >> "$ANALYSIS_DIR/unused_files.txt"
    else
        echo "  ✅ USED"
    fi
    echo ""
}

# Clear previous results
> "$ANALYSIS_DIR/unused_files.txt"

# Check each file
while IFS= read -r file; do
    check_file_usage "$file"
done < "$ANALYSIS_DIR/all_files.txt"

echo ""
echo "🔍 Step 3: Analyzing function usage within files..."

# Extract function definitions with better patterns
grep -r "^[[:space:]]*[a-zA-Z_][a-zA-Z0-9_.]*[[:space:]]*<-[[:space:]]*function" . --include="*.R" -n > "$ANALYSIS_DIR/function_defs_raw.txt"

# Clean function names
sed 's/.*:\([0-9]*\):[[:space:]]*\([a-zA-Z_][a-zA-Z0-9_.]*\).*/\2/' "$ANALYSIS_DIR/function_defs_raw.txt" | sort | uniq > "$ANALYSIS_DIR/function_names.txt"

echo "Found $(cat "$ANALYSIS_DIR/function_names.txt" | wc -l) function definitions"

# Check function usage
echo ""
echo "Checking function usage:"
> "$ANALYSIS_DIR/unused_functions.txt"

while IFS= read -r func; do
    # Count occurrences of function calls (exclude definitions)
    usage_count=$(grep -r "$func(" . --include="*.R" | grep -v "<-[[:space:]]*function" | wc -l)
    if [[ $usage_count -eq 0 ]]; then
        echo "  ❌ $func - not called"
        echo "$func" >> "$ANALYSIS_DIR/unused_functions.txt"
    else
        echo "  ✅ $func - called $usage_count times"
    fi
done < "$ANALYSIS_DIR/function_names.txt"

echo ""
echo "=============================================="
echo "FINAL ANALYSIS RESULTS"
echo "=============================================="

echo ""
echo "📊 SUMMARY:"
echo "  - Total R files: $TOTAL_FILES"
echo "  - Unused files: $(cat "$ANALYSIS_DIR/unused_files.txt" 2>/dev/null | wc -l)"
echo "  - Total functions: $(cat "$ANALYSIS_DIR/function_names.txt" | wc -l)"
echo "  - Unused functions: $(cat "$ANALYSIS_DIR/unused_functions.txt" 2>/dev/null | wc -l)"

echo ""
echo "❌ POTENTIALLY UNUSED FILES:"
if [[ -s "$ANALYSIS_DIR/unused_files.txt" ]]; then
    while IFS= read -r file; do
        echo "  - $file"
        # Provide context about why it might be unused
        if [[ "$file" == *"launch"* ]]; then
            echo "    💡 This appears to be a launcher script - verify if it's needed"
        elif [[ "$file" == *"validate"* ]]; then
            echo "    💡 This appears to be a validation script - might be used in CI/CD"
        fi
    done < "$ANALYSIS_DIR/unused_files.txt"
else
    echo "  ✅ No unused files detected!"
fi

echo ""
echo "⚠️  POTENTIALLY UNUSED FUNCTIONS:"
if [[ -s "$ANALYSIS_DIR/unused_functions.txt" ]]; then
    head -10 "$ANALYSIS_DIR/unused_functions.txt" | while IFS= read -r func; do
        echo "  - $func()"
    done
    total_unused_funcs=$(cat "$ANALYSIS_DIR/unused_functions.txt" | wc -l)
    if [[ $total_unused_funcs -gt 10 ]]; then
        echo "  ... and $((total_unused_funcs - 10)) more functions"
    fi
else
    echo "  ✅ No unused functions detected!"
fi

echo ""
echo "🔍 SPECIAL ANALYSIS:"

# Check specific files of interest
echo "Checking launch_dashboard.R usage:"
if [[ -f "launch_dashboard.R" ]]; then
    doc_mentions=$(grep -r "launch_dashboard" . --include="*.md" --include="*.txt" --include="*.R" --exclude="launch_dashboard.R" 2>/dev/null | wc -l)
    echo "  - Mentioned in documentation/code: $doc_mentions times"
    if [[ $doc_mentions -eq 0 ]]; then
        echo "  ❌ launch_dashboard.R appears to be completely unused"
        echo "  💡 Consider removing it as app.R seems to be the main entry point"
    else
        echo "  ✅ launch_dashboard.R has some references"
    fi
fi

echo ""
echo "Checking module consistency:"
MODULES_EXPECTED=8
MODULES_FOUND=$(find R/modules -name "*_ui.R" | wc -l)
echo "  - Expected modules: $MODULES_EXPECTED"
echo "  - Found module UI files: $MODULES_FOUND"
echo "  - Found module server files: $(find R/modules -name "*_server.R" | wc -l)"

if [[ $MODULES_FOUND -ne $MODULES_EXPECTED ]]; then
    echo "  ⚠️  Module count mismatch detected"
fi

# Generate final report
REPORT_FILE="comprehensive_unused_code_report.md"
cat > "$REPORT_FILE" << EOF
# Comprehensive Unused Code Analysis Report

**Generated:** $(date)
**Repository:** ALIVA Dashboard

## Executive Summary

- **Total R files analyzed:** $TOTAL_FILES
- **Potentially unused files:** $(cat "$ANALYSIS_DIR/unused_files.txt" 2>/dev/null | wc -l)
- **Total functions analyzed:** $(cat "$ANALYSIS_DIR/function_names.txt" | wc -l)
- **Potentially unused functions:** $(cat "$ANALYSIS_DIR/unused_functions.txt" 2>/dev/null | wc -l)

## Potentially Unused Files

EOF

if [[ -s "$ANALYSIS_DIR/unused_files.txt" ]]; then
    while IFS= read -r file; do
        echo "### \`$file\`" >> "$REPORT_FILE"
        echo "" >> "$REPORT_FILE"
        if [[ "$file" == *"launch"* ]]; then
            echo "- **Type:** Launcher script" >> "$REPORT_FILE"
            echo "- **Risk:** Low - might be used for deployment" >> "$REPORT_FILE"
        else
            echo "- **Type:** Unknown" >> "$REPORT_FILE"
            echo "- **Risk:** Medium - needs manual verification" >> "$REPORT_FILE"
        fi
        echo "" >> "$REPORT_FILE"
    done < "$ANALYSIS_DIR/unused_files.txt"
else
    echo "No unused files detected." >> "$REPORT_FILE"
fi

cat >> "$REPORT_FILE" << EOF

## Potentially Unused Functions

EOF

if [[ -s "$ANALYSIS_DIR/unused_functions.txt" ]]; then
    echo "The following functions are defined but never called:" >> "$REPORT_FILE"
    echo "" >> "$REPORT_FILE"
    while IFS= read -r func; do
        echo "- \`$func()\`" >> "$REPORT_FILE"
    done < "$ANALYSIS_DIR/unused_functions.txt"
else
    echo "No unused functions detected." >> "$REPORT_FILE"
fi

cat >> "$REPORT_FILE" << EOF

## Recommendations

1. **Immediate Actions:**
   - Manually verify each potentially unused file
   - Check if unused files are referenced in documentation or deployment scripts
   - Test the application after removing any code

2. **Safe Removals:**
   - Functions that are truly unused and not part of any API
   - Files that have no references anywhere

3. **Proceed with Caution:**
   - Launcher scripts might be used in deployment
   - Validation scripts might be used in CI/CD pipelines

## Next Steps

1. Review each identified item manually
2. Create a backup before removing anything
3. Test thoroughly after any removals
4. Update documentation accordingly

EOF

echo ""
echo "📄 Comprehensive report saved to: $REPORT_FILE"
echo ""
echo "✅ Analysis complete!"