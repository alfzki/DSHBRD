#!/bin/bash
# ==============================================================================
# UNUSED CODE DETECTION SCRIPT FOR ALIVA DASHBOARD
# ==============================================================================
#
# Purpose: Detect unused code, dead files, and unreferenced functions
# Author: GitHub Copilot AI Agent
# Created: July 2025
#
# This script analyzes the ALIVA dashboard codebase to identify:
# - Files that are never sourced or referenced
# - Functions that are defined but never called
# - Dead code patterns
# - Unused assets and resources
# ==============================================================================

echo "=============================================="
echo "ALIVA Dashboard - Unused Code Detection"
echo "=============================================="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Create temporary files for analysis
TEMP_DIR="/tmp/unused_code_analysis"
mkdir -p "$TEMP_DIR"

ALL_R_FILES="$TEMP_DIR/all_r_files.txt"
SOURCED_FILES="$TEMP_DIR/sourced_files.txt"
REFERENCED_FILES="$TEMP_DIR/referenced_files.txt"
FUNCTION_DEFINITIONS="$TEMP_DIR/function_definitions.txt"
FUNCTION_CALLS="$TEMP_DIR/function_calls.txt"
UNUSED_FILES="$TEMP_DIR/unused_files.txt"
UNUSED_FUNCTIONS="$TEMP_DIR/unused_functions.txt"

echo -e "${BLUE}[STEP 1]${NC} Analyzing file structure..."

# Find all R files
find . -name "*.R" -type f | grep -v "^\./" | sed 's|^\./||' > "$ALL_R_FILES"
echo "Found $(wc -l < "$ALL_R_FILES") R files"

# Find files that are sourced
echo -e "${BLUE}[STEP 2]${NC} Detecting sourced files..."
grep -r "source(" . --include="*.R" | grep -o '"[^"]*\.R"' | sed 's/"//g' > "$SOURCED_FILES"
grep -r "source(" . --include="*.R" | grep -o "'[^']*\.R'" | sed "s/'//g" >> "$SOURCED_FILES"

# Find files referenced in any way (not just sourced)
echo -e "${BLUE}[STEP 3]${NC} Detecting referenced files..."
while IFS= read -r file; do
    filename=$(basename "$file")
    filename_no_ext="${filename%.R}"
    # Search for any reference to this file
    if grep -r "$filename_no_ext" . --include="*.R" --include="*.md" --include="*.txt" >/dev/null 2>&1; then
        echo "$file" >> "$REFERENCED_FILES"
    fi
done < "$ALL_R_FILES"

# Find function definitions
echo -e "${BLUE}[STEP 4]${NC} Extracting function definitions..."
grep -r "^[[:space:]]*[a-zA-Z_][a-zA-Z0-9_]*[[:space:]]*<-[[:space:]]*function" . --include="*.R" | \
    sed 's/.*:\([a-zA-Z_][a-zA-Z0-9_]*\)[[:space:]]*<-.*/\1/' > "$FUNCTION_DEFINITIONS"

# Find function calls
echo -e "${BLUE}[STEP 5]${NC} Extracting function calls..."
grep -r "[a-zA-Z_][a-zA-Z0-9_]*(" . --include="*.R" | \
    grep -o "[a-zA-Z_][a-zA-Z0-9_]*(" | \
    sed 's/($//' | sort | uniq > "$FUNCTION_CALLS"

echo -e "${BLUE}[STEP 6]${NC} Analyzing unused files..."

# Find unused files
sort "$ALL_R_FILES" > "$TEMP_DIR/all_sorted.txt"
sort "$SOURCED_FILES" "$REFERENCED_FILES" | uniq > "$TEMP_DIR/used_sorted.txt"
comm -23 "$TEMP_DIR/all_sorted.txt" "$TEMP_DIR/used_sorted.txt" > "$UNUSED_FILES"

echo -e "${BLUE}[STEP 7]${NC} Analyzing unused functions..."

# Find unused functions (defined but never called)
sort "$FUNCTION_DEFINITIONS" > "$TEMP_DIR/functions_def_sorted.txt"
sort "$FUNCTION_CALLS" > "$TEMP_DIR/functions_call_sorted.txt"
comm -23 "$TEMP_DIR/functions_def_sorted.txt" "$TEMP_DIR/functions_call_sorted.txt" > "$UNUSED_FUNCTIONS"

echo ""
echo "=============================================="
echo "ANALYSIS RESULTS"
echo "=============================================="
echo ""

# Report unused files
echo -e "${RED}[POTENTIALLY UNUSED FILES]${NC}"
if [ -s "$UNUSED_FILES" ]; then
    while IFS= read -r file; do
        echo "  ❌ $file"
        # Check if file might be a launcher or utility
        if [[ "$file" == *"launch"* ]] || [[ "$file" == *"run"* ]] || [[ "$file" == *"start"* ]]; then
            echo "      💡 Might be a launcher/utility script"
        fi
    done < "$UNUSED_FILES"
else
    echo -e "  ${GREEN}✅ No unused files detected${NC}"
fi

echo ""

# Report unused functions
echo -e "${YELLOW}[POTENTIALLY UNUSED FUNCTIONS]${NC}"
if [ -s "$UNUSED_FUNCTIONS" ]; then
    head -20 "$UNUSED_FUNCTIONS" | while IFS= read -r func; do
        echo "  ⚠️  $func"
    done
    total_unused=$(wc -l < "$UNUSED_FUNCTIONS")
    if [ "$total_unused" -gt 20 ]; then
        echo "  ... and $((total_unused - 20)) more functions"
    fi
else
    echo -e "  ${GREEN}✅ No unused functions detected${NC}"
fi

echo ""

# Additional analysis
echo -e "${BLUE}[DETAILED ANALYSIS]${NC}"

echo "📊 File Statistics:"
echo "  - Total R files: $(wc -l < "$ALL_R_FILES")"
echo "  - Sourced files: $(sort "$SOURCED_FILES" | uniq | wc -l)"
echo "  - Referenced files: $(sort "$REFERENCED_FILES" | uniq | wc -l)"
echo "  - Potentially unused files: $(wc -l < "$UNUSED_FILES")"

echo ""
echo "🔍 Function Statistics:"
echo "  - Total function definitions: $(wc -l < "$FUNCTION_DEFINITIONS")"
echo "  - Total function calls: $(wc -l < "$FUNCTION_CALLS")"
echo "  - Potentially unused functions: $(wc -l < "$UNUSED_FUNCTIONS")"

echo ""

# Special checks for specific files
echo -e "${BLUE}[SPECIAL FILE ANALYSIS]${NC}"

# Check launch_dashboard.R specifically
if [ -f "launch_dashboard.R" ]; then
    echo "🔍 Checking launch_dashboard.R:"
    if ! grep -r "launch_dashboard" . --include="*.R" --include="*.md" --exclude="launch_dashboard.R" >/dev/null 2>&1; then
        echo -e "  ${RED}❌ launch_dashboard.R appears to be unused${NC}"
        echo "      - No references found in other files"
        echo "      - Might be redundant with app.R"
    else
        echo -e "  ${GREEN}✅ launch_dashboard.R is referenced${NC}"
    fi
fi

# Check for module consistency
echo ""
echo "🔍 Module Consistency Check:"
MODULES_DIR="R/modules"
if [ -d "$MODULES_DIR" ]; then
    for module_dir in "$MODULES_DIR"/*/; do
        if [ -d "$module_dir" ]; then
            module_name=$(basename "$module_dir")
            ui_file="${module_dir}${module_name}_ui.R"
            server_file="${module_dir}${module_name}_server.R"
            
            if [ ! -f "$ui_file" ]; then
                echo -e "  ${RED}❌ Missing UI file for module: $module_name${NC}"
            fi
            
            if [ ! -f "$server_file" ]; then
                echo -e "  ${RED}❌ Missing server file for module: $module_name${NC}"
            fi
            
            # Check if module is registered in load_modules.R
            if ! grep -q "$module_name" "R/load_modules.R" 2>/dev/null; then
                echo -e "  ${YELLOW}⚠️  Module $module_name not found in load_modules.R${NC}"
            fi
        fi
    done
fi

echo ""
echo "=============================================="
echo "RECOMMENDATIONS"
echo "=============================================="
echo ""

if [ -s "$UNUSED_FILES" ]; then
    echo -e "${YELLOW}📋 Unused Files Recommendations:${NC}"
    while IFS= read -r file; do
        echo "  1. Review $file for actual usage"
        echo "     - Check if it's a standalone utility"
        echo "     - Verify it's not used in documentation or scripts"
        echo "     - Consider removing if truly unused"
        echo ""
    done < "$UNUSED_FILES"
fi

if [ -s "$UNUSED_FUNCTIONS" ]; then
    echo -e "${YELLOW}📋 Unused Functions Recommendations:${NC}"
    echo "  1. Review potentially unused functions"
    echo "  2. Check if they're used dynamically (do.call, etc.)"
    echo "  3. Consider if they're part of public API"
    echo "  4. Remove or document if they're truly unused"
    echo ""
fi

echo "🔍 Next Steps:"
echo "  1. Manually verify each identified unused file"
echo "  2. Test the application after removing unused code"
echo "  3. Update documentation if needed"
echo "  4. Consider adding automated unused code detection to CI/CD"

echo ""
echo -e "${GREEN}Analysis complete! Check the detailed reports above.${NC}"

# Generate a summary report file
REPORT_FILE="unused_code_report.md"
cat > "$REPORT_FILE" << EOF
# Unused Code Analysis Report

Generated: $(date)

## Summary

- **Total R files:** $(wc -l < "$ALL_R_FILES")
- **Potentially unused files:** $(wc -l < "$UNUSED_FILES")
- **Potentially unused functions:** $(wc -l < "$UNUSED_FUNCTIONS")

## Potentially Unused Files

EOF

if [ -s "$UNUSED_FILES" ]; then
    while IFS= read -r file; do
        echo "- \`$file\`" >> "$REPORT_FILE"
    done < "$UNUSED_FILES"
else
    echo "- None detected" >> "$REPORT_FILE"
fi

cat >> "$REPORT_FILE" << EOF

## Potentially Unused Functions

EOF

if [ -s "$UNUSED_FUNCTIONS" ]; then
    head -20 "$UNUSED_FUNCTIONS" | while IFS= read -r func; do
        echo "- \`$func()\`" >> "$REPORT_FILE"
    done
    total_unused=$(wc -l < "$UNUSED_FUNCTIONS")
    if [ "$total_unused" -gt 20 ]; then
        echo "- ... and $((total_unused - 20)) more functions" >> "$REPORT_FILE"
    fi
else
    echo "- None detected" >> "$REPORT_FILE"
fi

cat >> "$REPORT_FILE" << EOF

## Recommendations

1. **Manual Review Required:** Each identified item needs manual verification
2. **Testing:** Test the application after any removals
3. **Documentation:** Update documentation if code is removed
4. **CI/CD Integration:** Consider automating this analysis

## Detailed Analysis Files

The following temporary files contain detailed analysis:
- Function definitions: $FUNCTION_DEFINITIONS
- Function calls: $FUNCTION_CALLS
- All R files: $ALL_R_FILES
- Sourced files: $SOURCED_FILES

EOF

echo ""
echo -e "${GREEN}📄 Detailed report saved to: $REPORT_FILE${NC}"

# Cleanup option
echo ""
read -p "Do you want to clean up temporary analysis files? (y/n): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    rm -rf "$TEMP_DIR"
    echo -e "${GREEN}✅ Temporary files cleaned up${NC}"
else
    echo -e "${YELLOW}📁 Temporary files kept in: $TEMP_DIR${NC}"
fi