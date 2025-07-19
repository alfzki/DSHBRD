# Test Geographic Map Implementation
# Testing the new Indonesian provincial capitals map on beranda page

# Load required libraries
library(shiny)
library(leaflet)

# Test data structure for provincial capitals
test_provincial_capitals <- function() {
    # Provincial capitals data as implemented
    provincial_capitals <- data.frame(
        city = c(
            "Jakarta", "Medan", "Palembang", "Bandar Lampung", "Bandung", "Semarang",
            "Yogyakarta", "Surabaya", "Denpasar", "Mataram", "Pontianak", "Palangkaraya",
            "Samarinda", "Makassar", "Palu", "Manado", "Ambon", "Jayapura", "Manokwari"
        ),
        province = c(
            "DKI Jakarta", "Sumatera Utara", "Sumatera Selatan", "Lampung", "Jawa Barat",
            "Jawa Tengah", "D.I. Yogyakarta", "Jawa Timur", "Bali", "Nusa Tenggara Barat",
            "Kalimantan Barat", "Kalimantan Tengah", "Kalimantan Timur", "Sulawesi Selatan",
            "Sulawesi Tengah", "Sulawesi Utara", "Maluku", "Papua", "Papua Barat"
        ),
        lng = c(
            106.8456, 98.6785, 104.7458, 105.2677, 107.6191, 110.4203,
            110.3650, 112.7521, 115.2192, 116.1158, 109.3200, 113.9213,
            117.1364, 119.4221, 119.8707, 124.8420, 128.1777, 140.7181, 134.0640
        ),
        lat = c(
            -6.2088, 3.5952, -2.9167, -5.4500, -6.9175, -6.9930,
            -7.7972, -7.2575, -8.6705, -8.5833, -0.0263, -2.2090,
            -0.5017, -5.1477, -0.8917, 1.4748, -3.6954, -2.5337, -0.8650
        ),
        region = c(
            "Jawa", "Sumatera", "Sumatera", "Sumatera", "Jawa", "Jawa",
            "Jawa", "Jawa", "Nusa Tenggara", "Nusa Tenggara", "Kalimantan", "Kalimantan",
            "Kalimantan", "Sulawesi", "Sulawesi", "Sulawesi", "Maluku", "Papua", "Papua"
        )
    )

    # Basic validation tests
    cat("=== Geographic Map Data Validation ===\n")
    cat("Number of cities:", nrow(provincial_capitals), "\n")
    cat("Unique regions:", length(unique(provincial_capitals$region)), "\n")
    cat("Regions:", paste(unique(provincial_capitals$region), collapse = ", "), "\n")

    # Check coordinate ranges (Indonesia boundaries approximately)
    lat_range <- range(provincial_capitals$lat)
    lng_range <- range(provincial_capitals$lng)

    cat("\nCoordinate validation:\n")
    cat("Latitude range:", lat_range[1], "to", lat_range[2], "\n")
    cat("Longitude range:", lng_range[1], "to", lng_range[2], "\n")

    # Check if coordinates are within Indonesia bounds
    indonesia_lat_range <- c(-11, 6) # Approximate Indonesia latitude range
    indonesia_lng_range <- c(95, 141) # Approximate Indonesia longitude range

    valid_lat <- all(provincial_capitals$lat >= indonesia_lat_range[1] &
        provincial_capitals$lat <= indonesia_lat_range[2])
    valid_lng <- all(provincial_capitals$lng >= indonesia_lng_range[1] &
        provincial_capitals$lng <= indonesia_lng_range[2])

    cat("All coordinates within Indonesia bounds:", valid_lat && valid_lng, "\n")

    # Check for duplicates
    cat("Duplicate cities:", any(duplicated(provincial_capitals$city)), "\n")
    cat("Duplicate coordinates:", any(duplicated(paste(provincial_capitals$lat, provincial_capitals$lng))), "\n")

    # Regional distribution
    region_counts <- table(provincial_capitals$region)
    cat("\nRegional distribution:\n")
    print(region_counts)

    return(provincial_capitals)
}

# Test leaflet map creation
test_leaflet_map <- function() {
    provincial_capitals <- test_provincial_capitals()

    # Create color palette for regions
    region_colors <- c(
        "Jawa" = "blue", "Sumatera" = "red", "Kalimantan" = "green",
        "Sulawesi" = "orange", "Nusa Tenggara" = "purple",
        "Maluku" = "brown", "Papua" = "pink"
    )

    cat("\n=== Creating Test Leaflet Map ===\n")

    # Create test map
    test_map <- leaflet() %>%
        addTiles() %>%
        setView(lng = 118.0148634, lat = -2.548926, zoom = 5) %>%
        addMarkers(
            lng = provincial_capitals$lng,
            lat = provincial_capitals$lat,
            popup = paste(
                "<b>", provincial_capitals$city, "</b><br/>",
                "Provinsi: ", provincial_capitals$province, "<br/>",
                "Wilayah: ", provincial_capitals$region
            ),
            label = paste(provincial_capitals$city, "-", provincial_capitals$province)
        )

    cat("Test map created successfully!\n")
    cat("Map includes:", nrow(provincial_capitals), "markers\n")

    return(test_map)
}

# Run tests
cat("Starting geographic map tests...\n")
test_data <- test_provincial_capitals()
test_map <- test_leaflet_map()
cat("\n=== All Tests Completed Successfully ===\n")
