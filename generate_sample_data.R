# Sample Data Generation Script for NusaStat Dashboard
# This script creates realistic sample data for demonstration purposes

# Load required libraries
library(dplyr)
library(readr)

# Set seed for reproducibility
set.seed(123)

# Number of observations (Indonesian districts/regencies)
n <- 514

# Generate SOVI (Social Vulnerability Index) data
cat("Generating SOVI data...\n")

sovi_data <- data.frame(
    # Administrative information
    kode_wilayah = sprintf("%04d", 1:n),
    nama_wilayah = paste("Kabupaten/Kota", 1:n),
    provinsi = rep(paste("Provinsi", LETTERS[1:34]), length.out = n),

    # Demographic indicators
    kepadatan_penduduk = pmax(50, round(rnorm(n, 500, 300), 0)),
    rasio_ketergantungan = pmax(20, pmin(80, round(rnorm(n, 45, 12), 2))),
    persentase_lansia = pmax(2, pmin(20, round(rnorm(n, 8, 3), 2))),
    persentase_balita = pmax(3, pmin(15, round(rnorm(n, 7, 2.5), 2))),
    persentase_disabilitas = pmax(0.5, pmin(10, round(rnorm(n, 3, 1.5), 2))),

    # Education indicators
    persentase_buta_huruf = pmax(0, pmin(25, round(rnorm(n, 5, 3), 2))),
    indeks_pendidikan = pmax(40, pmin(95, round(rnorm(n, 75, 10), 2))),

    # Economic indicators
    persentase_kemiskinan = pmax(1, pmin(35, round(rnorm(n, 12, 6), 2))),
    persentase_pengangguran = pmax(1, pmin(25, round(rnorm(n, 8, 4), 2))),
    indeks_ekonomi = pmax(30, pmin(90, round(rnorm(n, 65, 15), 2))),

    # Health indicators
    indeks_kesehatan = pmax(40, pmin(95, round(rnorm(n, 70, 12), 2))),

    # Infrastructure indicators
    akses_listrik = pmax(60, pmin(100, round(rnorm(n, 95, 8), 2))),
    akses_air_bersih = pmax(40, pmin(100, round(rnorm(n, 85, 15), 2))),
    akses_sanitasi = pmax(20, pmin(100, round(rnorm(n, 80, 18), 2))),

    # Composite vulnerability score
    sovi_score = pmax(0, pmin(100, round(rnorm(n, 50, 18), 2)))
)

# Create vulnerability level categories
sovi_data$vulnerability_level <- cut(sovi_data$sovi_score,
    breaks = c(0, 30, 60, 100),
    labels = c("Rendah", "Sedang", "Tinggi"),
    include.lowest = TRUE
)

# Generate distance data
cat("Generating distance data...\n")

distance_data <- data.frame(
    kode_wilayah = sprintf("%04d", 1:n),
    jarak_ke_ibukota_provinsi = pmax(5, round(rnorm(n, 150, 100), 0)),
    jarak_ke_pusat_kesehatan = pmax(1, round(rnorm(n, 25, 20), 0)),
    jarak_ke_pusat_pendidikan = pmax(1, round(rnorm(n, 20, 15), 0)),
    jarak_ke_pusat_ekonomi = pmax(1, round(rnorm(n, 35, 25), 0)),
    waktu_tempuh_ibukota = pmax(0.5, round(rnorm(n, 3, 2), 1))
)

# Add some realistic regional variations
regions <- c("Sumatera", "Jawa", "Kalimantan", "Sulawesi", "Papua", "Nusa Tenggara", "Maluku")
region_assignment <- sample(regions, n,
    replace = TRUE,
    prob = c(0.2, 0.3, 0.15, 0.15, 0.08, 0.08, 0.04)
)

# Adjust values based on regional characteristics
for (i in 1:n) {
    region <- region_assignment[i]

    # Regional adjustments for development indicators
    if (region == "Jawa") {
        # More developed region
        sovi_data$indeks_pendidikan[i] <- pmin(95, sovi_data$indeks_pendidikan[i] + 10)
        sovi_data$indeks_ekonomi[i] <- pmin(90, sovi_data$indeks_ekonomi[i] + 15)
        sovi_data$akses_listrik[i] <- pmin(100, sovi_data$akses_listrik[i] + 5)
        sovi_data$persentase_kemiskinan[i] <- pmax(1, sovi_data$persentase_kemiskinan[i] - 5)
    } else if (region %in% c("Papua", "Maluku")) {
        # Less developed regions
        sovi_data$indeks_pendidikan[i] <- pmax(40, sovi_data$indeks_pendidikan[i] - 15)
        sovi_data$indeks_ekonomi[i] <- pmax(30, sovi_data$indeks_ekonomi[i] - 20)
        sovi_data$akses_listrik[i] <- pmax(60, sovi_data$akses_listrik[i] - 15)
        sovi_data$persentase_kemiskinan[i] <- pmin(35, sovi_data$persentase_kemiskinan[i] + 10)
        distance_data$jarak_ke_ibukota_provinsi[i] <- distance_data$jarak_ke_ibukota_provinsi[i] + 100
    }
}

# Add region information to sovi_data
sovi_data$region <- region_assignment

# Recalculate SOVI score based on adjusted indicators
sovi_data$sovi_score <- with(sovi_data, {
    # Normalize indicators (higher values = higher vulnerability)
    norm_poverty <- (persentase_kemiskinan - min(persentase_kemiskinan)) /
        (max(persentase_kemiskinan) - min(persentase_kemiskinan)) * 100

    norm_unemployment <- (persentase_pengangguran - min(persentase_pengangguran)) /
        (max(persentase_pengangguran) - min(persentase_pengangguran)) * 100

    norm_illiteracy <- (persentase_buta_huruf - min(persentase_buta_huruf)) /
        (max(persentase_buta_huruf) - min(persentase_buta_huruf)) * 100

    # Reverse indicators (lower values = higher vulnerability)
    norm_education <- 100 - ((indeks_pendidikan - min(indeks_pendidikan)) /
        (max(indeks_pendidikan) - min(indeks_pendidikan)) * 100)

    norm_health <- 100 - ((indeks_kesehatan - min(indeks_kesehatan)) /
        (max(indeks_kesehatan) - min(indeks_kesehatan)) * 100)

    norm_economy <- 100 - ((indeks_ekonomi - min(indeks_ekonomi)) /
        (max(indeks_ekonomi) - min(indeks_ekonomi)) * 100)

    norm_electricity <- 100 - ((akses_listrik - min(akses_listrik)) /
        (max(akses_listrik) - min(akses_listrik)) * 100)

    norm_water <- 100 - ((akses_air_bersih - min(akses_air_bersih)) /
        (max(akses_air_bersih) - min(akses_air_bersih)) * 100)

    # Calculate composite score
    composite <- (norm_poverty + norm_unemployment + norm_illiteracy +
        norm_education + norm_health + norm_economy +
        norm_electricity + norm_water) / 8

    round(pmax(0, pmin(100, composite)), 2)
})

# Recalculate vulnerability levels
sovi_data$vulnerability_level <- cut(sovi_data$sovi_score,
    breaks = c(0, 33, 66, 100),
    labels = c("Rendah", "Sedang", "Tinggi"),
    include.lowest = TRUE
)

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
    dir.create("data")
}

# Save data files
cat("Saving data files...\n")
write_csv(sovi_data, "data/sovi_data.csv")
write_csv(distance_data, "data/distance.csv")

# Create data summary
cat("\nData Generation Summary:\n")
cat("========================\n")
cat("SOVI Data:\n")
cat("- Observations:", nrow(sovi_data), "\n")
cat("- Variables:", ncol(sovi_data), "\n")
cat("- Regions represented:", length(unique(sovi_data$region)), "\n")
cat("- Vulnerability levels:\n")
print(table(sovi_data$vulnerability_level))

cat("\nDistance Data:\n")
cat("- Observations:", nrow(distance_data), "\n")
cat("- Variables:", ncol(distance_data), "\n")

cat("\nNumeric Variables Summary (SOVI Data):\n")
numeric_vars <- sovi_data %>% select_if(is.numeric)
for (var in names(numeric_vars)) {
    var_summary <- summary(numeric_vars[[var]])
    cat("\n", var, ":\n")
    cat(
        "  Min:", var_summary[1], "  Max:", var_summary[6],
        "  Mean:", round(var_summary[4], 2), "\n"
    )
}

cat("\nSample data generation completed successfully!\n")
cat("Files saved:\n")
cat("- data/sovi_data.csv\n")
cat("- data/distance.csv\n")
