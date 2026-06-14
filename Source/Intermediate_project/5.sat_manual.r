# ==============================================================================
# 1. Load required packages
# ==============================================================================

#install.packages('luna', repos='https://rspatial.r-universe.dev')
library(luna)
library(MODIStsp)

#install.packages("appeears")

library(appeears)
library(terra)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)

# ------------------------------------------------------------------------------
# Optional installation
# install.packages(c("appeears", "terra", "sf", "rnaturalearth", "ggplot2", "dplyr"))
# ------------------------------------------------------------------------------


# ==============================================================================
# 2. Explore available MODIS products

# List all products available through AppEEARS
products <- rs_products()

# Display the first rows
head(products)


getProducts("^MOD|^MYD|^MCD")

#MOD = Terra satellite products

#MYD = Aqua satellite products

#MCD = Combined products (Terra + Aqua)

MODIStsp_get_prodlayers("M*D13Q1")

product <- "MOD09A1" #surface spectral reflectance of Terra
#product <- "MOD13Q1" # NDVI

productInfo(product)

# ==============================================================================
# 4. Export the Canada polygon for manual upload in AppEEARS
# ==============================================================================
# This file can be uploaded directly in the AppEEARS web interface
# when creating an area request.

canada_sf <- ne_countries(
  scale = "medium",
  country = "Canada",
  returnclass = "sf"
)

dir.create(".Data", showWarnings = FALSE)

st_write(
  canada_sf,
  "./Data/canada.geojson",
  delete_dsn = TRUE
)

plot(st_geometry(canada_sf), col = "lightgray", main = "Canada")

# ------------------------------------------------------------------------------
# MANUAL STEP IN APP EEARS
# ------------------------------------------------------------------------------
# 1. Open the AppEEARS website
# 2. Create an AREA request
# 3. Upload the file: .data/canada.geojson
# 4. Select product: MOD13Q1.061
# 5. Select layer: NDVI
# 6. Select the desired date range
# 7. Choose GeoTIFF as output format if available
# 8. Submit the task
# 9. Download the resulting NDVI raster manually
# 10. Save it in the folder: .data/appeears_manual_download
# ------------------------------------------------------------------------------


# ==============================================================================
# 5. Read the manually downloaded NDVI raster
# ==============================================================================
manual_path <- "./Data"

# List all tif files in the folder
manual_tif <- list.files(
  manual_path,
  pattern = "\\.tif",
  full.names = TRUE,
  recursive = TRUE
)

print(manual_tif)

# Read the first raster
ndvi_raster <- rast(manual_tif[1])

# Check raster information
print(ndvi_raster)

# Checker que le fichier existe
# print(getwd())
# dir("Data") # Vérifie si le dossier Data existe et contient le fichier


# Plot the raster and save
dir.create("Outputs/sat_manual_plots", recursive = TRUE, showWarnings = FALSE)

png("Outputs/sat_manual_plots/5.1_NDVI_Canada.png",
    width = 2000,
    height = 1500,
    res = 300)

plot(ndvi_raster, main = "Manually downloaded NDVI raster")

dev.off()

# ==============================================================================
# 6. Clip the raster to the exact Canada border
# ==============================================================================
canada_vect <- vect(canada_sf)

# Reproject the Switzerland polygon to the raster CRS
canada_vect <- project(canada_vect, crs(ndvi_raster))

# Crop and mask
ndvi_canada <- crop(ndvi_raster, canada_vect)
ndvi_canada <- mask(ndvi_canada, canada_vect)

# Plot the clipped raster
x11()
plot(ndvi_canada, main = "NDVI raster clipped to Canada")
plot(canada_vect, add = TRUE, border = "black", lwd = 1)


# ==============================================================================
# 7. Convert the sampling table to spatial points
# ==============================================================================

points_vect <- vect(
  matrix_full_clim_eco_elev,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

# Reproject the points to the raster CRS
points_vect <- project(points_vect, crs(ndvi_canada))

# Plot the points on top of the raster
png("Outputs/sat_manual_plots/5.2_NDVI_Points.png",
    width = 2000,
    height = 1500,
    res = 300)
plot(ndvi_canada, main = "Sampling points over NDVI raster")
plot(points_vect, add = TRUE, col = "red", pch = 16)

dev.off()

# ==============================================================================
# 8. Extract NDVI values at point locations
# ==============================================================================
ndvi_values <- terra::extract(ndvi_canada, points_vect)

# Check extracted values
head(ndvi_values)


# ==============================================================================
# 9. Add NDVI values to the original data frame
# ==============================================================================
# The first column returned by terra::extract() is usually the point ID
# and the second column contains the extracted raster value.

matrix_full_clim_eco_elev$NDVI <- ndvi_values[, 2]

# Check the updated table
head(matrix_full_clim_eco_elev)


# ==============================================================================
# 10. Simple control plot
# ==============================================================================

png("Outputs/sat_manual_plots/5.3_NDVI_Distribution_climate.png",
    width = 2000,
    height = 1500,
    res = 300)

  ggplot(matrix_full_clim_eco_elev, aes(x = NDVI, fill = Climate_Re)) +
  geom_density(alpha = 0.5, adjust = 3) +  # smoothed density curves
  labs(
    title = "NDVI Distribution by Climate",
    x = "NDVI",
    y = "Density"
  ) +
  theme_minimal()

dev.off()


write.csv(matrix_full_clim_eco_elev, "data/matrix_full_clim_eco_elev.csv", row.names = FALSE)
