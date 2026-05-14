
# ==========================================
# EXTRACTING ELEVATION DATA IN CANADA AND VISUALIZATION

# =========================
# 1. Load required packages

library(sf)        # modern spatial data handling 
library(elevatr)   # download elevation data
library(raster)    # raster data manipulation for maps
library(ggplot2)   # data visualization
library(rnaturalearth)

# Disable s2 geometry engine to avoid issues in some spatial operations
sf_use_s2(FALSE)


# =========================
# 2. Load Canada boundaries

# Retrieve country borders from Natural Earth
Canada <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Canada"
)


# =========================
# 3. Download elevation data

# z controls resolution (higher = more detail but slower)
elevation_canada <- get_elev_raster(Canada, z = 5)


# Quick visualization of the elevation raster and save 
dir.create("Outputs/elevation_plots", recursive = TRUE, showWarnings = FALSE)
png("Outputs/elevation_plots/4.1_Elevation_raster.png",
    width = 2000,
    height = 1500,
    res = 300)

plot(elevation_canada)

dev.off()

# =========================
# 4. Prepare sampling points

# Convert coordinates into a spatial object (SpatialPoints format)
spatial_points <- SpatialPoints(
  coords = matrix_full_clim_eco[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)


# =========================
# 5. Extract elevation values

# Extract raster values at each point location
elevation <- raster::extract(elevation_canada, spatial_points)


# =========================
# 6. Add elevation to the dataset

matrix_full_clim_eco_elev <- data.frame(
  matrix_full_clim_eco,
  elevation = elevation
)
head(matrix_full_clim_eco_elev)

# =========================
# 7. Visualization: elevation distribution

# Compare elevation distributions across climate categories and save
x11()

png("Outputs/elevation_plots/4.2_Compare_elevation.png",
    width = 2000,
    height = 1500,
    res = 300)

ggplot(matrix_full_clim_eco_elev, aes(x = elevation, fill = Climate_Re)) +
  geom_density(alpha = 0.5, adjust = 3) +  # smoothed density curves
  labs(
    title = "Elevation Distribution by Climate",
    x = "Elevation (m)",
    y = "Density"
  ) +
  theme_minimal()

dev.off()
