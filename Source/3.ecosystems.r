# When run through -> everything OK
# When run through Source ->


# ==========================================
# ADDING ECOSYSTEM DATA TO SPECIES OCCURRENCE COORDINATES

# =========================
# 1.1 ) LOAD REQUIRED PACKAGES

library(raster) # raster: to read and manipulate raster files
library(sf) # sf: to handle vector spatial data
library(rnaturalearth) # rnaturalearth: to download country boundaries
library(ggplot2) # ggplot2: to create graphs
library(sp) # sp: provides classes and modes for spatial data 


# =========================
# 1.2) LOAD THE ECOSYSTEM RASTER

# Define the path to the GeoTIFF file
file_path <- "./Data/WorldEcosystem.tif"

# Read the raster layer
ecosystem_raster <- raster("/Users/linewermeille/Desktop/Master Unine/Biodiversity data analyses/my_project_2026/Data/WorldEcosystem.tif")

# Gives basic information about the raster
print(ecosystem_raster)

# =========================
# 1.3) LOAD THE BOUNDARY OF CANADA

# Download the country boundary as an sf object
Canada <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Canada"
)

# Plot the country boundary
plot(st_geometry(Canada), main = "Boundary of Canada")

# =========================
# 1.4) CROP AND MASK THE RASTER TO CANADA

# This will keep only the rectangular extent around Canada
r2 <- crop(ecosystem_raster, extent(Canada))

# Only the pixels that fall inside the country boundary will be kept
ecosystem_canada <- mask(r2, Canada)

# Plot the cropped and masked raster
x11()
plot(ecosystem_canada, main = "Ecosystem Raster Restricted to CANADA")


save_base_plot(
  plot(ecosystem_canada, main = "Ecosystem_Raster_CANADA"),
  "3.1_Raster_eco_Canada.png",
  folder = "ecosystems_plots"
)

# =========================
# 1.5) CONVERT SPECIES COORDINATES INTO SPATIAL POINTS

# Two_species_clean contains the 3 species (U. arctos, U a. horribilis and U. maritimus)
matrix_full_clim <- species_climate_future_df #was before Two_species_clean but now with climate = species_cliamte_df 
head(matrix_full_clim)

# Convert the coordinate columns into spatial points
# The CRS used here is WGS84, which is the standard geographic coordinate system
spatial_points <- SpatialPoints(
  coords = matrix_full_clim[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

# Add the occurrence points on top of the ecosystem map
png("Outputs/ecosystems_plots/3.2_Ecosystem_occurrences.png",
    width = 2000,
    height = 1500,
    res = 300)
plot(ecosystem_canada, main = "Species Occurrences on Ecosystem Map")
plot(spatial_points, add = TRUE, pch = 16, cex = 1.2)

dev.off()
# =========================
# 1.6) EXTRACT ECOSYSTEM VALUES AT EACH OCCURRENCE POINT

# extract() retrieves the raster value at the location of each point
# Each point receives the ecosystem code of the raster cell where it falls
eco_values <- raster::extract(ecosystem_canada, spatial_points)

# Check the extracted values
head(eco_values)

# =========================
# 1.7) ADD THE EXTRACTED ECOSYSTEM VALUES TO THE ORIGINAL DATA FRAME

# Create a new data frame by adding the extracted ecosystem values
matrix_full_clim_eco <- data.frame(matrix_full_clim, eco_values)

# Inspect the result
head(matrix_full_clim_eco)

# =========================
# 1.8) LOAD THE ECOSYSTEM METADATA TABLE

# This metadata table links the numeric raster code to descriptive ecosystem names
metadata_eco <- read.delim("/Users/linewermeille/Desktop/Master Unine/Biodiversity data analyses/my_project_2026/Data/WorldEcosystem.metadata.tsv")
# getwd()
# Inspect the metadata table
head(metadata_eco)

# =========================
# 1.9) MERGE THE EXTRACTED VALUES WITH THE METADATA

# Merge the occurrence table with the metadata table
# by.x = "eco_values" means the ecosystem code in our occurrence table
# by.y = "Value" means the corresponding code column in the metadata table
matrix_full_clim_eco <- merge(
  matrix_full_clim_eco,
  metadata_eco,
  by.x = "eco_values",
  by.y = "Value"
)

# Inspect the enriched table
head(matrix_full_clim_eco)

# =========================
# 1.10) VISUALIZE THE NUMBER OF OBSERVATIONS PER CLIMATE CATEGORY AND SPECIES


# Creates a bar plot showing how many observations of each species
# are found in each climate category

library(dplyr)
library(ggtext) 

# Matches colors to species
species_colors <- c(
  "Ursus arctos" = "#8B4513",
  "Ursus arctos horribilis" = "#D2691E",
  "Ursus maritimus" = "#0b82e9"
)

species_labels <- c(
  "Ursus arctos" = "*Ursus arctos*",
  "Ursus arctos horribilis" = "*Ursus arctos horribilis*",
  "Ursus maritimus" = "*Ursus maritimus*"
)

# x11()
p2 <- ggplot(matrix_full_clim_eco, aes(x = Climate_Re, fill = species)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Count of Observations of Each Species by Climate",
    x = "Climate category",
    y = "Number of observations",
    fill = "Species" 
  ) +
  scale_fill_manual(
    name = "Species",
    values = species_colors,
    labels = species_labels # Labels are in italic
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.text = element_markdown(size = 10), # Activates the Markdown package for italic
    legend.title = element_text(face = "bold", size = 11)
  ) +
  guides(
    fill = guide_legend(
      order = 1,
      override.aes = list(
        fill = c("#8B4513", "#D2691E", "#0b82e9"),
        color = "black" 
      )
    )
  )


save_plot(p2, "3.3_Climate_vs_Species.png", folder = "ecosystems_plots")
