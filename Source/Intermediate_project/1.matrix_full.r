# SPECIES JUSTIFICATION ?

# ==================================================================
# Occurences of Ursus arctos in Canada (data origin = Inat and GBIF)

# =========================
# 1.1) PACKAGES & UPLOADINGS


#install.packages("rgbif")
library(rgbif)         # access to GBIF data
library(rnaturalearth) # country maps
library(ggplot2)       # graphics
library(rinat)         # access to iNaturalist data
library(raster)        # spatial extent management
library(dplyr)         # table manipulation
library(sf)            # modern spatial objects

# Disable spherical geometry for simpler spatial operations
sf_use_s2(FALSE)

# ===========


# =========================
# 1.2) MY PARAMETERS

myspecies <- "Ursus arctos"

gbif_limit <- 1000

# Time filtering period
date_start <- as.Date("2000-01-01")
date_end   <- as.Date("2026-03-11")

# Simplified geographic extent for Canada - numbers corresponding to the simplified boarders of Canada
xmin <- -141 # West boarder, near the 141° West meridian
xmax <- -52 # East border, doens't take into account Atlantic Islands (Terre-Neuve and Labrador)
ymin <- 41 # Southern boarder with USA
ymax <- 83 # Northern boarder, encompasses Nunavut island, Ellesmere island and other Arctic islands


# =========================
# 1.3) BASE MAP OF CANADA

# Download the outline of Canada
Canada <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Canada"
)

# Simple visualization of the map
p1 <- ggplot(data = Canada) +
geom_sf(fill = "grey95", color = "black") +
theme_classic()

#save_plot(p1, "1.1_Canada_map.png", folder = "matrix_full_plots") #save the plot in output
# =========================
# 1.4) DOWNLOAD GBIF DATA

# Download occurrences with coordinates
gbif_raw <- occ_data(
  scientificName = myspecies,
  hasCoordinate = TRUE,
  limit = gbif_limit
)

# Extract the main data table
gbif_occ <- gbif_raw$data

# Quick inspection
head(gbif_occ)
names(gbif_occ)

# Select occurrences located in Canada
gbif_canada <- gbif_occ %>%
  filter(country == "Canada")

# Check number of records
nrow(gbif_canada)

# Quick base plot for checking
x11()
plot(
  gbif_canada$decimalLongitude,
  gbif_canada$decimalLatitude,
  pch = 16,
  col = "darkgreen",
  xlab = "Longitude",
  ylab = "Latitude",
  main = "GBIF occurrences in Canada"
)


# Map showing GBIF occurrences only
x11()
p2 <- ggplot(data = Canada) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = gbif_canada,
    aes(x = decimalLongitude, y = decimalLatitude),
    size = 3,
    shape = 21,
    fill = "darkgreen",
    color = "black"
  ) +
  theme_classic()

#save_plot(p2, "1.2_GBIF_occurences_U.arctos.png", folder = "matrix_full_plots")
# =========================
# 1.5) FORMAT GBIF DATA

# Keep only the useful columns
# as.Date() keeps only the date
data_gbif <- data.frame(
  species   = gbif_canada$species,
  latitude  = gbif_canada$decimalLatitude,
  longitude = gbif_canada$decimalLongitude,
  date_obs  = as.Date(gbif_canada$eventDate),
  source    = "gbif"
)

# Check structure
head(data_gbif)
str(data_gbif)

# =========================
# 1.6) DOWNLOAD iNaturalist DATA

# Query iNaturalist for the same species in Canada
# place_id = "canada" usually works with rinat
inat_raw <- get_inat_obs(
  query = myspecies,
  place_id = "canada"
)

# Inspect the structure
head(inat_raw)
names(inat_raw)

# Map showing iNaturalist occurrences only
x11()
p3 <- ggplot(data = Canada) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = inat_raw,
    aes(x = longitude, y = latitude),
    size = 3,
    shape = 21,
    fill = "lightblue",
    color = "black"
  ) +
  theme_classic()

#save_plot(p3, "1.3_iNat_occurences_U.arctos.png", folder = "matrix_full_plots")
# =========================
# 1.7) FORMAT iNaturalist DATA

# Convert it to Date format
data_inat <- data.frame(
  species   = inat_raw$scientific_name,
  latitude  = inat_raw$latitude,
  longitude = inat_raw$longitude,
  date_obs  = as.Date(inat_raw$observed_on),
  source    = "inat"
)

# Check structure
head(data_inat)
str(data_inat)

# =========================
# 1.8) MERGE THE TWO DATABASES

# Stacking of GBIF and Inat data using bind_rows() instead of merge().
matrix_full <- bind_rows(data_gbif, data_inat) 

# Check results
head(matrix_full)
table(matrix_full$source, useNA = "ifany")
summary(matrix_full$date_obs)

## =========================
# 1.9) TIME FILTERING BETWEEN TWO DATES

# Keep only observations within the selected time interval
matrix_full_date <- matrix_full %>%
  filter(!is.na(date_obs)) %>%
  filter(date_obs >= date_start & date_obs <= date_end)

# Check results
head(matrix_full_date)
summary(matrix_full_date$date_obs)
table(matrix_full_date$source)

# =========================
# 1.10) MAP OF COMBINED DATA

x11()
ggplot(data = Canada) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = matrix_full_date,
    aes(x = longitude, y = latitude, fill = source),
    size = 3,
    shape = 21,
    color = "black",
    alpha = 0.8
  ) +
  theme_classic()

# =========================
# 1.11) DEFINE A SIMPLE SPATIAL EXTENT


# Crop the background using coordinates

library(sf)

sf_use_s2(FALSE)

# Define the spatial extent
extent(Canada)
ext_Canada_cut <- as(raster::extent(-141, -52, 41, 83), "SpatialPolygons")

# Crop Canada map to the defined extent
Canada_crop <- st_crop(Canada, ext_Canada_cut)

# Plot cropped map with occurrence points
x11()
ggplot(data = Canada_crop) +
  geom_sf() +
  geom_point(
    data = matrix_full,
    aes(x = longitude, y = latitude, fill = source),
    size = 4,
    shape = 23
  ) +
  theme_classic()

# Exclude points outside the specified spatial extent

# Convert occurrences to sf object
data_gbif_sf <- st_as_sf(matrix_full, coords = c("longitude", "latitude"), crs = 4326)

# Convert cropped Canada polygon to sf
Canada_crop_sf <- st_as_sf(Canada_crop)

# Identify points located inside the spatial extent
cur_data <- matrix_full[as.matrix(st_intersects(data_gbif_sf, Canada_crop_sf)),]

# Plot cropped Canada map with filtered points
x11()
p4 <- ggplot(data = Canada_crop) +
  geom_sf() +
  geom_point(
    data = cur_data,
    aes(x = longitude, y = latitude, fill = source),
    size = 4,
    shape = 23
  ) +
  theme_classic()

#save_plot(p4, "1.4_GBIF+iNat_occurences_U.arctos.png", folder = "matrix_full_plots")
# =========================
# 1.12) SAVE OF THE FINAL TABLE

# Save filtered occurrence table
write.csv(
  cur_data,
  file = "name.csv",
  row.names = FALSE
)




# =====================================================================
# Occurences of Ursus maritimus in Canada (data origin = Inat and GBIF)

# =========================
# 2.1) MY PARAMETERS

# Species of interest
myspecies2 <- "Ursus maritimus" 

# Maximum number of GBIF records to download
gbif_limit <- 10000

# Time filtering period
date_start <- as.Date("2000-01-01")
date_end   <- as.Date("2026-03-11")

# Simplified geographic extent for Canada - numbers corresponding to the simplified boarders of Canada
xmin <- -141 # West boarder, near the 141° West meridian
xmax <- -52 # East border, doens't take into account Atlantic Islands (Terre-Neuve and Labrador)
ymin <- 41 # Southern boarder with USA
ymax <- 83 # Northern boarder, encompasses Nunavut island, Ellesmere island and other Arctic islands

#=========================
# 2.2) BASE MAP: CANADA

# Download the outline of Canada
Canada <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Canada"
)
# Convert GBIF points to sf object
gbif_sf <- st_as_sf(
  gbif_occ,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326,
  remove = FALSE
)

# =========================
# 2.3) DOWNLOAD GBIF DATA

# Download occurrences with coordinates
gbif_raw <- occ_data(
  scientificName = myspecies2,
  hasCoordinate = TRUE,
  limit = gbif_limit
)

# Extract the main data table
gbif_occ <- gbif_raw$data

# Quick inspection
head(gbif_occ)
names(gbif_occ)

# Select occurrences located in Canada
gbif_canada <- gbif_sf[Canada, , op = st_within]

# Check number of records
nrow(gbif_canada)

# Quick base plot for checking
x11()
plot(
  gbif_canada$decimalLongitude,
  gbif_canada$decimalLatitude,
  pch = 16,
  col = "darkorange",
  xlab = "Longitude",
  ylab = "Latitude",
  main = "GBIF occurrences in Canada")

# Map showing GBIF occurrences only
p5 <- ggplot(data = Canada) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = gbif_canada,
    aes(x = decimalLongitude, y = decimalLatitude),
    size = 3,
    shape = 21,
    fill = "blue",
    color = "black"
  ) +
  theme_classic()

#save_plot(p5, "1.5_GBIF_occurences_U.maritimus.png", folder = "matrix_full_plots")
# =========================
# 2.4) FORMAT GBIF DATA

# Keep only the useful columns
# eventDate may contain date + time; as.Date() keeps only the date
data_gbif <- data.frame(
  species   = gbif_canada$species,
  latitude  = gbif_canada$decimalLatitude,
  longitude = gbif_canada$decimalLongitude,
  date_obs  = as.Date(gbif_canada$eventDate),
  source    = "gbif"
)

# Check structure
head(data_gbif)
str(data_gbif)

# =========================
# 2.5) DOWNLOAD iNaturalist DATA

# Query iNaturalist for the same species in Canada
# place_id = "canada" usually works with rinat
inat_raw <- get_inat_obs(
  query = myspecies2,
  place_id = "canada"
)

# Inspect the structure
head(inat_raw)
names(inat_raw)

# Map showing iNaturalist occurrences only
x11()
p6 <- ggplot(data = Canada) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = inat_raw,
    aes(x = longitude, y = latitude),
    size = 3,
    shape = 21,
    fill = "lightpink",
    color = "black"
  ) +
  theme_classic()

#save_plot(p6, "1.6_iNat_occurences_U.maritimus.png", folder = "matrix_full_plots")
# =========================
# 2.6) FORMAT iNaturalist DATA

# Convert it to Date format
data_inat <- data.frame(
  species   = inat_raw$scientific_name,
  latitude  = inat_raw$latitude,
  longitude = inat_raw$longitude,
  date_obs  = as.Date(inat_raw$observed_on),
  source    = "inat"
)

# Check structure
head(data_inat)
str(data_inat)

# =========================
# 2.7) MERGE THE TWO DATABASES

# Stacking of GBIF and Inat data using bind_rows() instead of merge().
matrix_full <- bind_rows(data_gbif, data_inat) 

# Check results
head(matrix_full)
table(matrix_full$source, useNA = "ifany")
summary(matrix_full$date_obs)

# =========================
# 2.8) TIME FILTERING BETWEEN TWO DATES

# Keep only observations within the selected time interval
matrix_full_date <- matrix_full %>%
  filter(!is.na(date_obs)) %>%
  filter(date_obs >= date_start & date_obs <= date_end)

# Check results
head(matrix_full_date)
summary(matrix_full_date$date_obs)
table(matrix_full_date$source)

# =========================
# 2.9) MAP OF COMBINED DATA
x11()
ggplot(data = Canada) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = matrix_full_date,
    aes(x = longitude, y = latitude, fill = source),
    size = 3,
    shape = 21,
    color = "blue",
    alpha = 0.8
  ) +
  theme_classic()

# =========================
# 2.10) DEFINE A SIMPLE SPATIAL EXTENT

#Crop the background using coordinates
library(sf)

sf_use_s2(FALSE)

# Define the spatial extent
extent(Canada)
ext_Canada_cut <- as(raster::extent(-141, -52, 41, 83), "SpatialPolygons")

# Crop Canada map to the defined extent
Canada_crop <- st_crop(Canada, ext_Canada_cut)

# Plot cropped map with occurrence points
x11()
ggplot(data = Canada_crop) +
  geom_sf() +
  geom_point(
    data = matrix_full,
    aes(x = longitude, y = latitude, fill = source),
    size = 4,
    shape = 23
  ) +
 theme_classic()

# Exclude points outside the specified spatial extent

# Convert occurrences to sf object
data_gbif_sf <- st_as_sf(matrix_full, coords = c("longitude", "latitude"), crs = 4326)

# Convert cropped Canada polygon to sf
Canada_crop_sf <- st_as_sf(Canada_crop)

# Identify points located inside the spatial extent
cur_data2 <- matrix_full[as.matrix(st_intersects(data_gbif_sf, Canada_crop_sf)),]

x11()
p7 <- ggplot(data = Canada_crop) +
  geom_sf() +
  geom_point(
    data = cur_data2,
    aes(x = longitude, y = latitude, fill = source),
    size = 4,
    shape = 23
  ) +
  theme_classic()

#save_plot(p7, "1.7_GBIF+iNat_occurences_U.maritimus.png", folder = "matrix_full_plots")
# =========================
# 2.11) SAVE OF THE FINAL TABLE

# Save filtered occurrence table
write.csv(
  cur_data2,
  file = "name2.csv",
  row.names = FALSE
)


# ============================================================
# 3.1 Create a new matrix with the occurence of my two species 

library(ggplot2)
library(dplyr)
library(stringr)
library(ggtext)

Two_species <- bind_rows(cur_data, cur_data2)

# Ursus arctos horribilis is added as a sub-species
Two_species_clean <- Two_species %>%
  mutate(
    species = case_when(
      str_detect(tolower(species), "horribilis") ~ "Ursus arctos horribilis", # if horribilis -> goes in the newly created category : Ursus arctos horribilis 
      str_detect(tolower(species), "arctos")     ~ "Ursus arctos",
      str_detect(tolower(species), "maritimus")  ~ "Ursus maritimus",
      TRUE                                       ~ NA_character_ # NA_character are ignored
    ),
    source = tolower(source)
  ) %>%
  filter(!is.na(species))

# Creation of occurrence ID
Two_species_clean$occurrence_id=c(1:nrow(Two_species_clean))
head(Two_species_clean)

# Add the corresponding colors to the legend "Species"
species_colors <- c(
  "Ursus arctos" = "#8B4513",
  "Ursus arctos horribilis" = "#D2691E",
  "Ursus maritimus" = "#0b82e9"
)

# Labels of Species in italic with *
species_labels <- c(
  "Ursus arctos" = "*Ursus arctos*",
  "Ursus arctos horribilis" = "*Ursus arctos horribilis*",
  "Ursus maritimus" = "*Ursus maritimus*"
)

x11()
p8 <- ggplot(data = Canada_crop) +
  geom_sf(fill = "lightgray", color = "black", alpha = 0.5) +
  geom_point(
    data = Two_species_clean,
    aes(x = longitude, y = latitude, 
        fill = species,  
        shape = source  
    ),
    size = 4,
    stroke = 0.8
  ) +
  scale_fill_manual(
    name = "Species",
    values = species_colors,
    labels = species_labels,
    drop = FALSE 
  ) +
  scale_shape_manual(
    name = "Source",
    values = c("inat" = 21, "gbif" = 22)
  ) +
  labs(
    title = "Occurrences of Ursus arctos, the sub-species Ursus arctos horribilis and Ursus maritimus in Canada",
    ) +
  guides(
    
    fill = guide_legend(
      order = 1,
      override.aes = list(
        shape = 21, #this shape allows color to be filled
        fill = c("#8B4513", "#D2691E", "#0b82e9"),
        color = "black" # Adds a black outline 
      )
    ),
    shape = guide_legend(order = 2)
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "right",
    legend.text = element_markdown(size = 10),
    legend.title = element_text(face = "bold")
  )

#save_plot(p8, "1.8_Occurences_U.arctos+U.a.horribilis+U.maritimus.png", folder = "matrix_full_plots")
