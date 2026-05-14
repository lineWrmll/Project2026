# =========================
# Adding climate data to my matrix

# 1.1) PACKAGES

library(Rchelsa)
library(terra)
library(dplyr)
library(ggplot2)

# =========================
# Dataframe from matrix_full.r
Two_species_clean
head(Two_species_clean)


# =========================
# 2) CREATE A SPATIAL OBJECT

# Creation of a spatial vector (longitude+latitude)
pts_v <- terra::vect(
  Two_species_clean,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

# Extract simple coordinates as a standard data frame
coords_df <- as.data.frame(terra::geom(pts_v)[, c("x", "y")]) |>
  rename(
    longitude = x,
    latitude = y
  ) |>
  mutate(occurrence_id = Two_species_clean$occurrence_id)

coords_df <- as.data.frame(terra::geom(pts_v)[, c("x", "y")]) %>%
  rename(
    longitude = x,
    latitude = y
  ) %>%
  mutate(occurrence_id = Two_species_clean$occurrence_id)

coords_df

# =========================
# 3) EXTRACT MONTHLY Tmax FO

# CHELSA variable naming:
# - tas    = near-surface air temperature
# - tasmin = minimum near-surface air temperature
# - tasmax = maximum near-surface air temperature
# - pr     = precipitation
#
# Temperature values are often returned in Kelvin.
# Conversion to Celsius: °C = K - 273.15

# Fixing the issue with duplicates -> Erase duplicates
coords_unique <- coords_df %>%
mutate(original_row = row_number()) %>%
distinct(longitude, latitude, .keep_all = TRUE)

tmax_r <- getChelsa(
  var       = "tasmax",
  coords    = coords_unique %>% select(longitude, latitude),
  startdate = as.Date("2018-01-01"),
  enddate   = as.Date("2019-01-01"),
  dataset   = "chelsa-monthly"
)
# Remove the time column with dplyr, then convert to matrix
tmax_mat <- tmax_r %>%
  select(-time) %>%
  as.matrix()


# Convert to tibble 
tmax_mat <- tmax_r %>%
  as_tibble() %>%      # Ensure it's a tibble
  select(-time) %>%    # Now select() works
  as.matrix()

# Calculate the mean across the 12 months for each point
# colMeans() works by column, and here each column corresponds to one point
tmax_mean_k <- colMeans(tmax_mat, na.rm = TRUE)

# Convert Kelvin to Celsius
tmax_mean_c <- tmax_mean_k - 273.15

# Get the usable dataframe 
coords_unique <- coords_df %>%
distinct(longitude, latitude) %>%
mutate(tmax_mean_c= as.numeric(tmax_mean_c))

# Get the needed dataframe with temperatures 
tmax_df <- Two_species_clean %>%
select(occurrence_id, longitude, latitude) %>%
left_join(coords_unique, by = c("longitude", "latitude")) %>%
select(occurrence_id, tmax_mean_c)

head(tmax_df)
nrow(tmax_df)
# =========================
# 4) EXTRACT MONTHLY PRECIPITATION FOR 2018

prec_r <- getChelsa(
  var       = "pr",
  coords    = coords_unique %>% select(longitude, latitude),
  startdate = as.Date("2018-01-01"),
  enddate   = as.Date("2018-02-01"),
  dataset   = "chelsa-monthly"
)

# Remove the time column with dplyr, then convert to matrix
prec_mat <- prec_r %>%
  select(-time) %>%
  as.matrix()

# Calculate the mean across the period for each point
prec_mean <- colMeans(prec_mat, na.rm = TRUE)

# Create a table containing the precipitation variable
prec_df <- coords_unique %>%
  select(longitude, latitude) %>%
  mutate(prec_mean_annual = as.numeric(prec_mean)) %>%
  right_join(
    Two_species_clean %>% select(occurrence_id, longitude, latitude),
    by = c("longitude", "latitude")
  ) %>%
  select(occurrence_id, prec_mean_annual)

nrow(prec_df)                        # should be 836
sum(is.na(prec_df$prec_mean_annual)) # should be 0
head(prec_df)



# =========================
# 5) JOIN THE NEW CLIMATE VARIABLES TO THE ORIGINAL DATASET

species_climate_df <- Two_species_clean %>%
  left_join(tmax_df, by = "occurrence_id") %>%
  left_join(prec_df, by = "occurrence_id")

species_climate_df # matrix full + tmax_mean et prec_mean_annual

# =========================
# 6) CHECK THE RESULT

dim(Two_species_clean)    # original dimensions
dim(species_climate_df)   # enriched dimensions
names(species_climate_df) # column names after enrichment

# =========================
# 7) PLOT THE DISTRIBUTION OF ANNUAL MEAN Tmax

x11()
p1 <- ggplot(species_climate_df, aes(x = tmax_mean_c)) +
  geom_density(color = "darkred", fill = "salmon", adjust = 1.5) +
  theme_classic() +
  labs(
    title = "Ursus arctos and Ursus maritimus: annual mean Tmax (2018)",
    x = "Annual mean Tmax (°C)",
    y = "Density"
  )

save_plot(p1, "2.1_Annual_mean_Tmax.png", folder = "climate_plots")
# =========================
# 8) PLOT THE DISTRIBUTION OF ANNUAL MEAN PRECIPITATION

x11()
p2 <- ggplot(species_climate_df, aes(x = prec_mean_annual)) +
  geom_density(color = "black", fill = "darkgreen", adjust = 1.5) +
  theme_classic() +
  labs(
    title = "Ursus arctos and Ursus maritimus: annual mean precipitation (2018)",
    x = "Annual mean precipitation",
    y = "Density"
  )
save_plot(p2, "2.2_Annual_mean_precipitation.png", folder = "climate_plots")

# Two_species_clean becomes species_climate_df
head(species_climate_df)

# =========================
# 9) CURRENT CLIMATE: July temperature (climatology 1981-2010)

tas_current_july <- getChelsa(
  var     = "tas",
  coords  = coords_df %>% dplyr::select(longitude, latitude),
  date    = c(7, 1981, 2010),
  dataset = "chelsa-climatologies"
)

dim(tas_current_july) #checkin'

# Remove metadata column/row named "time"
tas_values <- tas_current_july[, colnames(tas_current_july) != "time"]

# Convert to numeric vector
tas_values <- as.numeric(tas_values[1, ]) - 273.15

# Check lengths
length(tas_values)
nrow(species_climate_df)

# Build dataframe
current_july_df <- data.frame(
  occurrence_id       = species_climate_df$occurrence_id,
  current_july_temp_c = tas_values
)

names(current_july_df) #checkin'

# =========================
# 10) FUTURE CLIMATE: July temperature in 2050 -> Intermediate scenario

tas_future_july <- getChelsa(
  var     = "tas",
  coords  = coords_df %>% dplyr::select(longitude, latitude),
  date    = as.Date("2051-07-01"),
  dataset = "chelsa-climatologies",
  ssp     = "ssp370",    #intermediate scenario
  forcing = "GFDL-ESM4"  #US model for Canada
)

# Convert to numeric vector
future_values <- as.numeric(unlist(tas_future_july))

# Remove metadata value(s)
future_values <- future_values[-1]

# CHELSA temperatures are often stored as 0.1 K
future_values_c <- (future_values) - 273.15

# Keep correct number of rows
future_values_c <- future_values_c[1:nrow(species_climate_df)]

# Final dataframe
future_july_df <- data.frame(
  occurrence_id      = species_climate_df$occurrence_id,
  future_july_2050_c = future_values_c
)

head(future_july_df)
#print(future_july_df)


# =========================
# 11) MERGING AND CALCULATING CHANGE

species_climate_future_df <- species_climate_df %>%
  left_join(current_july_df, by = "occurrence_id") %>%
  left_join(future_july_df,  by = "occurrence_id") %>%
  dplyr::mutate(
    july_temp_change_c = future_july_2050_c - current_july_temp_c
  )

head(species_climate_future_df) #checkin'

# Statistics on temperature change
print(summary(species_climate_future_df$july_temp_change_c))
cat("\nMean projected change: ", 
    round(mean(species_climate_future_df$july_temp_change_c), 2), "°C\n\n")

# =========================
# 12) VISUALISATIONS OF FUTURE PROJECTIONS

# Plot 3: Current vs future comparison
x11()
p3 <- ggplot(species_climate_future_df, 
             aes(x = current_july_temp_c, y = future_july_2050_c)) +
  geom_point(size = 4, color = "steelblue", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", 
              color = "gray40", linewidth = 1) +
  annotate("text", x = min(species_climate_future_df$current_july_temp_c) + 0.5,
           y = max(species_climate_future_df$future_july_2050_c) - 0.5,
           label = "1:1 line\n(no change)", 
           color = "gray40", size = 3.5) +
  theme_classic(base_size = 12) +
  labs(
    title    = "July temperature: Current vs Future (2050)",
    subtitle = "Pinus sylvestris - SSP126 scenario",
    x = "Current July temperature (°C) [1981-2010]",
    y = "Future July temperature (°C) [2050]"
  ) +
  theme(plot.title = element_text(face = "bold"))

save_plot(p3, "2.3_CurrentVSFuture_projections.png", folder = "climate_plots")

# Plot 4: Distribution of temperature change
x11()
p4 <- ggplot(species_climate_future_df, aes(x = july_temp_change_c)) +
  geom_histogram(bins = 10, fill = "orange", color = "black", alpha = 0.7) +
  geom_vline(xintercept = mean(species_climate_future_df$july_temp_change_c),
             color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", 
           x = mean(species_climate_future_df$july_temp_change_c) + 0.1,
           y = Inf,
           label = paste0("Mean: ", 
                         round(mean(species_climate_future_df$july_temp_change_c), 2), 
                         "°C"),
           color = "red", vjust = 2, hjust = 0) +
  theme_classic(base_size = 12) +
  labs(
    title    = "Distribution of projected temperature change",
    subtitle = "Difference between 2050 (SSP126) and 1981-2010",
    x = "July temperature change (°C)",
    y = "Number of occurrences"
  ) +
  theme(plot.title = element_text(face = "bold"))

save_plot(p4, "2.4_Distribution_Temp_change.png", folder = "climate_plots")

# Plot 5: Map of changes
x11()
p5 <- ggplot(species_climate_future_df, 
             aes(x = longitude, y = latitude, color = july_temp_change_c)) +
  geom_point(size = 5, alpha = 0.8) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red",
                        midpoint = mean(species_climate_future_df$july_temp_change_c),
                        name = "Δ Temp (°C)") +
  theme_classic(base_size = 12) +
  labs(
    title    = "Spatial distribution of temperature change",
    subtitle = "July 2050 (SSP126) vs 1981-2010",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

save_plot(p5, "2.5_Maps_of_changes.png", folder = "climate_plots")
