# Projected distribution by 2050


# In order to predict the distribution of my species in 2050, incorporating climate change, I need to download WorldClim 2050 
# projections. Some part of the code below was already conducted in the first part of the project.

# I had issues with this last section. I have a low number of observations, I don't understand where the rest went.
# I still conducted the maps, however, the results must be treated with caution.

# Loading of the libraries
library(geodata)
library(terra)
library(randomForest)
library(ggplot2)
library(dplyr)
library(plotly)


# Only these 4 variables can be extracted from future climate rasters
predictor_vars_grid <- c("elevation", "prec_mean_annual", "current_july_temp_c", "NDVI")

set.seed(42)
rf_model_grid <- randomForest(
  x          = train_data[, predictor_vars_grid],
  y          = train_data$species,
  ntree      = 500,
  mtry       = floor(sqrt(length(predictor_vars_grid))),
  importance = TRUE
)

print(rf_model_grid)


# Fit NDVI Model
ndvi_model <- lm(NDVI ~ current_july_temp_c + prec_mean_annual + elevation,
                 data = df_model)

summary(ndvi_model)


# Download Climate Data
worldclim_current <- worldclim_global(
  var  = "bio",
  res  = 10,
  path = "data/worldclim/current/"
)

future_2050_ssp245 <- cmip6_world(
  model = "ACCESS-CM2",
  ssp   = "245",
  time  = "2041-2060",
  var   = "bioc",
  res   = 10,
  path  = "data/worldclim/future/"
)

future_2050_ssp585 <- cmip6_world(
  model = "ACCESS-CM2",
  ssp   = "585",
  time  = "2041-2060",
  var   = "bioc",
  res   = 10,
  path  = "data/worldclim/future/"
)


# Build Canada Spatial Grid
canada_ext  <- ext(-141, -52, 42, 83)
canada_grid <- rast(canada_ext, resolution = 0.5, crs = "EPSG:4326")
values(canada_grid) <- 1

canada_pts <- as.data.frame(canada_grid, xy = TRUE)
colnames(canada_pts) <- c("longitude", "latitude", "value")
canada_pts$value <- NULL

canada_vect <- vect(canada_pts, geom = c("longitude", "latitude"), crs = "EPSG:4326")

# Extract elevation once — reused for both scenarios
elev_raster <- elevation_global(res = 10, path = "data/worldclim/current/")
grid_elev   <- extract(elev_raster, canada_vect, ID = FALSE)
colnames(grid_elev) <- "elevation"

# Helper Function — Build Grid Predictions
build_canada_df <- function(climate_raster, canada_pts, canada_vect, grid_elev,
                            ndvi_model, rf_model, species_levels) {

  grid_climate <- extract(climate_raster, canada_vect, ID = FALSE)
  bio_n        <- names(grid_climate)

  df <- data.frame(
    longitude           = canada_pts$longitude,
    latitude            = canada_pts$latitude,
    elevation           = grid_elev$elevation,
    prec_mean_annual    = grid_climate[[grep("bio_12$|bio12$", bio_n, value = TRUE)]],
    current_july_temp_c = grid_climate[[grep("bio_10$|bio10$", bio_n, value = TRUE)]]
  )

  df <- df %>% filter(!is.na(elevation) & !is.na(prec_mean_annual))
  df$NDVI              <- predict(ndvi_model, newdata = df)
  df$predicted_species <- predict(rf_model,   newdata = df, type = "class")
  df$prob_presence     <- predict(rf_model,   newdata = df, type = "prob")[, species_levels[2]]

  return(df)
}


# Build SSP245 and SSP585 Grid Predictions
species_levels <- levels(df_model$species)

canada_df_245 <- build_canada_df(future_2050_ssp245, canada_pts, canada_vect,
                                  grid_elev, ndvi_model, rf_model_grid, species_levels)

canada_df_585 <- build_canada_df(future_2050_ssp585, canada_pts, canada_vect,
                                  grid_elev, ndvi_model, rf_model_grid, species_levels)

cat("SSP245 valid grid points:", nrow(canada_df_245), "\n")
cat("SSP585 valid grid points:", nrow(canada_df_585), "\n")

cat("\nSSP245 species distribution:\n"); table(canada_df_245$predicted_species)
cat("\nSSP585 species distribution:\n"); table(canada_df_585$predicted_species)


# Shared Plot Elements
species_labels_italic <- c(
  "Ursus arctos"    = expression(italic("Ursus arctos")),
  "Ursus maritimus" = expression(italic("Ursus maritimus"))
)

species_colours_combined <- c(
  "Ursus arctos Current"        = "#8B4513",
  "Ursus maritimus Current"     = "#2f2ff7",
  "Ursus arctos 2050 SSP245"    = "#97460d",
  "Ursus maritimus 2050 SSP245" = "#0b82e9"
)

species_labels_combined <- c(
  "Ursus arctos Current"        = expression(italic("U. arctos")~"(Current)"),
  "Ursus maritimus Current"     = expression(italic("U. maritimus")~"(Current)"),
  "Ursus arctos 2050 SSP245"    = expression(italic("U. arctos")~"(2050 SSP245)"),
  "Ursus maritimus 2050 SSP245" = expression(italic("U. maritimus")~"(2050 SSP245)")
)


# Plot Helper Functions
plot_species_map <- function(df, scenario_label) {
  ggplot(df, aes(x = longitude, y = latitude, fill = predicted_species)) +
    geom_raster() +
    scale_fill_manual(
      values = c("Ursus arctos"    = "#975424",
                 "Ursus maritimus" = "#2f8ee0"),
      name   = "Predicted Species",
      labels = species_labels_italic
    ) +
    coord_quickmap() +
    labs(
      title    = paste("Predicted Species Distribution —", scenario_label),
      subtitle = "Random Forest model | ACCESS-CM2 | 50km resolution",
      x = "Longitude", y = "Latitude"
    ) +
    theme_minimal() +
    theme(legend.text.align = 0)
}

plot_prob_map <- function(df, scenario_label) {
  ggplot(df, aes(x = longitude, y = latitude, fill = prob_presence)) +
    geom_raster() +
    scale_fill_gradient(
      low  = "#ffffff",
      high = "#449deb",
      name = expression(italic("P(U. maritimus)"))
    ) +
    coord_quickmap() +
    labs(
      title    = expression(paste("Probability of ", italic("U. maritimus"), " Presence")),
      subtitle = paste("Random Forest model | ACCESS-CM2 |", scenario_label),
      x = "Longitude", y = "Latitude"
    ) +
    theme_minimal()
}


# Plot SSP245
x11(); plot_species_map(canada_df_245, "2050 (SSP245) — Moderate emissions")
x11(); plot_prob_map(canada_df_245,    "2050 SSP245 — Moderate emissions")


# Plot SSP585
x11(); plot_species_map(canada_df_585, "2050 (SSP585) — High emissions")
x11(); plot_prob_map(canada_df_585,    "2050 SSP585 — High emissions")


# Panel displacement
library(patchwork)

# Generate the 4 plots as objects
p_ssp245_species <- plot_species_map(canada_df_245, "2050 — Moderate emissions")
p_ssp245_prob    <- plot_prob_map(canada_df_245,    "2050 — Moderate emissions")
p_ssp585_species <- plot_species_map(canada_df_585, "2050 — High emissions")
p_ssp585_prob    <- plot_prob_map(canada_df_585,    "2050 — High emissions")

# Assemble panel
final_map_panel <- (p_ssp245_species + p_ssp245_prob) /
                   (p_ssp585_species + p_ssp585_prob) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "",
    tag_suffix = ")",
    theme      = theme(plot.tag = element_text(face = "bold", size = 12))
  )

x11(); print(final_map_panel)

ggsave(
  "Outputs/Final_Project/4.Panels/4.3.Panel_Maps_2050.png",
  final_map_panel,
  width  = 16,
  height = 12,
  dpi    = 300,
  bg     = "white"
)


# ------- Interpretation of predicted species distribution -------
# Firstly, maps for both scenarios predict exactly the same pattern. 
# This is due to the fact that the Random forest analysis is very confident and will 
# assigns every grid cell to one species with near 100% probability, so the small climate 
# differences between the moderate emissions scenario and the high emmisions scenario 
# don't change the predicted species at any cell. 
# One way to fix that is to add the temperature to the random forest model -> it was only trained
# on 4 variables (longitude, elevation, NDVI and mean annual precipitation). Another way is to 
# balance the classes to have the same number for each class. 

# The first map predicts the distribution of U. arctos and polar bears U. maritimus in 2050 
# under a moderate emissions scenario. Based on this map, U. arctos are expected to occupy much 
# of mainland Canada, while U. maritimus remains concentrated in northern Arctic regions. 
# Transitional areas in northern Canada suggest an increas in overlap between the species' 
# projected ranges as climate conditions change. 

# A limitation for this map is that each grid cell assigned to a single species and doesn't show
# the probability of occurrence or abundance but only which species is predicted to be more 
# suitable in each location. 

# The second map presents the predicted probability of U. maritimus presence in 2050 still
# under a moderate emissions scenario. The highest occurrence probabilities are concentrated 
# in the Canadian Arctic Archipelago and northern coastal regions, while suitability declines 
# progressively toward southern latitudes. The spatial gradient suggests a future displacement
# of optimal habitat toward the high Arctic. 
# There is a very smooth west-to-east transition in the middle of Canada. This pattern is certainly
# due to the observations taken in zoos that weight enough to move the gradient western. Indeed, as
# U. maritimus closely linked to sea ice, it has low chances to occure inland as the map suggests. 



# Current vs SSP245 Combined Interactive Map
current_dist <- data.frame(
  longitude = df_model$longitude,
  latitude  = df_model$latitude,
  species   = as.character(df_model$species),
  scenario  = "Current"
)

ssp245_dist <- data.frame(
  longitude = canada_df_245$longitude,
  latitude  = canada_df_245$latitude,
  species   = as.character(canada_df_245$predicted_species),
  scenario  = "2050 SSP245"
)

combined_df <- bind_rows(current_dist, ssp245_dist) %>%
  mutate(species_scenario = paste(species, scenario))

raster_df <- combined_df %>% filter(scenario == "2050 SSP245")
points_df <- combined_df %>% filter(scenario == "Current") %>%
  mutate(bear_label = case_when(
    species == "Ursus arctos"    ~ paste0("🐻 Ursus arctos<br>Scenario: Current"),
    species == "Ursus maritimus" ~ paste0("🐻‍❄️ Ursus maritimus<br>Scenario: Current")
  ))

p_interactive <- plot_ly(width = 1200, height = 800) %>%
  add_trace(
    data      = raster_df,
    type      = "scatter",
    mode      = "markers",
    x         = ~longitude,
    y         = ~latitude,
    color     = ~species_scenario,
    colors    = species_colours_combined,
    marker    = list(symbol = "square", size = 6),
    text      = ~paste0("<i>", species, "</i><br>Scenario: ", scenario),
    hoverinfo = "text",
    alpha     = 0.7,
    name      = ~species_scenario
  ) %>%
  add_trace(
    data      = points_df,
    type      = "scatter",
    mode      = "markers",
    x         = ~longitude,
    y         = ~latitude,
    color     = ~species_scenario,
    colors    = species_colours_combined,
    marker    = list(symbol = "circle", size = 8,
                     line = list(color = "white", width = 1.5)),
    text      = ~bear_label,
    hoverinfo = "text",
    name      = ~species_scenario
  ) %>%
  layout(
    title  = list(text = "Current vs 2050 (SSP245) Species Distribution — Canada",
                  font = list(size = 16)),
    xaxis  = list(title = "Longitude"),
    yaxis  = list(title = "Latitude", scaleanchor = "x"),
    legend = list(title = list(text = "Species & Scenario")),
    margin = list(l = 50, r = 50, t = 80, b = 50)
  )

p_interactive


# ------- Interpretation of Interactive map -------
# The points shows the actual occurence of both species, while the brown background shows the
# predicted habitat for U. arctos and the blue background for U. maritimus. 
# 

# In my sense, this map is not giving us really useful informations. We only have the current 
# occurence and not the current habitats for the species. It makes it hard to draw any conclusion.
# However, it was nice to learn how to do it.

# Did you see the emoji bears in the code ? I tried to add them in the tooltip. It didn't work though. 


# I have many suggestions for the improvment of this section:
# 1. I would have fix the low number of observation first
# 2. Based on the curret occurences, I would have extracted the habitats for both species in the present 
# in order to compare it to predicted habitats in 2050.
# 3. Also, I would have refined the suitable habitats for both species by adding a buffer zone around big 
# cities for example. I also stated this limitation in the intermediate project: I would also 
# clean out observation made in zoos.
