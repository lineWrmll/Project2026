# Density plots

# I will conduct density plots for the environmental variables to see how much species differ in their distribution along the 
# environmental gradients. 
# I will keep the subspecies U. arctos horribilis in the analysis to see if it is significantly different from U. arctos or 
# show some specificities in terms of ecological needs. 

matrix_full <- read.csv("data/matrix_full_clim_eco_elev.csv")

names(env_clean)


# ------- Density plot for elevation -------
#x11()
p1 <- ggplot(df, aes(x = elevation, fill = species)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    values = c("#8B4513", "#D2691E", "#0b82e9"),
    labels = c(expression(italic("Ursus arctos")),
               expression(italic("Ursus arctos horribilis")),
               expression(italic("Ursus maritimus")))
  ) +
  theme_minimal() +
  labs(title = "Elevation Distribution by Species",
       x = "Elevation (m)",
       y = "Density",
       fill = "Species")


outdir <- "Outputs/1.PCA"
if (!dir.exists(outdir)) {
  dir.create(outdir, recursive = TRUE)
}

ggsave(
  filename = file.path(outdir, "PCA_plot.png"),
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)


# ------- Interpretation of elevation density plot -------

# The density plot for elevation shows that U. arctos and U. arctos horribilis have a similar distribution 
# along the elevation gradient, with peaks around 500-1000 meters. This suggests that both species are commonly 
# found in low to mid-elevation habitats. 

# On the contrary, U. maritimus shows a very distinct distribution, with a peak at much lower elevations (close to sea level) 
# and almost no presence above 500 meters. This is consistent with the fact that polar bears are typically found in 
# coastal and Arctic environments, which are generally at low elevations.



# ------- Density plot for mean maximum temperature -------
#x11()
p2 <- ggplot(df, aes(x = tmax_mean_c, fill = species)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    values = c("#8B4513", "#D2691E", "#0b82e9"),
    labels = c(expression(italic("Ursus arctos")),
               expression(italic("Ursus arctos horribilis")),
               expression(italic("Ursus maritimus")))
  ) +
  theme_minimal() +
  labs(title = "Maximum Temperature Distribution by Species",
       x = "Mean Maximum Temperature (°C)",
       y = "Density",
       fill = "Species")

ggsave("/Users/linewermeille/Desktop/Master Unine/git_repo_real/Project_2026/Outputs/Final_Project/2.Density_plots/2.2.tmax_mean_c_density.png", plot = p2, width = 8, height = 6, dpi = 300, bg = "white")

# ------- Interpretation of mean maximum temperature density plot -------

# The density plot for mean maximum temperature shows that U. arctos and U. arctos horribilis have a similar distribution 
# along the temperature gradient, with peaks around 15-25°C. This makes sense as they are both found in habitats with moderate temperatures. 

# U. a. horribilis shows a slender and higher peak compared to U. arctos. This suggests that U. a. horribilis presents a
# temperature range slithly more restricted than U. arctos. 

# Intrestingly, U. maritimus shows two peaks, one around 0-5°C and another around 10-15°C. 
# This can be explained by the fact that polar bears are following the sea ice cicle, which presents a strong seasonal variation
# Indeed, they pass winter on the sea ice and then move on land during summer, where they can experience higher temperatures. 
# The two peaks could correspond to observations made during winter (0-5°C) and summer (10-15°C).

# Another explanation could be that the data for U. maritimus were taken with a wide difference in latitudes, corresponding to 
# different populations of polar bears experiencing different temperature regimes. 
# Another explanation could be that observations were made on U. maritimus populations from different regions, 
# some of which experience colder temperatures, for example those in the high Arctic, while others experience milder temperatures, 
# for example those in the southern range of the species. 

# A third explanation could be that data is made up of old observations and more recent ones, reflecting global warming.



# ------- Density plot for mean annual precipitation -------
#x11()
p3 <- ggplot(df, aes(x = prec_mean_annual, fill = species)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    values = c("#8B4513", "#D2691E", "#0b82e9"),
    labels = c(expression(italic("Ursus arctos")),
               expression(italic("Ursus arctos horribilis")),
               expression(italic("Ursus maritimus")))
  ) +
  theme_minimal() +
  labs(title = "Annual Precipitation Distribution by Species",
       x = "Mean Annual Precipitation (mm)",
       y = "Density",
       fill = "Species")
ggsave("/Users/linewermeille/Desktop/Master Unine/git_repo_real/Project_2026/Outputs/Final_Project/2.Density_plots/2.3.prec_mean_annual_density.png", plot = p3, width = 8, height = 6, dpi = 300, bg = "white")

# ------- Interpretation of mean annual precipitation density plot -------

# The density plot for mean annual precipitation shows that U. arctos and U. arctos horribilis have a similar distribution.
# Both species show a peak around 500-1000 mm of precipitation, suggesting that they are commonly found in habitats with moderate precipitation levels.

# Regarding U. maritimus, the density plots shows two narrow peaks 250 mm and 750 mm of precipitation. This generaly low precipitation
# rate is consistent as U. maritimus lives in Arctic environments, where precipitation are usually low (arctic desert). 
# The fact that the two peaks are close to each other, suggests that this pattern is likely due to the regions where observations
# where made (lower precipitation in high Arctic regions for example), rather than seasonal variation like the temperature plot above. 


# ------- Density plot for July temperatures -------
#x11()
p4 <- ggplot(df, aes(x = current_july_temp_c, fill = species)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    values = c("#8B4513", "#D2691E", "#0b82e9"),
    labels = c(expression(italic("Ursus arctos")),
               expression(italic("Ursus arctos horribilis")),
               expression(italic("Ursus maritimus")))
  ) +
  theme_minimal() +
  labs(title = "July Temperature Distribution by Species",
       x = "Mean July Temperature (°C)",
       y = "Density",
       fill = "Species")
ggsave("/Users/linewermeille/Desktop/Master Unine/git_repo_real/Project_2026/Outputs/Final_Project/2.Density_plots/2.5.July_temp_density.png", plot = p4, width = 8, height = 6, dpi = 300, bg = "white")


# ------- Interpretation of July temperatures density plot -------
# U. arctos and U. a. horribilis present a similar values for mean July temperatures, with peaks around 12-17°C. 
# U. maritimus shows two peaks, one around 10°C and another arount 20°C. The first peak can be explaiend by observations
# made in summer in tundra or coastal areas, when polar bears are on land. However, the second peak is not expected for this species.
# This could be explained by quality issues with the origin of datasets (Inat and GBIF). This is possible as Inat and GBIF relies on 
# citizen science data, which can be less accurate than data from scientific surveys. 
# Also, some observations might have been made in zoos where the temperature might be higher than in the wild. 
# A suggestion to get a better pattern would be to filter the data for U. maritimus to keep only observations made in the 
# wild. 


# ------- Density plot for NDVI -------
#x11()
p5 <- ggplot(df, aes(x = NDVI, fill = species)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    values = c("#8B4513", "#D2691E", "#0b82e9"),
    labels = c(expression(italic("Ursus arctos")),
               expression(italic("Ursus arctos horribilis")),
               expression(italic("Ursus maritimus")))
  ) +
  theme_minimal() +
  labs(title = "NDVI Distribution by Species",
       x = "NDVI",
       y = "Density",
       fill = "Species")

ggsave("/Users/linewermeille/Desktop/Master Unine/git_repo_real/Project_2026/Outputs/Final_Project/2.Density_plots/2.4.NDVI_density.png", plot = p5, width = 8, height = 6, dpi = 300, bg = "white")


# ------- Interpretation of NDVI density plot -------

# The density plot for NDVA show again that U. arctos and U. arctos horribilis have similar distribution, with both peaks being around 0.7. 
# This value is consistent with the fact that both species are commonly found in forested habitats, which typically have high NDVI values.

# For U. maritimus, the density plot shows an interesting pattern. Indeed, the peak shows a value around 0.5, which is unexpectedly high 
# for a species living in Arctic environments, as this environemnt is generally characterized by low vegetation cover and therefore low 
# NDVI values. However, this could be explained by the hypothesis that observations were taken also during summer, when U. maritimus is 
# found on land, in areas with higher vegetation cover, such as tundra or coastal areas with some vegetation. 



# ------- Radar plot for environmental profiles -------

library(dplyr)
library(fmsb)

# Combine species vector and environmental variables into one data frame
env_data <- cbind(spec, env_clean)

env_data <- matrix_full %>%
  select(spec     = species,
         tmax_mean_c,
         prec_mean_annual,
         current_july_temp_c,
         elevation,
         NDVI)

# Calculate the mean of each environmental variable per species
species_means <- env_data %>%
  group_by(spec) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

# Convert to a standard data frame
radar_df <- as.data.frame(species_means)

# Set species names as row names
rownames(radar_df) <- radar_df$spec

# Remove the species name column (now stored as row names)
radar_df$spec <- NULL

# Rename columns to understand them directly
colnames(radar_df) <- c("Max Temp (°C)", 
                         "Precip (mm)", 
                         "July Temp (°C)", 
                         "Elevation (m)", 
                         "NDVI")

# Order rows to match color order: arctos, horribilis, maritimus
radar_df <- radar_df[c("Ursus arctos", 
                        "Ursus arctos horribilis", 
                        "Ursus maritimus"), ]

# Z-score normalization centers each variable around 0 -> all variables are on the same scale regardless of their unit
radar_scaled_z <- as.data.frame(scale(radar_df))

# Calculate a buffer of 20% of the range above and below -> no species polygon sits exactly at the edge of the chart
buffer <- apply(radar_scaled_z, 2, function(x) (max(x) - min(x)) * 0.2)

# Build the final radar data frame with max and min rows 
radar_plot_df <- rbind(
  max = apply(radar_scaled_z, 2, max) + buffer,  
  min = apply(radar_scaled_z, 2, min) - buffer,  
  radar_scaled_z                                 
)

# Define my colors
colors_line <- c("#8B4513", "#D2691E", "#0b82e9")

# Same as above but transparent (last value = opacity)
colors_fill <- c(rgb(0.55, 0.27, 0.07, 0.2),   # U. arctos
                 rgb(0.82, 0.41, 0.12, 0.2),    # U. a. horribilis
                 rgb(0.04, 0.51, 0.91, 0.2))    # U. maritimus


# Save the plot
png("/Users/linewermeille/Desktop/Master Unine/git_repo_real/Project_2026/Outputs/Final_Project/2.Density_plots/2.6.Radar_plot.png", 
    width = 1000,   
    height = 800,   
    res = 150,      
    bg = "white")   



par(mar = c(5, 2, 5, 10), xpd = TRUE)

# Draw the radar chart
radarchart(
  radar_plot_df,
  axistype = 1,          
  pcol = colors_line,    
  pfcol = colors_fill,   
  plwd = 2,              
  cglcol = "grey80",     
  vlcex = 0.85,
  axislabcol = "black"  # axis scale labels in black
)

# Add title 
mtext("Environmental Profile by Species", 
      side = 3,    
      line = 3,    
      cex = 1.2,   
      font = 2)    

# Add legend
legend(x = 1.4, y = 0.5,
       legend = expression(italic("Ursus arctos"),           # italic species names
                           italic("Ursus arctos horribilis"),
                           italic("Ursus maritimus")),
       col = colors_line,   
       lty = 1,             
       lwd = 2,             
       bty = "n",           
       cex = 0.85)          


dev.off()


# ------ Interpretation of the radar plot ------

# Each axis is a z-score normalized environmental variable. 0 si the average across all three groups. Positive values
# indicate above-average conditions and negative values indicate below-average conditions for that variable. 

# Not suprisingly, U. arctos and U. a. horribilis share almost the same ecological profil. The only noticable difference is
# for the precipitation with a decrease fo 25% for U. arctos compared to U. a. horribilis. This can be related to where (region)
# and when (season) the data were taken, or reflect a real preference between the two sub species. This would be interesting 
# to investigate further. 

# U. maritimus show a clear distinct ecological profil. This makes sense with the ecology of the species as described above with the
# density plots except for the July temperatures axis. As mentioned with the density plot, the high value for the species is 
# likely due to incorrect data or data taken in zoos. To get a better profil, these observations would need to get filtered. 



# Panel displacement
# The Radar chart is not compatible with ggplot. It needs to be converted in another
# object before to appear on the final panel.

# Draw the radar chart to a temporary device and record it
dev.new()
par(mar = c(5, 2, 5, 10), xpd = TRUE)
radarchart(
  radar_plot_df,
  axistype   = 1,
  pcol       = colors_line,
  pfcol      = colors_fill,
  plwd       = 2,
  cglcol     = "grey80",
  vlcex      = 0.85,
  axislabcol = "black"
)
legend(x = 1.4, y = 0.5,
       legend = expression(italic("Ursus arctos"),
                           italic("Ursus arctos horribilis"),
                           italic("Ursus maritimus")),
       col = colors_line,
       lty = 1, lwd = 2, bty = "n", cex = 0.85)
p6_recorded <- recordPlot()
dev.off()

# Convert recorded base plot to a ggplot-compatible grob
library(cowplot)

p6 <- ggdraw() + draw_grob(grid::rasterGrob(
  {
    tmp <- tempfile(fileext = ".png")
    png(tmp, width = 800, height = 800, res = 150)
    par(mar = c(5, 2, 5, 10), xpd = TRUE)
    radarchart(
      radar_plot_df,
      axistype   = 1,
      pcol       = colors_line,
      pfcol      = colors_fill,
      plwd       = 2,
      cglcol     = "grey80",
      vlcex      = 0.85,
      axislabcol = "black"
    )
    legend(x = 1.4, y = 0.5,
           legend = expression(italic("Ursus arctos"),
                               italic("Ursus arctos horribilis"),
                               italic("Ursus maritimus")),
           col = colors_line,
           lty = 1, lwd = 2, bty = "n", cex = 0.85)
    dev.off()
    png::readPNG(tmp)
  }
))

# Now assemble the panel
panel <- plot_grid(
  p1, p2, p3, p4, p5, p6,
  labels = c("a)", "b)", "c)", "d)", "e)", "f)"),
  ncol   = 2,
  align  = "hv"
)

final_plot <- ggdraw() + draw_plot(panel)
final_plot

ggsave(
  "Outputs/Final_Project/4.Panels/4.1.Density_Panel.png",
  final_plot,
  width  = 15,
  height = 15,
  dpi    = 300,
  bg     = "white"
)







# First, the cart plot is converted to be compatible with ggplot
library(grid)

p6 <- grid::grid.grabExpr(
  radarchart(
    radar_plot_df,
    axistype = 1,
    pcol = colors_line,
    pfcol = colors_fill,
    plwd = 2,
    cglcol = "grey80",
    vlcex = 0.85,
    axislabcol = "black"
  )
)

library(cowplot)

panel <- plot_grid(
  p1, p2, p3, p4, p5, p6,
  labels = c("a)", "b)", "c)", "d)", "e)", "f)"),
  ncol = 2,
  align = "hv"
)

final_plot <- ggdraw() +
  draw_plot(panel)

final_plot

ggsave(
  "Outputs/Final_Project/4.Panels/4.2.Panel.png",
  final_plot,
  width = 15,
  height = 15,
  dpi = 300,
  bg = "white"
)