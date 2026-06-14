# PCA analysis 

# I conducted a PCA analysis to visualize the environmental space occupied by the three species in my dataset. 


library(vegan)
library(ggplot2)
df = read.csv("/Users/linewermeille/Desktop/Master Unine/git_repo_real/Project_2026/Data/matrix_full_clim_eco_elev.csv", header = TRUE)

names(df)


# Select only the columns with the ecological variables
env = df[, c("tmax_mean_c", "prec_mean_annual", "current_july_temp_c", "elevation", "NDVI")]
spec = df[, c("species")]

# Remove rows with NA values
env_clean <- na.omit(env)
spec_clean <- spec[complete.cases(env)]

# Perform PCA
pca_result <- prcomp(env_clean, scale. = TRUE)

# Check if PC1 and PC2 explain at least 50% of the variance
summary(pca_result) # Yes they do


# Extract coordinates
pca_coords <- as.data.frame(pca_result$x)
pca_coords$species <- spec_clean

# Extract arrows (variable loadings)
loadings <- as.data.frame(pca_result$rotation)
loadings$variable <- rownames(loadings)

# Scale arrows to fit the plot
scale_factor <- 3
loadings$PC1_scaled <- loadings$PC1 * scale_factor
loadings$PC2_scaled <- loadings$PC2 * scale_factor

# Plot

x11()
pca_plot <- ggplot() +
  # dashed center lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  # points colored by species
  geom_point(data = pca_coords, aes(x = PC1, y = PC2, color = species), 
             size = 3, alpha = 0.7) +
  # ellipses per species
  stat_ellipse(data = pca_coords, aes(x = PC1, y = PC2, color = species)) +
  # arrows in black
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1_scaled, yend = PC2_scaled),
               arrow = arrow(length = unit(0.3, "cm")), color = "black", linewidth = 0.7) +
  # variable labels in black
  geom_text(data = loadings, aes(x = PC1_scaled * 1.1, y = PC2_scaled * 1.1, label = variable),
            color = "black", size = 3.5) +
  # species colors and italic labels
  scale_color_manual(values = c("steelblue", "tomato", "forestgreen"),
                     labels = c(expression(italic("Ursus arctos")),
                                expression(italic("Ursus arctos horribilis")),
                                expression(italic("Ursus maritimus")))) +
  theme_minimal() +
  labs(title = "PCA - Environmental Space by Species",
       x = "PC1 (54%)",
       y = "PC2 (22%)",
       color = "Species")

# Save to your project folder
ggsave("/Users/linewermeille/Desktop/Master Unine/git_repo_real/Project_2026/Outputs/Final_Project/1.PCA/pca_plot.png", plot = pca_plot, width = 8, height = 6, dpi = 300, bg = "white")


# ------------------------------
# Interpretation of the PCA plot

# PC1 and PC2 together explain 76% of the total variance (PC1 = 54%, PC2 = 22%).
# This indicates that a large proportion of the dataset is captured by the biplot, and that the first 
# two principal components are sufficient to visualize the main patterns in the data.
# PC1 is mainly influenced by mean maximum temperature and NDVI, which are correlate with each other. 
# PC2 is mainly influenced by elevation and annual mean precipitation. 
# While elevation is negatively correlated to mean temperatures in July. This makes sense as elevation 
# increases, temperature decreases. 

# U. arctos presents a wide point distribution, suggesting that it occupies a rather large environmental condition range,
# and therefore a large ecological niche (as it is widely dispersed on PC1 and PC2). 
# U. arctos horribilis distribution overlaps with U. arctos. This suggests that U. arctos horribilis presents a 
# similar ecological niche as U. arctos. This makes sense as it is a sub-species of 
# U.arctos. However, the points are slightly more centered. 
# On the contrary, U. maritimus shows points clearly oriented toward the positive values of PC2. Ellipse for 
# U. maritimus shows almost no overlap with the other ellipses, suggesting that the species presents a different
# ecological niche. 

# Temperature in July, precipitation and elevation variables are the ones that best differentiate the niches of 
# brown bears and polar bears. 
# Indeed, brown bears are more associated with higher temperatures in July, lower elevation and higher precipitation.
# While polar bears are more associated with lower temperatures in July, higher elevation and lower precipitation.
