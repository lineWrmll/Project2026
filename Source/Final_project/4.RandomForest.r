# Random Forest

# In order to project the distribution of my species into the future, I need to discriminate which 
# variable separate them the most. To do so, I will classify my species using environmental, spectral, 
# and spatial variables with a random forest analysis.
# For this analysis, U. arctos and U. a. horribilis are merged in one species. This is needed as they share almost the same
# ecological niche, the Random Forest analysis won't be able to discriminate them. Also, the number of observations of U. a. horribilis 
# was to low to conduct the analysis with three species instead of two (U. arctos and U. maritimus). 


# Installation and librarisation 

#install.packages("randomForest")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("caret")
#install.packages("vip")
#install.packages("pdp")
#install.packages("patchwork")

library(randomForest)
library(ggplot2)
library(dplyr)
library(caret)
library(vip)
library(pdp)
library(patchwork)


# Data preparation => drop of columns that are non ecologically meaningful
# Columns to EXCLUDE from predictors
id_cols <- c("species", "occurrence_id", "source", "date_obs", "color")

# All predictor variable names
predictor_vars <- c(
  "tmax_mean_c", "prec_mean_annual", "current_july_temp_c",
  "Temperatur", "Moisture", "Climate_Re",
  "future_july_2050_c", "july_temp_change_c",
  "elevation", "Landforms",
  "Landcover", "W_Ecosystm", "eco_values",
  "Red", "Green", "Blue", "NDVI",
  "latitude", "longitude"
)

# Loading of my matrix
df = read.csv("/Users/linewermeille/Desktop/Master Unine/git_repo_real/Project_2026/Data/matrix_full_clim_eco_elev.csv", header = TRUE)

# Species need to be a factor
df$species <- as.factor(df$species)

# Creation of the model
df_model <- df[, c("species", predictor_vars)]
df_model$species[df_model$species == "Ursus arctos horribilis"] <- "Ursus arctos"
df_model$species <- droplevels(df_model$species)  # remove the empty factor level

# Check for missing values
cat("Missing values per column:\n")
print(colSums(is.na(df_model))) # none, good thing

# Train (70/30) => the model is trained on 70% of the data while 30% remain untouched. 
set.seed(123)
train_idx  <- sample(seq_len(nrow(df_model)), size = 0.7 * nrow(df_model))
train_data <- df_model[ train_idx, ]
test_data  <- df_model[-train_idx, ]

cat(sprintf("\nTraining set: %d rows | Test set: %d rows\n",
            nrow(train_data), nrow(test_data)))


# Tuning of the algorythm 
best_mtry <- floor(sqrt(length(predictor_vars)))


# Training of the final Random Forest
set.seed(42)
rf_model <- randomForest(
  species ~ .,          # Prediction of the species from all other columns
  data       = train_data,
  ntree      = 500,     # Number of trees
  mtry       = best_mtry,
  importance = TRUE,    
  keep.forest = TRUE    # Keep the forest so I can predict later
)

print(rf_model) # OOB estimates of error rate = 1.6%
# Confusion matrix = good results


# Evaluation on the test set
pred_test <- predict(rf_model, newdata = test_data)

# Confusion matrix with sensitivity, specificity, kappa
cm <- confusionMatrix(pred_test, test_data$species)
print(cm) 

# Overall accuracy
cat(sprintf("\nTest accuracy: %.1f%%\n", cm$overall["Accuracy"] * 100)) # = 100%

# Accuracy of 100%, seems suspicious -> the model is maybe only associating latitude/longitude to the species and not 
# any ecological variables. To check this, I will rerun the model without spatiale variables


predictor_vars_nospatial <- setdiff(predictor_vars, c("latitude", "longitude"))

# Training of new forest without spatial variables
set.seed(42)
rf_nospatial <- randomForest(
  x          = train_data[, predictor_vars_nospatial],
  y          = train_data$species,
  ntree      = 500,
  mtry       = floor(sqrt(length(predictor_vars_nospatial))),
  importance = TRUE
)

# Print OOB error
print(rf_nospatial) # Same results as the previous model

# Predict on test set
pred_nospatial <- predict(rf_nospatial, newdata = test_data[, predictor_vars_nospatial])

# Confusion matrix
cm_nospatial <- confusionMatrix(pred_nospatial, test_data$species)
print(cm_nospatial) # Still the same results, this means the model doesn't need to know where the observation 
# was made in order to predict the species, it can predict the right species based on other variables. 

# Comparison of models with and without the spatial variables
cat(sprintf("Full model (with lat/lon) accuracy : %.1f%%\n",
            cm$overall["Accuracy"] * 100)) # => full model accuracy = 100%
cat(sprintf("Reduced model (no lat/lon) accuracy: %.1f%%\n",
            cm_nospatial$overall["Accuracy"] * 100)) # => full model accuracy = 100%

# Feature importance without spatial variables
importance_nospatial <- as.data.frame(importance(rf_nospatial))
importance_nospatial$Variable <- rownames(importance_nospatial)

# Plot importance without spatial variables
x11()
p_nospatial <- importance_nospatial %>%
  arrange(MeanDecreaseAccuracy) %>%
  mutate(Variable = factor(Variable, levels = Variable)) %>%
  ggplot(aes(x = MeanDecreaseAccuracy, y = Variable)) +
  geom_col(fill = "#2E86AB", alpha = 0.85) +
  labs(
    title    = "Feature Importance — No Spatial Variables",
    subtitle = "Ecological signal without lat/lon",
    x        = "Mean Decrease in Accuracy",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

print(p_nospatial)


# Feature Importance
importance_df          <- as.data.frame(importance(rf_model))
importance_df$Variable <- rownames(importance_df)

p_mda <- importance_df %>%
  arrange(MeanDecreaseAccuracy) %>%
  mutate(Variable = factor(Variable, levels = Variable)) %>%
  ggplot(aes(x = MeanDecreaseAccuracy, y = Variable)) +
  geom_col(fill = "#2E86AB", alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  labs(
    title    = "Feature Importance — Mean Decrease in Accuracy",
    subtitle = "How much model accuracy drops when each variable is permuted",
    x        = "Mean Decrease in Accuracy",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

p_mdg <- importance_df %>%
  arrange(MeanDecreaseGini) %>%
  mutate(Variable = factor(Variable, levels = Variable)) %>%
  ggplot(aes(x = MeanDecreaseGini, y = Variable)) +
  geom_col(fill = "#A23B72", alpha = 0.85) +
  labs(
    title    = "Feature Importance — Mean Decrease in Gini",
    subtitle = "Contribution to node purity across all trees",
    x        = "Mean Decrease in Gini",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

combined_importance <- p_mda + p_mdg
x11(); print(combined_importance)

ggsave("Outputs/Final_Project/3.Random_Forest/3.1.Feature_importance.png",
       combined_importance, width = 14, height = 7, dpi = 300)


# Top 4 Continuous Variables by MDA
top4_vars <- importance_df %>%
  arrange(desc(MeanDecreaseAccuracy)) %>%
  filter(sapply(Variable, function(v) is.numeric(train_data[[v]]))) %>%
  slice(1:4) %>%
  pull(Variable)

cat("\nTop 4 discriminating continuous variables (MDA):\n")
print(top4_vars)

# Partial Dependence Plots
logit_to_prob <- function(x) x

species_colors <- c(
  "Ursus arctos"    = "#D2691E",
  "Ursus maritimus" = "#0b82e9"
)

pdp_plots_v2 <- lapply(top4_vars, function(var) {

  pd_arctos    <- partial(rf_model, pred.var = var, which.class = 1,
                          plot = FALSE, train = train_data)
  pd_maritimus <- partial(rf_model, pred.var = var, which.class = 2,
                          plot = FALSE, train = train_data)

  pd_combined <- data.frame(
    x           = c(pd_arctos[[var]],    pd_maritimus[[var]]),
    probability = c(logit_to_prob(pd_arctos$yhat), logit_to_prob(pd_maritimus$yhat)),
    species     = c(rep("Ursus arctos",    nrow(pd_arctos)),
                    rep("Ursus maritimus", nrow(pd_maritimus)))
  )

  label_df <- pd_combined %>%
    group_by(species) %>%
    slice_max(x, n = 1) %>%
    ungroup()

  ggplot(pd_combined, aes(x = x, y = probability, colour = species)) +
    geom_line(linewidth = 1.2) +
    geom_rug(data = train_data %>% filter(species == "Ursus arctos"),
             aes(x = .data[[var]]), inherit.aes = FALSE,
             colour = species_colors["Ursus arctos"],
             alpha = 0.4, sides = "b") +
    geom_rug(data = train_data %>% filter(species == "Ursus maritimus"),
             aes(x = .data[[var]]), inherit.aes = FALSE,
             colour = species_colors["Ursus maritimus"],
             alpha = 0.6, sides = "t") +
    geom_hline(yintercept = 0.5, linetype = "dashed",
               colour = "grey50", linewidth = 0.5) +
    annotate("text", x = min(pd_combined$x), y = 0.52,
             label = "decision boundary", hjust = 0,
             size = 3, colour = "grey50") +
    geom_text(data = label_df,
              aes(x = x, y = probability, label = species, colour = species),
              hjust       = -0.05,
              fontface    = "italic",
              size        = 3.5,
              inherit.aes = FALSE) +
    scale_colour_manual(values = species_colors) +
    scale_y_continuous(limits = c(0, 1),
                       labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.25))) +
    labs(title = var, x = var, y = "Predicted probability") +
    theme_minimal(base_size = 11) +
    theme(legend.position  = "none",
          panel.grid.minor = element_blank(),
          plot.title       = element_text(face = "bold"))
})

pdp_final <- wrap_plots(pdp_plots_v2, ncol = 2)
x11(); print(pdp_final)

ggsave("Outputs/Final_Project/3.Random_Forest/3.2.Partial_dependence_top4.png",
       pdp_final, width = 15, height = 9, dpi = 300)



# ------- Interpretation of Partial dependance -------

# 1. Longitude
# Longitude is acting as a major geographic discriminator. The model associates western 
# Canada with U. arctos and eastern/northeastern Arctic regions with U. maritimus. 
# This likely reflects broad spatial patterns rather than a direct biological effect of 
# longitude itself.

# 2. Elevation
# The model associates strongly higher elevations with U. arctos habitat and low-lying 
# terrain with U. maritimus habitat. This is ecologically plausible because U. maritimus is
# closely linked to coastal and sea-ice environments, which occur at low elevations. 

# 3. Annual mean precipitation
# The model identifies wetter environments as strongly associated with U. arctos occurrence. 
# U. maritimus are predicted primarily in relatively dry regions, which is relfecting the low 
# precipitation characteristic of Arctic climates.

# 4. NDVI
# The model strongly associates vegetated landscapes with U. arctos and sparsely vegetated 
# environments with U. maritimus. This is consistent with the ecology of both species, 
# since U. maritimus occupies Arctic habitats with limited plant productivity, while U. arctos use 
# more productive terrestrial ecosystems. 



# Display in onw panel
final_panel <- p_nospatial / (p_mda + p_mdg) /
               (pdp_plots_v2[[1]] + pdp_plots_v2[[2]]) /
               (pdp_plots_v2[[3]] + pdp_plots_v2[[4]]) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "",
    tag_suffix = ")",
    theme      = theme(plot.tag = element_text(face = "bold", size = 12))
  )

x11(); print(final_panel)

ggsave(
  "Outputs/Final_Project/4.Panels/4.2.Panel_RF.png",
  final_panel,
  width  = 16,
  height = 22,
  dpi    = 300,
  bg     = "white"
)