
#This code is to save all the plots in Outputs/Plots
save_plot <- function(plot, filename,
                      folder = "Plots",
                      width = 8,
                      height = 6,
                      dpi = 300) {

  plot_dir <- file.path("Outputs", folder)

  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

  ggsave(
    filename = file.path(plot_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
}

#This code is to save plots with raster
save_base_plot <- function(expr, filename,
                           folder = "Plots",
                           width = 2000,
                           height = 1500,
                           res = 300) {

  dir.create(file.path("Outputs", folder),
             recursive = TRUE,
             showWarnings = FALSE)

  png(file.path("Outputs", folder, filename),
      width = width,
      height = height,
      res = res)

  eval(substitute(expr))

  dev.off()
}

source("./Source/1.matrix_full.r")

source("./Source/2.climate.r")

source("./Source/3.ecosystems.r")

source("./Source/4.elevation.r")

source("./Source/5.sat_manual.r")
