rm(list = ls())
gc()

library(terra)
library(tidyterra)
library(ggplot2)
library(patchwork)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/MODIS/")
ele_step <- 5

datasets <- list(
  list(
    legend_title = "Mean SOD - mean \nSOD in 5m elevation \nband [days]",
    fname_dem     = "DEM/MODIS_dem.tif",
    fname_ave_los = "Datas/mean_maps/mean_sos.tif"
  ),
  list(
    legend_title = "Mean SED - mean \nSED in 5m elevation \nband [days]",
    fname_dem     = "DEM/MODIS_dem.tif",
    fname_ave_los = "Datas/mean_maps/mean_eos.tif"
  )
)



brks <- c(-Inf, -25, -15, -5, 0, 5, 15, Inf)
labs <- c("< -25", "-25 to -15", "-15 to -5", "-5 to 0", "0 - 5", "5 - 15", "> 15")
cols <- c("#d73027", "#fc8d59", "#fee090", "#ffffbf", "#e0e0e0", "#91bfdb", "#4575b4")
rcl_matrix <- cbind(brks[-length(brks)], brks[-1], seq_along(labs))



compute_los_residuals_class <- function(fname_dem, fname_ave_los, ele_step,
                                        rcl_matrix, labs) {
  dem      <- rast(fname_dem)
  mean_los <- rast(fname_ave_los)
  dem_masked <- mask(dem, mean_los)
  
  lims <- c(seq(from = -5, to = 4000, by = ele_step), 4700)
  class_dem <- classify(dem_masked, rcl = lims, include.lowest = TRUE)
  
  mean_los_by_ele <- zonal(mean_los, class_dem, fun = "mean",
                           na.rm = TRUE, as.raster = TRUE)
  los_residuals <- mean_los - mean_los_by_ele
  
  los_class <- classify(los_residuals, rcl = rcl_matrix,
                        include.lowest = TRUE, right = FALSE)
  levels(los_class) <- data.frame(id = seq_along(labs), class = labs)
  
  list(los_class = los_class, mean_los = mean_los)
}



plot_los_class <- function(los_class, italy_vect, palette, labels,
                           legend_name, plot_title = NULL) {
  p <- ggplot() +
    geom_spatraster(data = los_class, maxcell = Inf) +
    scale_fill_manual(
      values = palette,
      labels = labels,
      name = legend_name,
      na.value = "transparent",
      guide = guide_legend(
        title.position = "top",
        ncol = 2,
        byrow = FALSE,
        label.position = "right",
        keywidth = unit(0.4, "cm"),
        keyheight = unit(0.4, "cm")
      )
    ) +
    geom_spatvector(data = italy_vect, fill = NA, color = "black", linewidth = 0.3) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold", size = 25, vjust = 0.5),
      legend.text = element_text(size = 20),
      legend.spacing.x = unit(0.3, 'cm'),
      legend.spacing.y = unit(0.2, 'cm'),
      plot.margin = margin(5, 5, 5, 5)
    )
  
  if (!is.null(plot_title)) {
    p <- p + ggtitle(plot_title) +
      theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5))
  }
  p
}

italy_border <- geodata::gadm(country = "ITA", level = 0, path = tempdir())
plots <- lapply(datasets, function(ds) {
  res <- compute_los_residuals_class(ds$fname_dem, ds$fname_ave_los,
                                     ele_step, rcl_matrix, labs)
  italy_cropped <- crop(italy_border, ext(res$mean_los))
  plot_los_class(res$los_class, italy_cropped, cols, labs,
                 ds$legend_title)
})

combined <- wrap_plots(plots, ncol = length(plots)) + 
  plot_annotation(tag_levels = 'A') &
  theme(
    plot.tag = element_text(size = 12, face = "bold"),
    plot.tag.position = c(0.05, 0.95)
  )
print(combined)

# combined_shared <- wrap_plots(plots, ncol = length(plots), guides = "collect") &
#   theme(legend.position = "right")
# print(combined_shared)

# ggsave("los_residuals_dataset1_vs_dataset2.png", combined, width = 18, height = 10, dpi = 300)