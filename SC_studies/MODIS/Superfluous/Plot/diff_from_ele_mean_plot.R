# The main goal of this script is to check whether some LOS differences from a 5 meter
# elevation window are present across the Italian territory.
rm(list = ls())
gc()

library(terra)
library(tidyterra)
library(ggplot2)

fname_dem <- "DEM/MODIS_dem.tif"
fname_ave_los <- "Datas/mean_maps/mean_los.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/MODIS/")



# Importing DEM and average LOS map to assess pixel frequency on elevation steps
dem <- rast(fname_dem)
mean_los <- rast(fname_ave_los)
dem_masked <- mask(dem, mean_los)



# Classifying DEM pixels based on their elevation
ele_step <- 5
lims <- c(seq(from = -5, to = 4000, by = ele_step), 4700)
class_dem <- classify(dem_masked, rcl = lims, include.lowest = TRUE)



# Evaluating mean LOS for a given altitude range and creating a new raster
mean_los_by_ele <- zonal(mean_los, class_dem, fun = "mean", na.rm = TRUE, as.raster = TRUE)
los_residuals <- mean_los - mean_los_by_ele



# Potting procedure
brks <- c(-Inf, -40, -20, -5, 0, 5, 20, Inf)
labs <- c("< -40", "-40 to -20", "-20 to -5", "-5 to 0", "0 - 5", "5 - 20", "> 20")
cols <- c("#d73027", "#fc8d59", "#fee090", "#ffffbf", "#e0e0e0", "#91bfdb", "#4575b4")

rcl_matrix <- cbind(brks[-length(brks)], brks[-1], seq_along(labs))
los_class <- classify(los_residuals, rcl = rcl_matrix, include.lowest = TRUE, right = FALSE)
levels(los_class) <- data.frame(id = seq_along(labs), class = labs)

italy_border <- geodata::gadm(country = "ITA", level = 0, path = tempdir())
italy_cropped <- crop(italy_border, ext(mean_los))

legend_title <- paste0("Mean SCD - mean \n SCD in 5m elevation \n bins [days]")

p <- ggplot() +
  geom_spatraster(data = los_class, maxcell = Inf) +
  scale_fill_manual(
    values = cols,
    labels = labs,
    name = legend_title,
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
  geom_spatvector(data = italy_cropped, fill = NA, color = "black", linewidth = 0.3) +
  theme_void() +
  theme(
      legend.position = "right",
      legend.direction = "horizontal", 
      legend.title = element_text(face = "bold", size = 25, vjust = 0.5),
      legend.text = element_text(size = 20),
      legend.spacing.x = unit(0.3, 'cm'), 
      legend.spacing.y = unit(0.2, 'cm'),
      plot.margin = margin(5, 5, 5, 5)
  )

print(p)
# ggsave("los_residuals_map.png", p, width = 10, height = 10, dpi = 300)