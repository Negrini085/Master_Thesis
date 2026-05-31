# The main goal of this script is to plot the mean SCD values for Venice region
# and faulty pixels, in order to visually check if there is a sensible jump in SCD
# values across adjacent pixels.
rm(list = ls())
gc()

library(sf) 
library(ncdf4)
library(ggplot2) 
library(rnaturalearth) 

library(grid) 
library(gtable) 
library(gridExtra) 

get_legend <- function(p){ 
  g <- ggplotGrob(p) 
  gtable_filter(g, "guide-box") 
}

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD-from-SWE")
fname <- "Datas/mean_SCD_map.nc"



# Importing netCDF file with mean SCD map and DEM model (necessary for pixel localization)
nc <- nc_open(fname)
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")
scd <- ncvar_get(nc, "SCD")
nc_close(nc)

dem <- rast("../DEM/DEM_region.tif")
dem <- as.matrix(dem, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)



# Creating mask in order to locate faulty pixel and clip correct sub-region
mask <- scd > 30 & scd < 200 & dem < 50

faulty_lims <- which(mask, arr.ind = TRUE)
lon_min <- min(faulty_lims[, 1])
lon_max <- max(faulty_lims[, 1])
lat_min <- min(faulty_lims[, 2])
lat_max <- max(faulty_lims[, 2])

lat <- lat[lat_min : lat_max]
lon <- lon[lon_min : lon_max]
region_scd <- scd[lon_min:lon_max, lat_min:lat_max]
region_scd[region_scd == 0] <- NA

scd[scd < 30] <- NA
faulty_scd <- scd[lon_min:lon_max, lat_min:lat_max]



# Creating two layer tensor in order to be able to fill a two subplot figure
appo <- array(0, dim = c(length(lon), length(lat), 2))
appo[, , 1] <- region_scd
appo[, , 2] <- faulty_scd



# Plotting procedure
grid_df <- expand.grid(lon = lon, lat = lat)
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf") 
titles <- c("Mean SCD", "Faulty SCD")

lims <- c(1, max(appo[, , 2]))

plots <- vector("list", length(titles)) 
for (i in 1:length(titles)) {
  pippo <- appo[,,i]
  grid_df$scd_masked <- as.vector(pippo)
  
  p <- ggplot() +
    geom_sf(data = europe, fill = "grey90", color = "black", linewidth = 0.2) +
    coord_sf(xlim = c(lon[1], lon[length(lon)]), ylim = c(lat[1], lat[length(lat)])) +
    geom_raster(data = grid_df, aes(x = lon, y = lat, fill = scd_masked)) +
    scale_fill_viridis_c( option = "C", limits = lims, na.value = "transparent", oob = scales::squish) +
    labs(title = titles[i], x = NULL, y = NULL, fill = "SCD") +
    theme_minimal(base_size = 10) +
    theme( plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "right" )
  
  plots[[i]] <- p 
} 

legend <- get_legend(plots[[1]]) 
plots_noleg <- lapply(plots, \(p) p + theme(legend.position = "none")) 
panel_grid <- arrangeGrob(grobs = plots_noleg, nrow = 1, ncol = 2) 

title_grob <- textGrob(
  "Venice sub-region: faulty pixels in mean SCD map",
  gp = gpar(fontsize = 16, fontface = "bold")
)

final <- arrangeGrob(
  panel_grid,
  legend,
  ncol = 2,
  widths = c(12, 1.2),
  top = title_grob
)

grid.newpage()
grid.draw(final)