# We already localized faulty pixels in Comacchio valleys. We should now focus on
# 2011 and 2013 years, which look suspicious. I would like to make a plot containing
# four subplots. The first row would be concerning 2011, whilst the second one 2013.
# The left panel should contain SCD values for the whole region and the other one 
# should show only pixels identified as faulty.
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
fname_season <- "Datas/season_maps_SCD.nc"
fname_mean <- "Datas/mean_SCD_map.nc"



# First thing first we need to import annual and mean SCD maps, as well as a 
# DEM model, in order to mask pixels
nc <- nc_open(fname_mean)
mean_scd <- ncvar_get(nc, "SCD")
nc_close(nc)

nc <- nc_open(fname_season)
lat <- ncvar_get(nc, "lat") 
lon <- ncvar_get(nc, "lon") 
scd <- ncvar_get(nc, "SCD") 
nc_close(nc) 

demR <- rast("../DEM/DEM_region.tif")
dem <- as.matrix(demR, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)



# Masking values and selecting correct data ranges
mask <- mean_scd > 30 & mean_scd < 200 & dem < 50
faulty_lims <- which(mask, arr.ind = TRUE)
lon_min <- min(faulty_lims[, 1])
lon_max <- max(faulty_lims[, 1])
lat_min <- min(faulty_lims[, 2])
lat_max <- max(faulty_lims[, 2])

lat <- lat[lat_min:lat_max]
lon <- lon[lon_min:lon_max]

appo <- array(0, dim = c(length(lon), length(lat), 4))
appo[, , 1] <- scd[lon_min:lon_max, lat_min:lat_max, 1]
appo[, , 3] <- scd[lon_min:lon_max, lat_min:lat_max, 3]

filter_map <- scd[, , 1]
filter_map[!mask] <- NA
appo[, , 2] <- filter_map[lon_min:lon_max, lat_min:lat_max]

filter_map <- scd[, , 3]
filter_map[!mask] <- NA
appo[, , 4] <- filter_map[lon_min:lon_max, lat_min:lat_max]

appo[appo == 0] <- NA



# Plotting procedure
grid_df <- expand.grid(lon = lon, lat = lat)
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf") 
titles <- c(
  "Mean SCD 2011", "Faulty pixels 2011",
  "Mean SCD 2013", "Faulty pixels 2013"
)

lims <- c(1, max(appo))
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
panel_grid <- arrangeGrob(grobs = plots_noleg, nrow = 2, ncol = 2) 

title_grob <- textGrob(
  "Venice: faulty pixels during suspicious years",
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