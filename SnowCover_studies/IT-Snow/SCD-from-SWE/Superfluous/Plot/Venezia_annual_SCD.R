# The main goal of this script is to plot Venice region and its SCD values across
# the investigated period. We expect to have values close to 100 for the first 
# year and then 365 (or 366 in case of a leap year) for the following ones.
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

# Here we have to open the mean SCD map in order to understand which pixel we want
# to plot, in order to then select them in annual snow cover maps.
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

# Selecting Venice region (focus of our study --> full of faulty pixels)
mask <- mean_scd > 30 & mean_scd < 200 & dem < 50
appo <- which(mask, arr.ind = TRUE)
lon_min <- min(appo[, 1])
lon_max <- max(appo[, 1])
lat_min <- min(appo[, 2])
lat_max <- max(appo[, 2])

lat <- lat[lat_min:lat_max]
lon <- lon[lon_min:lon_max]

for(i in 1:15){
  appo <- scd[, , i]
  appo[!mask] <- NA
  scd[, , i] <- appo
}
scd <- scd[lon_min:lon_max, lat_min:lat_max, ]
scd[scd == 0] <- NA

grid_df <- expand.grid(lon = lon, lat = lat)
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf") 
titles <- c(
  "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018",
  "2019", "2020", "2021", "2022", "2023", "2024", "2025"
)

scd[scd == 0] <- NA 
lims <- c(1, 365)

plots <- vector("list", length(titles)) 
for (i in 1:length(titles)) {
  pippo <- scd[,,i]
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
panel_grid <- arrangeGrob(grobs = plots_noleg, nrow = 3, ncol = 5) 

title_grob <- textGrob(
  "Venice: faulty pixels across years",
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