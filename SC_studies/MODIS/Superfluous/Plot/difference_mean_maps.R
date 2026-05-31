# The main goal of this script is to compute difference between dataset and datas
# mean map. I just want to check whether my procedure can produce comparable results.
rm(list = ls())
gc()

library(sf)
library(terra)
library(rnaturalearth)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/MODIS")
fname_mod <- "Dataset/annual_maps/LOS/los_avg.tif"
fname_me <- "Datas/mean_maps/mean_los.tif"


# Importing mean maps, namely being the one developed by unimi research team and
# the one computed by me
mod_map <- rast(fname_mod)
me_map <- rast(fname_me)


# Making sure that those are comparable. Here I will mask to clip only those pixels
# where both maps aren't NAs
mask_NA <- !is.na(mod_map) & !is.na(me_map)
comp_mod <- mask(mod_map, mask_NA, maskvalues = 0)
comp_me <- mask(me_map, mask_NA, maskvalues = 0)

rm(mod_map, me_map, mask_NA)
gc()


# Computing difference between modis map and my map
diff <- comp_mod - comp_me





# # Plotting procedure
europe_sf <- ne_countries(continent = "Europe", scale = "medium", returnclass = "sf")
europe_v  <- vect(europe_sf)
europe_v  <- project(europe_v, crs(diff))

italy_sf <- europe_sf[europe_sf$admin == "Italy", ]
italy_v  <- vect(italy_sf)
italy_v  <- project(italy_v, crs(diff))


ex <- ext(diff)
bb_vec <- c(xmin = xmin(ex), ymin = ymin(ex), xmax = xmax(ex), ymax = ymax(ex))
bb_sf <- st_as_sfc(st_bbox(bb_vec, crs = st_crs(4326)))

lon_seq <- seq(floor(ex[1]), ceiling(ex[2]), by = 2)
lat_seq <- seq(floor(ex[3]), ceiling(ex[4]), by = 2)

grat_sf <- st_graticule(bb_sf, lon = lon_seq, lat = lat_seq)
grat_v  <- vect(grat_sf)

cols <- hcl.colors(101, "RdBu")
mx <- global(abs(diff), "max", na.rm = TRUE)[1,1]
zlim <- c(-mx, mx)

plot(diff, col = cols, zlim = zlim, main = "LOS difference map: modis - mine", axes = TRUE, mar  = c(3, 3, 3, 6))


lines(grat_v, col = "grey80", lwd = 0.8)
lines(europe_v, col = "grey30", lwd = 0.8)
lines(italy_v,  col = "black",  lwd = 1.2)
