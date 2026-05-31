# The main goal of this script is to test SCD evaluation procedure starting from
# RHO, because we just got some strange results.
rm(list = ls())
gc()

library(sf)
library(ncdf4)
library(terra)
library(ggplot2)
library(rnaturalearth)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD-from-RHO")
fname_swe <- "../y2011/ITSNOW_SWE_201103.nc"
fname_rho <- "../y2011/ITSNOW_RhoS_201103.nc"


# Importing SWE, snow density and DEM model
nc <- nc_open(fname_swe)
lat <- ncvar_get(nc, "Latitude")
lon <- ncvar_get(nc, "Longitude")
swe <- ncvar_get(nc, names(nc$var)[2])
nc_close(nc)

nc <- nc_open(fname_rho)
rho <- ncvar_get(nc, names(nc$var)[2])
nc_close(nc)

dem <- rast("Datas/DEM_Italy.tif")
dem <- as.matrix(dem, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)



# Evaluating snow cover fraction
initial_mask <- !is.na(swe) & !is.na(rho) & rho != 0
swe[!initial_mask] <- 0
rho[!initial_mask] <- 1

appo_sc <- swe/(0.1 * rho)
appo_sc[is.na(appo_sc)] <- 0
sc <- pmin(appo_sc, 1)

sc[!initial_mask] <- 0
# sc[sc >= 0.5] <- 1
# sc[sc < 0.5] <- 0
sc[is.na(dem)] <- NA



# Plotting procedure
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")

grid <- expand.grid(lon = lon, lat = lat)
grid$sc <- as.vector(sc[, , 1])

ggplot() +
  geom_sf(data = europe, fill = "grey90", color = "black", inherit.aes = FALSE) +
  coord_sf(xlim = c(6, 19), ylim = c(37, 46.8)) +
  geom_raster(data = grid, aes(x = lon, y = lat, fill = sc)) +
  scale_fill_viridis_c(option = "C", na.value = "transparent") +
  labs(title = "Snow cover 01/03/2011", x = "Longitude", y = "Latitude", fill = "") +
  theme_minimal()
