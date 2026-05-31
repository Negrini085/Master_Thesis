# The main goal here is to plot every pixel SCD vs altitude, in order to check if
# my routine has biases or not.
rm(list = ls())
gc()

library(ncdf4)
library(terra)
library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD/SCD-from-SWE")
fname <- "Datas/scd_mean_map.nc"
fname_dat <- "Datas/scd_mean_per_band.dat"



# Opening DEM and other files necessary to create a plot SCD vs elevation for my 
# region of interest. We will need to mask some values, in order not to take into
# account outliers.
dem <- rast("Datas/DEM_Italy.tif")
dem <- as.matrix(dem, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)

nc <- nc_open(fname)
scdM <- ncvar_get(nc, "SCD")
nc_close(nc)

scdE <- read.table(fname_dat)
bands <- c(seq(12.5, 2987.5, 25), seq(3025, 3475, 50), seq(3550, 3950, 100))
scdE <- scdE$V1



# Masking values in order to take into account only the ones who got into mean SCD
# calculation for elevation band
mask_faulty <- scdM > 300 & dem < 500
scdM[mask_faulty] <- NA

mask_faulty <- scdM > 30 & scdM < 200 & dem < 50
scdM[mask_faulty] <- NA

mask_faulty <- scdM == 0 & dem > 1000 & !is.na(dem)
scdM[mask_faulty] <- NA


mask <- !is.na(dem) & !is.na(scdM) 
scd <- as.array(scdM[mask])
appo <- as.array(dem[mask])



# From now on is only plotting procedure
df <- data.frame(
  elevation = appo,
  scd = scd
)

dfE <- data.frame(
  el = bands,
  sc = scdE
)

ggplot() +
  geom_point(data = df, aes(x = elevation, y = scd),
             color = "blue", size = 1, alpha = 0.4) +
  geom_point(data = dfE, aes(x = el, y = sc),
             color = "orange", size = 2) +
  labs(
    title = "Mean SCD vs Elevation",
    x = "Elevation (m)",
    y = "Mean SCD (days)"
  ) +
  theme_minimal()
