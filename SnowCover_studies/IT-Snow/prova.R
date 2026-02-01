# The main goal here is to plot every pixel SCD vs altitude, in order to check if
# my routine has biases or not. In particular, I'm looking for outlier, which stand
# out from the usual distribution as an indication taht somenthing went wrong in my code
rm(list = ls())
gc()

library(ncdf4)
library(terra)
library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")
fname <- "Datas/meanMapSCD.nc"
fname_dat <- "Datas/scd_elevation_bands.dat"

# Opening digitalized elevation model
demR <- rast("DEM/DEM_Italy.tif")
dem <- as.matrix(demR, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)

# Opening netCDF file for SCD analysis
nc <- nc_open(fname)
scdM <- ncvar_get(nc, "SCD")
nc_close(nc)

# Opening .dat file for altitude bands avarages
scdE <- read.table("Datas/scd_elevation_bands.dat")
bands <- seq(50, 4000, 50)
scdE <- scdE$V1


scd <- as.array(scdM[dem > -10])
appo <- as.array(dem[dem > -10])

# Creating dataframes for plotting
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
