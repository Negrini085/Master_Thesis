# The main goal of this script is to inspect resized dem characteristics!
rm(list = ls())
gc()


library(terra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/DEM")
fname <- "DEM_Italy.tif"
fname1 <- "Mask.tif"

dem <- rast(fname)
swe <- rast(fname1)

print(dem)
print(swe)

plot(dem, main = "DEM Italy")
summary(values(dem))

res(dem)
ext(dem)