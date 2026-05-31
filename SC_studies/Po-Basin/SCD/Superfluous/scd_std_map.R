# The main goal of this script is to compute standard deviation for snow metrics
# across the investigated period. I would like to consider only pixels with at least
# 15 valid years over the 25-year investigated span
rm(list = ls())
gc()

library(terra)

years <- 1992:2021
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")


# Importing raster files
files <- paste0("Datas/Seasonal_SCD/SCD_", years, ".tif")
r <- rast(files)


# Checking if some pixels are not assigned for more than 10 years on a 30 year span
valid <- app(r, function(x) sum(!is.na(x)))
mask_faulty <- valid < 20 & valid != 0

n_faulty <- global(mask_faulty, "sum", na.rm = TRUE)$sum
print(paste0("There are ", n_faulty, " with at least one value and at most 19."))

rm(mask_faulty)
gc()


# Taking mean values only considering those pixel which have at least 15 datas on
# a 25 year span. We just have to mask those pixels not reaching our threshold.
mask_NAs <- valid < 20
sd_r <- stdev(r, na.rm = TRUE)

correct_r <- mask(sd_r, mask_NAs, maskvalues = 1)
writeRaster(correct_r, "Datas/sd_map.tif", overwrite=TRUE)