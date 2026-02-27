# The main goal of this script is to make a slope analysis of time series pixel-wise
rm(list = ls())
gc()

library(trend)
library(terra)

years <- 1992:2021
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin/SCD/")

# Function to get p-value
getting_slope <- function(x, len_years=14) {
  
  # Omitting NAs, we don't want them to cause a faulty calculation
  x <- na.omit(x)
  
  # Evaluating if the test is doable or not, then computing slope
  if (length(x) < len_years) return(NA)
  res <- trend::sens.slope(x)
  
  return(as.numeric(res$estimates))
}





# Importing files
files <- paste0("Datas/Seasonal_SCD/SCD_", years, ".tif")
r <- rast(files)


# Checking if there are enough datas for a given set of coordinates. Those that
# don't respect these conditions will be set to NA, to not produce fallacious trends
valid <- app(r, function(x) sum(!is.na(x)))
mask_faulty <- valid < 14 & valid != 0

n_faulty <- global(mask_faulty, "sum", na.rm = TRUE)$sum
print(paste0("There are ", n_faulty, " with at least one value and at most 13."))

r <- mask(r, mask_faulty, maskvalues = 1)


# Using Sen-Theil test in order to evaluate trend slope
slope_map <- app(r, fun = getting_slope, cores = 8)
names(slope_map) <- "slope"
writeRaster(slope_map, "Datas/scd_slope.tif", overwrite=TRUE)