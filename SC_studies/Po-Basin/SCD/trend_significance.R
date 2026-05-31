# The main goal of this script is to check trend significance for snow metrics. I
# would like to check if I can reproduce paper results.
rm(list = ls())
gc()

library(trend)
library(terra)

years <- 1992:2021
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin/SCD/")

# Function to get p-value
getting_pval <- function(x, len_years=14) {
  
  # Omitting NAs, we don't want them to cause a faulty calculation
  x <- na.omit(x)
  
  # Evaluating if the test is doable or not, then computing p-value
  if (length(x) < len_years) return(NA)
  res <- trend::mk.test(x)
  
  return(res$p.value)
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


# Using Mann-Kendall test in order to evaluate trend significance
pval_map <- app(r, fun = getting_pval, cores = 8)


# Filtering those pixels who don't meet our standards (at least 10 non zero values)
num_years <- app(r, function(x) sum(x == 0))
pval_map <- ifel(num_years < 15, pval_map, 1)
names(pval_map) <- "p-value"

writeRaster(pval_map, "Datas/pval_scd.tif", overwrite=TRUE)