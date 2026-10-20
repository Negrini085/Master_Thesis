# The main goal of this script is to compute SWE "climatologies" in order to 
# check the changes related to climate change.
rm(list = ls())
gc()

library(terra)

river_name <- "adige"
lim_inf <- seq(1952, 1992, by = 10)
lim_sup <- seq(1981, 2021, by = 10)
fname_basin <- paste0("DEM/", river_name,"_basin.tif")
fname_glacier_mask <- paste0("Results/years_continuous_snow.nc")
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")



# Importing basin mask and glacier mask
basin_mask <- rast(fname_basin)
glacier_mask <- rast(fname_glacier_mask)
area <- cellSize(glacier_mask, unit = "m")
basin_mask <- project(basin_mask, glacier_mask, method = "near")

glacier_mask <- glacier_mask > 36
basin_mask[glacier_mask] <- NA
area[is.na(basin_mask)] <- NA



# Cycle over time windows
appo_swe <- array(0, dim = c(5, 12))
for(i in seq_along(lim_inf)){
  
  # Selecting time window
  min_year <- lim_inf[i]
  max_year <- lim_sup[i]
  
  # Cycle over investigated time-window
  for(y in min_year:max_year){
    
    # Importing first maps
    dates <- as.Date(paste0(y-1, "-", sprintf("%02d", 10:12), "-01"))
    fnames <- paste0("Dataset/SWE/SWE_", y-1 ,".nc")
    idx <- as.integer(format(dates, "%j"))
    
    swe_maps <- rast(fnames)[[idx]]
    swe_maps <- swe_maps * area  * 10^-12
    
    for(j in seq_along(idx)) appo_swe[i, j+9] <- appo_swe[i, j+9] + global(swe_maps[[j]], "sum", na.rm = TRUE)[1, 1]
    
    
    
    # Importing second maps
    dates <- as.Date(paste0(y, "-", sprintf("%02d", 1:9), "-01"))
    fnames <- paste0("Dataset/SWE/SWE_", y ,".nc")
    idx <- as.integer(format(dates, "%j"))
    
    swe_maps <- rast(fnames)[[idx]]
    swe_maps <- swe_maps * area * 10^-12
    
    for(j in seq_along(idx)) appo_swe[i, j] <- appo_swe[i, j] + global(swe_maps[[j]], "sum", na.rm = TRUE)[1, 1]
  }
  
  cat("Done climatology number ", i, "\n")
}


# Computing actual climatology (dividing per year) and saving to file
appo_swe <- appo_swe/30
df <- as.data.frame(t(appo_swe))
names(df) <- paste0(lim_inf, "_", lim_sup)
rownames(df) <- month.abb
write.table(df, paste0("Results/SWE/clima_", river_name, "_basin.dat"), quote = FALSE)
