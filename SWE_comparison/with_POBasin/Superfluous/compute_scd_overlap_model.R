# The main goal of this script is to compute SCD climatology over the overlap 
# period, in order to assess if model and Po basin predictions agree on snow cover 
# persistence on the ground.
rm(list = ls())
gc()

library(terra)

years <- 1992:2021
fname_template_model <- "../Dataset/SWE_1945.nc"
fname_template_po <- "../../SC_studies/Po-Basin/Dataset/1992/SWE_1991-10-03.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_comparison/with_POBasin/")



# Creating overlap map
template_po <- rast(fname_template_po)
template_model <- rast(fname_template_model)[[1]]
template_po <- project(template_po, template_model, method = "near")
compareGeom(template_po, template_model, stopOnError = TRUE)
overlap_mask <- !is.na(template_model) & !is.na(template_po)


# Cycle over years
appo_list <- list()
for(i in seq_along(years)){
  
  # Importing relevant SC maps
  y <- years[i]
  dates <- seq(as.Date(paste0(y-1, "-01-01")), as.Date(paste0(y-1, "-12-31")), by = "day")
  idx_min <- which(dates == as.Date(paste0(y-1, "-10-03")))
  idx_max <- which(dates == as.Date(paste0(y-1, "-12-31")))
  scd_maps <- rast(paste0("../../SWE_analysis/Dataset/SC/lim_15/SC_", y-1, ".nc"))[[idx_min:idx_max]]
  
  dates <- seq(as.Date(paste0(y, "-01-01")), as.Date(paste0(y, "-12-31")), by = "day")
  idx_max <- which(dates == as.Date(paste0(y, "-07-01")))
  scd_maps <- c(scd_maps, rast(paste0("../../SWE_analysis/Dataset/SC/lim_15/SC_", y, ".nc"))[[1:idx_max]])
  
  # Computing SCD map
  appo_list[[i]] <- sum(scd_maps)
  cat("Taken care of year", y, "\n")
}



# Computing mean map
scd_stack <- rast(appo_list)
scd_clim <- mean(scd_stack)
compareGeom(scd_clim, overlap_mask, stopOnError = TRUE)
scd_clim[!overlap_mask] <- NA



# Saving climatological map
names(scd_clim) <- "SCD_mean_1992_2021"
writeRaster(scd_clim, "Results/scd_climatology_model.tif", overwrite = TRUE)