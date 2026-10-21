# The main goal of this script is to evaluate the model predicted SWE evolution 
# over the overlap area with the ITSNOW reanalysis.
rm(list = ls())
gc()

library(terra)

years <- 2011:2023
fname_template_model <- "../Dataset/SWE_1945.nc"
fname_dem_itsnow <- "../../SC_studies/IT-Snow/DEM/DEM_Italy.tif"
fname_glacier_mask <- "../../SWE_analysis/Results/years_continuous_snow.nc"
fname_template_itsnow <- "../../SC_studies/IT-Snow/SCD/SCD-from-RHO/Datas/scd_mean_map.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_comparison/with_ITSNOW/")



# Creating overlap map
template_model <- rast(fname_template_model)[[1]]
template_itsnow <- rast(fname_template_itsnow)
dem_itsnow <- rast(fname_dem_itsnow)

dem_itsnow <- project(dem_itsnow, template_itsnow, method = "near")
dem_mask <- !is.na(dem_itsnow) & dem_itsnow > 1000 & template_itsnow == 0
dem_itsnow[dem_mask] <- NA

glacier_mask <- rast(fname_glacier_mask)
glacier_mask <- glacier_mask > 36

dem_itsnow <- project(dem_itsnow, template_model, method = "near")
overlap_mask <- !is.na(dem_itsnow) & !is.na(template_model)
overlap_mask[glacier_mask] <- FALSE
rm(dem_itsnow, dem_mask, template_itsnow, glacier_mask); invisible(gc())



# Selecting required dates
appo_list <- list()
clim_list <- list()
dates <- c(Nov = "11-01", Jan = "01-01", Mar = "03-01", May = "05-01")
for(m in names(dates)) {
  
  # Selecting year to take into account
  for (i in seq_along(years)) {
    y <- years[i]
    y_file <- if (m == "Nov") y - 1 else y
    fname <- paste0("../../SC_studies/IT-Snow/y", y, "/ITSNOW_SWE_", y_file, sub("-01", "", unname(dates[m])),".nc")
    appo_list[[i]] <- rast(fname)[[1]]
  }
  
  # Check for missing files
  clim_list[[m]] <- mean(rast(appo_list), na.rm = TRUE)
  appo_list <- list()
  
  cat(m, ": averaged", length(years), "maps\n")
}

clim <- do.call(c, unname(clim_list))
clim <- project(clim, overlap_mask, method = "average")
names(clim) <- names(dates)
clim <- mask(clim, overlap_mask, maskvalues = 0)
writeRaster(clim, "Results/swe_climatologies_itsnow.tif", overwrite = TRUE)