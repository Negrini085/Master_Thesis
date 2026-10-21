# The main goal of this script is to create a climatological SCD map which yields 
# snow cover duration values on the overlap region between the PO basin and the 
# model outputs.
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
  
  # Creating filenames
  y <- years[i]
  dates <- seq(as.Date(paste0(y-1, "-10-03")), as.Date(paste0(y, "-07-01")), by = "day")
  fnames <- paste0("../../SC_studies/Po-Basin/Dataset/", y, "/SWE_", dates,".tif")
  
  # Importing rasters
  r <- rast(fnames)
  r <- project(r, template_model, method = "average")
  
  appo_list[[i]] <- sum(r > 15)
  cat("Taken care of year ", y, " of the overlap!", "\n")
}



# Computing mean map (and projecting as I should)
scd_stack <- rast(appo_list)
scd_clim <- mean(scd_stack)
scd_clim[!overlap_mask] <- NA



# Saving climatological map
names(scd_clim) <- "SCD_mean_1992_2021"
writeRaster(scd_clim, "Results/scd_climatology_po.tif", overwrite = TRUE)