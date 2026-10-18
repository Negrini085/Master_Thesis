# The main goal of this script is to compute SWE climatological maps for Po river 
# basin, in order to make a comparison with model outputs.
rm(list = ls())
gc()

library(terra)

years <- 1992:2021
dir_po <- "../../SC_studies/Po-Basin/Dataset/"
fname_model_template <- "../Dataset/SWE_1945.nc"
fname_glacier_mask <- "../../SWE_analysis/Results/years_continuous_snow.nc"
fname_po_template <- "../../SC_studies/Po-Basin/Dataset/1992/SWE_1991-10-03.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_comparison/with_POBasin/")



# Creating overlap mask
glacier_mask <- rast(fname_glacier_mask)
glacier_mask <- glacier_mask >  36

po_template <- rast(fname_po_template)
model_template <- rast(fname_model_template)[[1]]
po_template <- project(po_template, model_template, method = "near")
overlap_mask <- !is.na(po_template) & !is.na(model_template)

overlap_mask[glacier_mask] <- FALSE




# Selecting required dates
appo_list <- list()
clim_list <- list()
dates <- c(Nov = "11-01", Jan = "01-01", Mar = "03-01", May = "05-01")
for(m in names(dates)) {
  
  # Selecting year to take into account
  for (i in seq_along(years)) {
    y <- years[i]
    y_file <- if (m == "Nov") y - 1 else y
    fname_map <- paste0("../Dataset/SWE_", y_file, ".nc")
    
    idx <- which(seq(as.Date(paste0(y_file, "-01-01")), as.Date(paste0(y_file, "-12-31")), by = "day") == paste0(y_file, "-", dates[m]))
    appo_list[[i]] <- rast(fname_map, subds = "swe")[[idx]]
  }
  
  # Check for missing files
  clim_list[[m]] <- mean(rast(appo_list), na.rm = TRUE)
  appo_list <- list()
  
  cat(m, ": averaged", length(years), "maps\n")
}

clim <- do.call(c, unname(clim_list))
names(clim) <- names(dates)
clim <- mask(clim, overlap_mask, maskvalues = 0)
writeRaster(clim, "Results/swe_climatologies_model.tif", overwrite = TRUE)