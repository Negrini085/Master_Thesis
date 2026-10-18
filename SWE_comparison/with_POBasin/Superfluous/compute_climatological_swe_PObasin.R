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
clim_list <- list()
dates <- c(Nov = "11-01", Jan = "01-01", Mar = "03-01", May = "05-01")
for (m in names(dates)) {
  
  # File names for this date, one per year
  fnames <- character(length(years))
  for (i in seq_along(years)) {
    y <- years[i]
    y_file <- if (m == "Nov") y - 1 else y
    fnames[i] <- paste0(dir_po, y, "/SWE_", y_file, "-", dates[[m]], ".tif")
  }
  
  # Check for missing files
  missing <- fnames[!file.exists(fnames)]
  if (length(missing) > 0) {
    warning("Missing files for ", m, ":\n", paste(missing, collapse = "\n"))
    fnames <- fnames[file.exists(fnames)]
  }
  
  swe_stack <- rast(fnames)
  clim_list[[m]] <- mean(swe_stack, na.rm = TRUE)
  
  cat(m, ": averaged", length(fnames), "maps\n")
}

clim <- do.call(c, unname(clim_list))
names(clim) <- names(dates)

clim_model <- project(clim, model_template, method = "near")
clim_model <- mask(clim_model, overlap_mask, maskvalues = 0)
writeRaster(clim_model, "Results/swe_climatologies_POBasin.tif", overwrite = TRUE)