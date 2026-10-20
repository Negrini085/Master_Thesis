# The main goal of this script is to evaluate the model predicted SWE evolution 
# over the overlap area with the ITSNOW reanalysis.
rm(list = ls())
gc()

library(terra)
library(ggplot2)

years <- 2011:2023
fname_template_model <- "../Dataset/SWE_1945.nc"
fname_dem_model <- "../../SWE_analysis/DEM/DEM_compatible.tif"
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



# Importing compatible dem and selecting elevation bands
area <- cellSize(template_model, unit = "m")
dem <- rast(fname_dem_model)
dem[!overlap_mask] <- NA

m_lower <- ifel(dem > -100 & dem <= 1000, 1, NA)
m_medium <- ifel(dem > 1000 & dem <= 2000, 1, NA)
m_higher <- ifel(dem > 2000 & dem <= 3000, 1, NA)
cat("Overlap map created successfully!", "\n")



# Cycle over years
appo_swe <- numeric(0)
appo_swe_lower <- numeric(0)
appo_swe_medium <- numeric(0)
appo_swe_higher <- numeric(0)
appo_dates <- as.Date(character(0))
for(y in years){
  
  # Creating filenames
  first_fn <- paste0("../../SC_studies/IT-Snow/y", y, "/ITSNOW_SWE_", y-1, sprintf("%02d", 9:12), ".nc") 
  second_fn <- paste0("../../SC_studies/IT-Snow/y", y, "/ITSNOW_SWE_", y, sprintf("%02d", 1:8), ".nc") 
  fnames <- c(first_fn, second_fn)
  
  
  # Importing swe maps and computing over the whole investigated area
  r <- rast(fnames)
  r <- project(r, template_model, method = "average")
  r <- mask(r, overlap_mask, maskvalues = FALSE)
  volume <- area * r
  swe_hydro <- global(volume, "sum", na.rm = TRUE)[[1]] * 1e-12


  # Working elevation-wise
  swe_hydro_lower  <- global(mask(volume, m_lower),  "sum", na.rm = TRUE)[[1]] * 1e-12
  swe_hydro_medium <- global(mask(volume, m_medium), "sum", na.rm = TRUE)[[1]] * 1e-12
  swe_hydro_higher <- global(mask(volume, m_higher), "sum", na.rm = TRUE)[[1]] * 1e-12

  appo_swe <- c(appo_swe, swe_hydro)
  appo_swe_lower <- c(appo_swe_lower, swe_hydro_lower)
  appo_swe_medium <- c(appo_swe_medium, swe_hydro_medium)
  appo_swe_higher <- c(appo_swe_higher, swe_hydro_higher)
  appo_dates <- c(appo_dates, seq(as.Date(paste0(y-1, "-09-01")), as.Date(paste0(y, "-08-31")), by = "day"))

  cat("Taken into account year ", y, "\n")
}



# Saving to file
df <- data.frame(
  dates = appo_dates, swe = appo_swe, 
  lower = appo_swe_lower, medium = appo_swe_medium, higher = appo_swe_higher
)
write.table(df, "Results/itsnow_swe_series.dat", col.names = TRUE, row.names = FALSE, quote = FALSE)