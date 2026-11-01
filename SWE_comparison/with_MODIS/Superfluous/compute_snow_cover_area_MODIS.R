# The main goal of this script is to compute the climatological snow cover fraction 
# over the hydrological year in order to evaluate whether modis and  model differences 
# are caused by satellite misreading of by a bias in representing snowpack dynamics.
rm(list = ls())
gc()

library(terra)

years <- 2000:2023
fname_template_model <- "../Dataset/SWE_1945.nc"
fname_template_modis <- "../../SC_studies/MODIS/Dataset/annual_maps/LOS/los_2023.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_comparison/with_MODIS/")



# Creating overlap mask
template_model <- rast(fname_template_model)[[1]]
template_modis <- rast(fname_template_modis)

template_modis <- project(template_modis, template_model, method = "near")
compareGeom(template_modis, template_model, stopOnError = TRUE)
overlap_mask <- !is.na(template_model) & !is.na(template_modis)



# Computing snow cover fractions across multiple dates
cell_area <- cellSize(template_model, unit = "km") 
covered_area <- array(0, dim = c(1, 365))
for(y in years){
  start <- 1
  end <- 365
  if(y%%4==0) end <- 366
  
  if(y==2000) {start <- 221; end <- 312}
  if(y==2023) end <- 273
  
  fname <- paste0("../../SC_studies/MODIS/Dataset/daily/", y,"/day_", sprintf("%03d", start:end), ".tif")
  year_maps <- rast(fname)
  if(y %% 4 == 0  & y != 2000) year_maps <- year_maps[[-60]]
  year_maps <- subst(year_maps, 2, NA)
  year_maps <- project(year_maps, template_model, method = "average")
  year_maps <- mask(year_maps, overlap_mask, maskvalues = 0)
  
  if(y %% 4 == 0){end <- 365}
  if(y==2000){start <- 274} 
  mm <- minmax(year_maps, compute = TRUE)
  if (max(mm["max", ]) > 1 || min(mm["min", ]) < 0) {
    stop("Invalid values for year ", y)
  }
  appo_area <- global(cell_area * year_maps, "sum", na.rm = TRUE)[, 1]
  stopifnot(length(appo_area) == end - start + 1)
  
  covered_area[start:end] <- covered_area[start:end] + appo_area
  cat("Processed year: ", y, "\n")
}

covered_area <- covered_area/(length(years)-1)
covered_area_hy <- c(covered_area[274:365], covered_area[1:273])
write.table(data.frame(value = as.vector(covered_area_hy)), "Results/snow_covered_area_modis.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)