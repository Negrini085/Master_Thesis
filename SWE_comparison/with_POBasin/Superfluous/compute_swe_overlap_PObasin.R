# The main goal of this script is to create PO basin SWE series over the overlap region with model outputs.
rm(list = ls())
gc()

library(terra)

years <- 1992:2021
fname_model_map <- "../Dataset/SWE_1951.nc"
fname_dem <- "../../SWE_analysis/DEM/DEM_compatible.tif"
fname_po <- "../../SC_studies/Po-Basin/Dataset/1992/SWE_1991-10-03.tif"
fname_glacier_mask <- "../../SWE_analysis/Results/years_continuous_snow.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_comparison/with_POBasin/")



# Importing model, Po Basin map and glacier area map to create common overlap region
glacier_mask <- rast(fname_glacier_mask)[[1]]
model_map <- rast(fname_model_map)[[1]]
glacier_mask <- glacier_mask > 36
model_map[glacier_mask] <- NA
po_map <- rast(fname_po)


# Projecting po map on model grid map
po_map <- project(po_map, model_map, method = "near")
compareGeom(po_map, model_map, stopOnError = FALSE)


# Masking procedure
overlap_mask <- !is.na(model_map) & !is.na(po_map)
area <- cellSize(po_map, unit = "m")


# Importing, projecting and cropping dem
dem <- rast(fname_dem)
compareGeom(dem, po_map, stopOnError = FALSE)
overlap_mask <- overlap_mask & !is.na(dem)
dem[!overlap_mask] <- NA
rm(po_map, model_map, glacier_mask); invisible(gc())


m_lower  <- ifel(dem > -100 & dem <= 1000, 1, NA)
m_medium <- ifel(dem > 1000 & dem <= 2000, 1, NA)
m_higher <- ifel(dem > 2000 & dem <= 5000, 1, NA)




appo_swe <- numeric(0)
appo_swe_lower <- numeric(0)
appo_swe_medium <- numeric(0)
appo_swe_higher <- numeric(0)
appo_dates <- as.Date(character(0))
for(y in years){
  
  # Selecting filenames
  dates_first <-seq(as.Date(paste0(y-1, "-10-03")), as.Date(paste0(y-1, "-12-31")), by = "day")
  fnames_first <- paste0("../../SC_studies/Po-Basin/Dataset/", y, "/SWE_", dates_first, ".tif")
  
  dates_second <-seq(as.Date(paste0(y, "-01-01")), as.Date(paste0(y, "-07-01")), by = "day")
  fnames_second <- paste0("../../SC_studies/Po-Basin/Dataset/", y, "/SWE_", dates_second, ".tif")
  fnames <- c(fnames_first, fnames_second)
  
  
  # Importing swe maps and computing over the whole investigated area
  r <- rast(fnames)
  r <- project(r, overlap_mask, method = "average")
  r[!overlap_mask] <- NA
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
  appo_dates <- c(appo_dates, c(dates_first, dates_second))
  
  cat("Taken into account hydrological year ", y, "\n")
}

df <- data.frame(
  dates = appo_dates, 
  swe = appo_swe, 
  lower = appo_swe_lower,
  medium = appo_swe_medium,
  higher = appo_swe_higher
)
all.equal(df$swe, df$lower + df$medium + df$higher)
write.table(df, "Results/po_basin_swe_series.dat", col.names = TRUE, row.names = FALSE, quote = FALSE)