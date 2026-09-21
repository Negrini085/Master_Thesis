# The main goal of this script is to map total SWE evolution across the investigated area.
rm(list = ls())
gc()

library(terra)

years <- 2003:2021
fname_dem <- "Dataset/DEM_Italy.tif"
fname_template <- "../../Po-Basin/Dataset/1992/SWE_1991-10-03.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/Comparison/PO_vs_ITSNOW/")


# Creating matrix area
r <- rast(fname_template)
area <- cellSize(r, unit = "m")


# Importing, projecting and cropping dem
dem <- rast(fname_dem)
dem <- project(dem, r, method = "bilinear")
mask <- is.na(r)
dem[mask] <- NA
rm(r); invisible(gc())


m_lower  <- ifel(dem <  1000, 1, NA)
m_medium <- ifel(dem >= 1000 & dem < 2000, 1, NA)
m_higher <- ifel(dem >= 2000, 1, NA)




appo_swe <- numeric(0)
appo_swe_lower <- numeric(0)
appo_swe_medium <- numeric(0)
appo_swe_higher <- numeric(0)
appo_dates <- as.Date(character(0))
for(y in years){
  
  # Selecting filenames
  dates_first <-seq(as.Date(paste0(y-1, "-10-03")), as.Date(paste0(y-1, "-12-31")))
  fnames_first <- paste0("../../Po-Basin/Dataset/", y, "/SWE_", dates_first, ".tif")
  
  dates_second <-seq(as.Date(paste0(y, "-01-01")), as.Date(paste0(y, "-07-01")))
  fnames_second <- paste0("../../Po-Basin/Dataset/", y, "/SWE_", dates_second, ".tif")
  fnames <- c(fnames_first, fnames_second)
  
  
  # Importing swe maps and computing over the whole investigated area
  r <- rast(fnames)
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
}

df <- data.frame(
  dates = appo_dates, 
  swe = appo_swe, 
  lower = appo_swe_lower,
  medium = appo_swe_medium,
  higher = appo_swe_higher
)

write.table(df, "appo_SWE.dat", col.names = TRUE, row.names = FALSE, quote = FALSE)