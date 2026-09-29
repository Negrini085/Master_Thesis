# The main goal of this script it to find the maximum value of HS and SWE for a 
# given station, in order to be able to produce standardized plots.
rm(list = ls())
gc()

library(nixmass)
library(ncdf4)

years <- 1950:2023
fname_mask <- "../../Results/SWE_1950.nc"
fname_ana <- "../../../HS_series/Correct/STATION_check/Dataset/ANAGRAFICA"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_outputs/SWE/")


# Function to find maximum HS
compute_max_hs <- function(name){
  
  # Importing HS series
  fname_hs <- paste0("../../../HS_series/Correct/Dataset/", name)
  df_hs <- read.table(fname_hs, header = FALSE)
  year <- as.numeric(df_hs$V1)
  hs <- as.numeric(df_hs$V2)
  
  # Selecting only years which will be plotted (no 2024 or 2025)
  mask <- year < 2024
  hs <- hs[mask]
  year <- year[mask]
  
  # Finding actual hs max
  appo_max <- max(hs, na.rm = TRUE)
  return(appo_max)
}


# Function to compute maximum SWE from DeltaSnow conversion
compute_max_swe_from_delta <- function(name){
  
  # Importing HS in order to convert into swe
  fname_hs <- paste0("../../../HS_series/Correct/Dataset/", name)
  df_hs <- read.table(fname_hs, header = FALSE)
  years <- as.numeric(df_hs$V1)
  year <- unique(years)
  
  appo_swe <- numeric(0)
  for(y in year){
    
    # Selecting HS hydrological data data
    mask <- years == y
    hs_hydro <- as.numeric(df_hs$V2)[mask]
    dates <- seq(as.Date(paste0(y-1, "-09-01")), as.Date(paste0(y, "-08-31")), by = "day")
    
    if(hs_hydro[1] != 0){
      hs_hydro <- c(0, hs_hydro)
      dates <- seq(as.Date(paste0(y-1, "-08-31")), as.Date(paste0(y, "-08-31")), by = "day")
    }
    
    # HS to SWE conversion
    hsdata <- data.frame(date = dates, hs = hs_hydro/100)
    tryCatch({
      appo_swe <- c(appo_swe, swe.delta.snow(hsdata, dyn_rho_max = FALSE))
    }, error = function(e) {
      stop(paste0("Delta snow failed for ", name, " during ", y, ": ", e$message))
    })
  }
  
  return(max(appo_swe, na.rm = TRUE))
}


# Function to compute maximum SWE value from model
compute_max_swe_from_model <- function(name){
  
  # Importing both swe and hs years in order to select only useful years
  fname_hs <- paste0("../../../HS_series/Correct/Dataset/", name)
  df_hs <- read.table(fname_hs, header = FALSE)
  years <- as.numeric(df_hs$V1)
  year <- unique(years)
  
  df <- read.table(paste0("Results/SWE_series/", name), header = TRUE, sep = "\t")
  swe_years <- as.integer(format(as.Date(df$date), "%Y"))
  swe <- as.numeric(df$swe)
  
  # Selecting max value
  mask <- swe_years %in% year
  df <- df[mask, ]
  
  appo <- max(as.numeric(df$swe), na.rm = TRUE)
  return(max(appo, na.rm = TRUE))
}










# Importing station meta-data
df_ana <- read.table(fname_ana, header = TRUE)
lon <- as.numeric(df_ana$lon)
lat <- as.numeric(df_ana$lat)
name <- df_ana$name



# Importing SWE mask
nc <- nc_open(fname_mask)
mask <- ncvar_get(nc, "swe", start = c(1, 1, 1), count = c(-1, -1, 1))
lon_grid <- ncvar_get(nc, "lon")
lat_grid <- ncvar_get(nc, "lat")
mask <- is.na(mask)
nc_close(nc)



# Checking which stations fall on a valid pixel
keep <- rep(FALSE, length(name))
ix <- rep(NA, length(name))
iy <- rep(NA, length(name))

for (i in 1:length(name)) {
  
  if (lon[i] < min(lon_grid) | lon[i] > max(lon_grid) |
      lat[i] < min(lat_grid) | lat[i] > max(lat_grid)) next
  
  # Closest grid-point & keeping only non NA
  ix[i] <- which.min(abs(lon_grid - lon[i]))
  iy[i] <- which.min(abs(lat_grid - lat[i]))
  if (!mask[ix[i], iy[i]]) keep[i] <- TRUE
}

df_ana <- df_ana[keep, ]


# Cycle over stations
max_hs <- numeric(0)
max_swe <- numeric(0)
appo_name <- character(0)
for(name in unique(df_ana$name)){
  
  appo_hs <- compute_max_hs(name)
  max_hs <- c(max_hs, appo_hs)

  appo_swe_delta <- compute_max_swe_from_delta(name)
  appo_swe_from_model <- compute_max_swe_from_model(name)
  max_swe <- c(max_swe, max(c(appo_swe_delta, appo_swe_from_model), na.rm = TRUE))
  
  appo_name <- c(appo_name, name)
  print(paste0("Tehen care of: ", name, " - max HS: ", appo_hs, " - max SWE: ", max(c(appo_swe_delta, appo_swe_from_model), na.rm = TRUE)))
}



# Check for NAs
if(any(is.na(max_hs))) {
  warning(paste0("Found ", sum(is.na(max_hs)), " NAs in max_hs"))
  print(paste0("Stations which have NAs in max_hs: ", paste(appo_name[is.na(max_hs)], collapse = ", ")))
}

if(any(is.na(max_swe))) {
  warning(paste0("Found ", sum(is.na(max_swe)), " NAs in max_swe"))
  print(paste0("Stations which have NAs in max_swe: ", paste(appo_name[is.na(max_swe)], collapse = ", ")))
}



# Saving datas to file
df_print <- data.frame(
  name = appo_name, 
  max_hs = max_hs, 
  max_swe = max_swe
)

write.table(df_print, "Results/max_hs_swe_values.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)