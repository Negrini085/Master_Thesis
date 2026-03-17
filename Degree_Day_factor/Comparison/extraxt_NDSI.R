# The main goal of this script is to extract NDSI values for a given station, in order
# to later check whether there is a continuous relationship between those two metrics
# or not. I have to be careful about the number of days in a given year, because those
# are not whole year data-sets! I also have to be careful that MODIS hydrological years
# are not what I intended as one, in fact:
#         MODIS: 1st October --> 30th September
#         FILON: 1st September --> 31th August
rm(list = ls())
gc()

library(terra)

fname_hs <- "../STATION_series/Datas/station_series/HSD_IT_VDA_AO_GRESSONEY_SAINT_JEAN-BIELTSCHOCKE_3040"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Comparison/")
fname_anagrafica <- "../Dataset/ANAGRAFICA"


# Importing coordinates in order to extract NDSI and data quality values
name_sta <- "HSD_IT_VDA_AO_GRESSONEY_SAINT_JEAN-BIELTSCHOCKE_3040"
df <- read.table(fname_anagrafica, header = FALSE)
lon_st <- df$V2[df$V1 == name_sta]
lat_st <- df$V3[df$V1 == name_sta]


# Cycle to extract NDSI and quality check series
days <- numeric(0)
yr_s <- numeric(0)
years <- 2020:2023
ndsi <- numeric(0)
quality <- numeric(0)
start <- c(275, 1, 1, 1)
end <- c(366, 365, 365, 243)

for(i in seq_len(length(years))){
  for(day in seq(start[i], end[i])){
    
    # Creating correct filepath
    fname_NDSI <- paste0("../../SnowCover_studies/MODIS/NDSI_maps/", years[i], "/MOD_", years[i], sprintf("%03d", day), ".tif")
    if(years[i] == 2022 & day >= 274){
      fname_NDSI <- paste0("../../SnowCover_studies/MODIS/NDSI_maps/", years[i], "/MOD_", years[i], sprintf("%03d", day), "_NDSI.tif")
    }
    else if(years[i] == 2023){
      fname_NDSI <- paste0("../../SnowCover_studies/MODIS/NDSI_maps/", years[i], "/MOD_", years[i], sprintf("%03d", day), "_NDSI.tif")
    }
    fname_QA <- paste0("../../SnowCover_studies/MODIS/NDSI_maps/", years[i], "/MOD_", years[i], sprintf("%03d", day), "_QA.tif")
    
    # Safety check on file existance
    if (!file.exists(fname_NDSI)) {
      print(paste0("Not found ", day, " of ", years[i]))
      
      ndsi <- c(ndsi, NA)
      days <- c(days, day)
      yr_s <- c(yr_s, years[i])
      quality <- c(quality, NA)
      
      next
    }

    # Finding correct pixel and extracting NDSI value
    ndsi_map <- rast(fname_NDSI)
    ind <- cellFromXY(ndsi_map, cbind(lon_st, lat_st))
    ndsi <- c(ndsi, as.numeric(ndsi_map[ind]))
    
    # Finding correct pixel and extracting quality value
    quality_map <- rast(fname_QA)
    ind <- cellFromXY(quality_map, cbind(lon_st, lat_st))
    quality <- c(quality, as.numeric(quality_map[ind]))
    
    # Saving day and year
    days <- c(days, day)
    yr_s <- c(yr_s, years[i])
    
    # Erasing memory
    print(paste0("Taken care of day ", day, " of ", years[i]))
    rm(ndsi_map, quality_map)
    gc()
  }
}


# Saving dataframe
df <- data.frame(
  days = days, 
  years = yr_s, 
  ndsi = ndsi, 
  quality = quality
)

write.table(df, "Datas/NDSI_raw.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)