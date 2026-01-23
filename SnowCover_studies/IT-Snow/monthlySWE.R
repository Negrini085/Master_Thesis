# The main goal of this script is to enable the user to compute swe monthly maps. 
# In particoular we would like to have as output a netCDF file for every hydrological 
# year containing monthly SWE maps 
rm(list = ls())
gc()

library(ncdf4)
library(matrixStats)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")

years <- 2011:2025
months <- c("09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08")

for(y in years){
  for(i in 1:length(months)){
    if(i<=4){
      fname = paste0("y", toString(y), "/ITSNOW_SWE_", toString(y-1), months[i], ".nc")
    }
    else{
      fname = paste0("y", toString(y), "/ITSNOW_SWE_", toString(y), months[i], ".nc")
    }
    
    nc <- nc_open(fname)
    time <- ncvar_get(nc, names(nc$var)[1])
    
    appo <- matrix(0, nrow = nc$dim$Longitude$len, ncol = nc$dim$Latitude$len )
    
    for(i in time){
      # Here I am assuming that missing value actually means absence of snow. I feel
      # like that this is a strong assumption, but considering that I'm dealing with 
      # a reanalysis output, that is something I can do.
      swe_day <- ncvar_get(nc, names(nc$var)[2], start = c(1, 1, i+1), count = c(-1, -1, 1))
      swe_day[is.na(swe_day)] <- 0
     
      print(paste0("Working with ", i+1, "/03/2011 SWE map!"))
      appo <- appo + swe_day
    }
     
    appo <- appo/length(time)
  }
  
  
}


# nc <- nc_open("y2011/ITSNOW_SWE_201103.nc")
# time <- ncvar_get(nc, names(nc$var)[1])

# 
# save(appo, file = "March2011_map.RData")
# 
# nc_close(nc)