# The main goal of this script is to enable the user to compute swe monthly maps. As
# a first step we will just plot one map, but then we will create a netCDF file in order
# to store values in a clean way, without need to compute them every time.

library(ncdf4)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")

# In this script I am assuming that missing value actually means absence of snow. I feel
# like that this is a strong assumption, but considering that I'm dealing with
# a reanalysis output, that is something I can do.
nc <- nc_open("y2011/ITSNOW_SWE_201103.nc")
time <- ncvar_get(nc, names(nc$var)[1])
appo <- matrix(0, nrow = nc$dim$Longitude$len, ncol = nc$dim$Latitude$len )

for(i in time){
  swe_day <- ncvar_get(nc, names(nc$var)[2], start = c(1, 1, i+1), count = c(-1, -1, 1))
  swe_day[is.na(swe_day)] <- 0
  
  print(paste0("Working with ", i+1, "/03/2011 SWE map!"))
  appo <- appo + swe_day
}

appo <- appo/length(time)
save(appo, file = "March2011_map_t2.RData")

nc_close(nc)