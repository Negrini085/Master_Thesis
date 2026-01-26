# The main goal of this script is to enable the user to compute swe monthly maps. As
# a first step we will just plot one map, but then we will create a netCDF file in order
# to store values in a clean way, without need to compute them every time.

library(ncdf4)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")

# In this script I am assuming that missing value actually means absence of snow. I feel
# like that this is a strong assumption, but considering that I'm dealing with
# a reanalysis output, that is something I can do. I will also use a matrix function that
# could help me to archive even better execution times
nc <- nc_open("y2011/ITSNOW_SWE_201103.nc")
time <- ncvar_get(nc, names(nc$var)[1])

# As you can see, here there isn't an explicit for loop
swe <- ncvar_get(nc, names(nc$var)[2])
swe[is.na(swe)] <- 0


appo <- rowMeans(swe, dims = 2)
save(appo, file = "March2011_map_t3.RData")

nc_close(nc)