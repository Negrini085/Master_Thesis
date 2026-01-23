# The main goal of this script is to enable the user to compute swe monthly maps. As
# a first step we will just plot one map, but then we will create a netCDF file in order
# to store values in a clean way, without need to compute them every time.

library(ncdf4)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")

# We have to address a problem that could lead to erraneous results. In fact there is
# no built-in operator for matrices that is able to treat the NAs as we want to. We will
# have to define a function that performs operations element-wise and gives NA as a result
# only when both values are missing. 
sumNA <- function(a, b){
  if(is.na(a) & is.na(b)){ return(NA) }
  sum(a, b, na.rm = TRUE)               # No need to use return (last line always returned)
}


nc <- nc_open("y2011/ITSNOW_SWE_201103.nc")
time <- ncvar_get(nc, names(nc$var)[1])
appo <- matrix(0, nrow = nc$dim$Longitude$len, ncol = nc$dim$Latitude$len )

for(i in time){
  swe_day <- ncvar_get(nc, names(nc$var)[2], start = c(1, 1, i+1), count = c(-1, -1, 1))
  
  print(paste0("Working with ", i+1, "/03/2011 SWE map!"))
  res <- mapply(sumNA, appo, swe_day)
  appo <- matrix(res, nrow = nrow(appo))
}

appo <- appo/length(time)
save(appo, file = "March2011_map.RData")

nc_close(nc)