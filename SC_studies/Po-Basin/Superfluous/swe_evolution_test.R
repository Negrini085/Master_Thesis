# The main goal of this script is just to be develop a routine which enables us 
# to compute swe volume. We will use a .tif file we created starting from one of
# IT-Snow netCDF files, so that we can compare the previous procedure with the 
# new one, with the goal of having a close match. We won't find a perfect match, 
# because terra procedures are much more advanced than our spherical earth assumption.

library(ncdf4)
library(terra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")

netname <- "Dataset/SWE_map.nc"
tifname <- "Dataset/SWE_map.tif"


#---------------------------------------------------------------------------------------------#
#    First check --> we want to know if we can sum raster/matrix values to the same result    #
#---------------------------------------------------------------------------------------------#
nc <- nc_open(netname)
swe_nc <- ncvar_get(nc, "SWE")
nc_close(nc)

print("NetCDF result: ")
print(sum(swe_nc, na.rm = TRUE))

# Here we will be using global, which is a procedure that compute global statistic.
# To chose which static to compute, you just have to specify it as a "fun=" option.
# The result is given in a dataframe shape, so you just have to treat as such.
swe_tif <- rast(tifname)

print("Tiff result: ")
print(global(swe_tif, fun="sum", na.rm = TRUE)$sum[1])


#---------------------------------------------------------------------------------------------#
#    Second check --> we want to know if we are able to create similar area matrix/raster     #
#---------------------------------------------------------------------------------------------#
nc <- nc_open(netname)
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")
nc_close(nc)

# Here we are going to assume that earth is a perfect sphere, in order to apply
# basic geometric reasoning.
rEarth <-  6371005.0
colat <- (90 - lat)*2*pi/360          # Evaluating colatitude as radiant variable
sp <- (lat[45] - lat[44])*2*pi/360    # We have to do degree -> radiant conversion
area <- array(rEarth^2 * sp^2, dim = c(length(lon), length(lat)))

# Final pixel area computation
for(i in 1:length(lat)){
  area[, i] <- sin(colat[i]) * area[, i]
}

print("NetCDF result: ")
print(sum(area))

# Here we will be using cellSize(), which is a function that create a raster 
# containing pixel areas.
swe_tif <- rast(tifname)

print("Tiff result: ")
print(global(cellSize(swe_tif, unit="m"), "sum", na.rm=TRUE)$sum[1])


#---------------------------------------------------------------------------------------------#
#        Third check ---> we want to know if we are able to compute similar swe values        #
#---------------------------------------------------------------------------------------------#
print("NetCDF result: ")
print(sum(area*swe_nc*10^-12, na.rm = TRUE))

print("Tiff result: ")
print(global(cellSize(swe_tif, unit="m")*swe_tif*10^-12, fun="sum", na.rm=TRUE)$sum[1])