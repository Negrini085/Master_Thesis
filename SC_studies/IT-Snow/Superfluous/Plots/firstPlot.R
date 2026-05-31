# The main goal of this script is to enable the user to plot SWE single-day snapshots
# To do so, ncdf4 and ggplot2 packages will be used

library(ncdf4)
library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")

# Opening netCDF file (using nc_open function) and inspecting its properties
# As we can see thanks to "class()" function here we are dealing with a netcdf4 object
f_name = "y2011/ITSNOW_SWE_201103.nc"
nc <- nc_open(f_name)
class(nc)

# We want to take a look at its arguments and properties
# We can inspect a netCDF file content by means of print() or ncatt_get() 
ncatt_get(nc, varid = 0)    # Varid = 0 means that the function will read the whole file

# After that, we want to focus our attention to SWE. To get that variable from the
# netCDF file, we have to use ncvar_get(). Its usage requires the knowledge of the 
# variable name, which we can get simply by means of names()
print("Variable names are: ")
print(names(nc$ndims))
print("Coordinate names are: ")
print(names(nc$dim))

# At last, knowing that IT-SNOW netCDF files contain monthly values, we have to
# chose which day we want to consider (necessary, because we risk to load an excessive 
# amount of data if we don't do so)
lat_name <- names(nc$dim)[1]
lon_name <- names(nc$dim)[2]
val_name <- names(nc$var)[2]
lat = ncvar_get(nc, lat_name)
lon = ncvar_get(nc, lon_name)
swe = ncvar_get(nc, val_name, start = c(1, 1, 10), count = c(-1, -1, 1))

# Here we try to print latitude values, to check if missing values are present
# Moreover, we will also check coordinates and values dimensions.
print(paste("Class: ", class(lat), "Dimensions: ", dim(lat)))
print(lat)
print(paste("Class: ", class(swe), "Dimensions: ", toString(dim(swe))))
print(swe)

# We will now make the switch to ggplot2(), in order to produce our snapshot
# I would like to start by plotting the simple SWE distribution, and only then
# I will try to add map contours. First step is creating a grid and adding swe values
grid <- expand.grid(lon = lon, lat = lat)
grid$swe <- as.vector(swe)

# Second step is creating a plot. The first thing you have to specify is the data
# you want to plot, then it's just a matter of specifying aesthetic issues
ggplot(grid, aes(x = lon, y = lat, fill = swe)) +
  geom_raster() +                       
  scale_fill_viridis_c(option = "C") +   # Continuous color palette
  coord_fixed() +   
  labs(title = "SWE 10 March 2011", x = "Latitude", y = "Longitude", fill = "SWE (mm w.e.)") +     # Titles
  theme_minimal()


# It's very important that you close a netCDF file once you are done with it
# If you don't do so, there is a high chance of data loss.
nc_close(nc)