library(ncdf4)

f_name = "y2011/ITSNOW_SWE_201103.nc"

# Opening netCDF file of SWE
nc <- nc_open(f_name)
print("\n\nDataset loaded successfully!\n\n")
print(nc)

# names() is a function that state variable names
print("Variable names are: \n")
print(names(nc$ndims))
print("\n\nCoordinate names are: \n")
print(names(nc$dim))
print("\n")

# Printing latitude and longitude to inspect if values stored are int64 as
# when I was using xarray. At least here are OK!
print(nc$dim$Latitude$vals)
print(nc$dim$Longitude$vals)

# It's useful to note that you can get datas simply by using ncvar_get()
# First thing first I have to get the variable name. I think that there are 2 ways to do so.
val_name <- names(nc$var)[2]     # Also "attributes(swe$var)$names[1]" works!
swe = ncvar_get(nc, val_name, start = c(1, 1, 1), count = c(-1, -1, 1))
print(swe)
