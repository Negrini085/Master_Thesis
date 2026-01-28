# The main goal of this script is to be able to compute pixel area correctly, in 
# order to actually give a correct estimate of SWE total volume and evolution

library(ncdf4)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")

nc <- nc_open("y2011/ITSNOW_SWE_201103.nc")
lat <- ncvar_get(nc, "Latitude")
lon <- ncvar_get(nc, "Longitude")
nc_close(nc)

# Here we are going to assume that earth is a perfect sphere, in order to apply
# basic geometric reasoning.
rEarth <-  6371005.0
colat <- (90 - lat)*2*pi/360          # Evaluating colatitude as radiant variable
sp <- (lat[45] - lat[44])*2*pi/360    # We have to do degree -> radiant conversion
area <- array(rEarth^2 * sp^2, dim = c(length(lon), length(lat)))

# Final pixel area computation
for(i in 1:length(lat)){
  area[i, ] <- sin(colat[i]) * area[i, 1]
}

# We should now test how to multiply matrix together element-wise
a <- matrix(1:4, nrow = 2)
b <- matrix(5:8, nrow = 2)

print("Prima matrice: ")
print(a)
print("")
print("Seconda matrice: ")
print(b)
print("")

print("Matrice ottenuta moltiplicando element-wise: ")
print(a*b)