# The main goal of this script is to compute SWE series with a parallel coding 
# approach. I would like to split SWE matrix in sub-matrices, in order to make 
# total volume computation faster
rm(list = ls())
gc()

library(ncdf4)
library(ggplot2)
library(parallel)

start_time <- Sys.time()
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")


# Time window
years <- 2011:2025
months <- c("09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08")


#----------------------------------------------#
#              Creating area map               #
#----------------------------------------------#
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
  area[, i] <- sin(colat[i]) * area[, i]
}


#----------------------------------------------#
#            Evaluating SWE evo                #
#----------------------------------------------#
swe_evolution <- numeric(0)
for(y in years){
  for(i in 1:length(months)){
    
    # Logic conditions in order to create the correct file path
    if(i > 4){
      fname <- paste("y", toString(y), "/ITSNOW_SWE_", toString(y), months[i], ".nc", sep="")
      print(paste("Evaluating SWE total volume ", months[i], "/", y, sep=""))
    }
    else{
      fname <- paste("y", toString(y), "/ITSNOW_SWE_", toString(y-1), months[i], ".nc", sep="")
      print(paste("Evaluating SWE total volume ", months[i], "/", y-1, sep=""))
    }
    
    # The real deal, opening netCDF4 datas and evaluating total SWE
    nc <- nc_open(fname)
    swe_maps <- ncvar_get(nc, names(nc$var[2]))
    nc_close(nc)
    
    swe_maps <- sweep(swe_maps, c(1, 2), area, `*`)
    swe_month <- colSums(swe_maps, dims = 2, na.rm = TRUE)*10^-12
    
    swe_evolution <- c(swe_evolution, swe_month)
  }
}

df <- data.frame(
  day = 1:length(swe_evolution),
  swe = swe_evolution
)

data0 <- as.Date("2010-09-01")

# Plotting options (we will do a better job when it's all finished)
write.table(df$swe, file = "swe_evolution.dat", row.names = FALSE, col.names = FALSE)
ggplot(df, aes(x = df$day, y = df$swe)) + 
  geom_line(color = "blue", size = 1.5) +
  labs(title = "SWE evolution: 2011 to 2025", x = "Days", y = "SWE Gm^3") +
  theme_minimal()

end_time <- Sys.time()
elapsed <- end_time - start_time
elapsed