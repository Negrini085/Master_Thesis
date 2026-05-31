# The main goal of this script is to study the dependence between scd and elevation, 
# looking for faulty pixels as the ones we already encountered in our past study. 
# Here we will take into account pixels whose SCD is at least unitary, in order to 
# mask on SCD values and to rule out the possibility that those bugs were caused by 
# an error in the DEM clipping procedure.
rm(list = ls())
gc()

library(ncdf4)
library(terra)
library(ggplot2)

# Setting repository and specifying file name
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD-from-SWE")
fname <- "Datas/mean_SCD_map.nc"

# Importing DEM. Here we need to make sure that we are importing the one of the 
# whole region, and not the one clipped only on Italy.
demR <- rast("../DEM/DEM_region.tif")
dem <- as.matrix(demR, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)

# Opening netCDF file containing mean SCD map
nc <- nc_open(fname)
scd <- ncvar_get(nc, "SCD")
nc_close(nc)

# Selecting only those pixels which value is assigned. All those pixels with value
# equal to zero will be not taken into account as if right now, because we don't 
# want to use any tool to select the italian shape.
scd_vals <- scd[scd > 0]
scd_elev <- dem[scd > 0]


# Plotting procedure
df <- data.frame(
  ele = scd_elev,
  scd = scd_vals
)

ggplot() +
  geom_point(data = df, aes(x = ele, y = scd),
             color = "blue", size = 1, alpha = 0.4) +
  labs(
    title = "Mean SCD vs Elevation: only pixels with at least 1 day",
    x = "Elevation (m)",
    y = "Mean SCD (days)"
  ) +
  theme_minimal()

# Faulty pixels definetly show up even when we are using the unclipped model 
# for elevation. It means that those are most likely a bug of IT-Snow itself.
