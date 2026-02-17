# The main goal of this script is to check whether p-value has some dependence 
# from elevation or not. If that's the case, I would like to maybe check why it's
# that so
rm(list = ls())
gc()

library(ncdf4)
library(terra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD/SCD-from-RHO")
fname_dem <- "Datas/DEM_Italy.tif"
fname_pval <- "Datas/pval_map.nc"



# Importing DEM model and p-value map
dem <- rast(fname_dem)
dem <- as.matrix(dem, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)

nc <- nc_open(fname_pval)
pval <- ncvar_get(nc, "Pval")
nc_close(nc)



# Masking values to have both dem and trend significance datas for the same pixel
mask <- !is.na(dem) & !is.na(pval)
plot_ele <- as.vector(dem[mask])
plot_pval <- as.vector(pval[mask])
rm(pval, dem, mask)
gc()



# Plotting procedure
df <- data.frame(
  elevation = plot_ele,
  pval = plot_pval
)


ggplot() +
  geom_point(data = df, aes(x = elevation, y = pval), color = "blue", size = 1, alpha = 0.4) +
  labs(
    title = "P-value vs Elevation",
    x = "Elevation (m)",
    y = "P-value"
  ) +
  theme_minimal()
