# The main goal of this script is to make elevation-wise analysis on p-value, but
# focusing on the last 10% of the confidence interval (from 90% confidence to 100%)
rm(list = ls())
gc()

library(terra)
library(ncdf4)


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
mask <- !is.na(dem) & !is.na(pval) & pval <= 0.1
plot_ele <- as.vector(dem[mask])
plot_pval <- as.vector(pval[mask])
rm(pval, dem, mask)
gc()



# Plotting procedure
df <- data.frame(
  elevation = plot_ele,
  pval = plot_pval
)

ggplot(df, aes(x = elevation, y = pval)) +
  stat_bin2d(binwidth = c(500, 0.01)) +
  labs(
    x = "Elevation (m)",
    y = "P-value",
    fill = "Number of points",
    title = "P-value vs Elevation: focus on last 10%"
  ) +
  theme_minimal()