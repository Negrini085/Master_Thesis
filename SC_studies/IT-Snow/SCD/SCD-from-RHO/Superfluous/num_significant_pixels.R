# The main goal of this script is to check significant trend for IT-Snow dataset
# In particular I will focus on extremely significant low p-value pixel, to check
# whether those can be justified by the data or not
rm(list = ls())
gc()

library(sf)
library(ncdf4)
library(scales)
library(ggplot2)
library(rnaturalearth)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD/SCD-from-RHO")
fname <- "Datas/pval_map.nc"



# Importing p-value map to check trend significance
nc <- nc_open(fname)
lon <- ncvar_get(nc, names(nc$dim)[1])
lat <- ncvar_get(nc, names(nc$dim)[2])
pval <- ncvar_get(nc, names(nc$var)[1])
nc_close(nc)



# At first we would like to check some ratios (number of pixels below a certain 
# threshold over number of pixel with datas). The pval map is not na only on much 
# of italian territory, so we don't have to filter with dem model.
mask <- pval < 0.1 & !is.na(pval)
num_sign <- length(pval[mask])
print(paste0("Number of pixel with significant trend: ", num_sign))

mask <- !is.na(pval)
num_tot <- length(pval[mask])
print(paste0("Total number of pixels: ", num_tot))



# The output of the previous lines of code is kind of concerning, because almost 
# thirty percent of pixels show a trend with p-value lower than 1/10. I will now
# check how the number of significant pixels changes with p-value threshold, hopefully
# making a plot with semi-logaritmic scale.
p_lims <- c(1e-5, 5e-5, 1e-4, 5e-4, 1e-3, 5e-3, 1e-2, 5e-2, 1e-1, 5e-1, 1)

num_lims <- numeric(0)
for(p in p_lims){
  
  # Looking for number of pixels with a p-value as high as p, which is one of
  # the items of p_lims
  mask <- pval <= p & !is.na(pval)
  appo <- length(pval[mask])
  
  num_lims <- c(num_lims, appo)
}



# Plotting procedure
df <- data.frame(
  p_lim = p_lims,
  n_pix = num_lims
)

ggplot(df, aes(x = p_lim, y = n_pix)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_log10(breaks = p_lims, labels = function(x) format(x, scientific = TRUE, trim = TRUE)) +
  labs(x = "P-value threshold", y = "Number of pixels", title = "Number of pixels with p-value lower than a certain threshold") +
  theme_bw()
