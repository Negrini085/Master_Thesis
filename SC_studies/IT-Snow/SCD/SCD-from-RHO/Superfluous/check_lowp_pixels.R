# The main goal of this script is to check trends of those pixels who have extremely
# low p-values (lower than 1e-4, those pixels should be 44). I would like to test
# all of them once again in order to check whether the p-value map construction was
# faulty or not. Then I would like to plot some of them in order to actually see if
# there is a significant trend or not.
rm(list = ls())
gc()

library(trend)
library(ncdf4)
library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD/SCD-from-RHO")
fname_scd <- "Datas/season_maps_SCD.nc"
fname_pval <-"Datas/pval_map.nc"



# Importing pval map and scd maps in order to find high confidence pixels 
nc <- nc_open(fname_scd)
scd <- ncvar_get(nc, names(nc$var)[1])
nc_close(nc)

nc <- nc_open(fname_pval)
pval <- ncvar_get(nc, names(nc$var)[1])
nc_close(nc)



# Finding significant pixels and running again the mk test. We should find the same
# p-values and to make sure I will plot both (the old one and the new one).
p_lim <- 1e-4
years <- 2011:2025
mask <- !is.na(pval) & pval < p_lim
inds <- which(mask, arr.ind = TRUE)

for (k in seq_len(nrow(inds))) {
  
  # Finding row and column indexes
  r <- inds[k, "row"]
  c <- inds[k, "col"]
  
  # Finding old p-value from pval map
  p_old <- pval[r, c]
  
  # Running again mk test on pixel annual SCD values (which we can find in SCD
  # array, which is a stack of 15 layers, one for each year of IT-SNOW reanalysis)
  appo <- scd[r, c, ]
  p_new <- mk.test(appo)$p.value
  
  print(paste0("Pixels ", sprintf("%02d", k), ": ", round(p_old, 6), "     ", round(p_new, 6)))
}



# Randomly selecting 8 pixels to be plotted so that we can actually see what's going on
v <- sample(1:44, size = 8)
v <- sort(v)

ind <- 1
appo <- array(0, dim = c(8, dim(scd)[[3]]))
for(i in v){
  
  # Getting pixel coordinates
  r <- inds[i, "row"]
  c <- inds[i, "col"]
  
  # Selecting time series
  appo[ind, ] <- scd[r, c, ]
  ind <- ind + 1
}



# Plotting procedure
df <- data.frame(
  pixel = rep(seq_len(nrow(appo)), each = length(years)),
  year  = rep(years, times = nrow(appo)),
  scd   = as.vector(t(appo))
)

pvals <- sapply(seq_len(nrow(appo)), function(j) mk.test(appo[j, ])$p.value)
df$facet_lab <- factor(
  df$pixel,
  levels = seq_len(nrow(appo)),
  labels = paste0("Pixel ", seq_len(nrow(appo)), ": p-value of ", format(pvals, scientific = TRUE, digits = 2))
)

ggplot(df, aes(x = year, y = scd)) +
  geom_line() +
  geom_point(size = 1.6) +
  facet_wrap(~ facet_lab, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = years) +
  labs(x = "Anno", y = "SCD") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(size = 10, face = "bold")
  )
