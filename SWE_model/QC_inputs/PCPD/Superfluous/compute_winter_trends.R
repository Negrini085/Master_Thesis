# The main goal of this script is to compute winter precipitation totals on IH-GAR, to 
# check whether some localized increase in precipitation can be detected over 1951-2023.
rm(list = ls())
gc()

library(terra)
library(trend)

years <- 1952:2023
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_inputs/PCPD/")

# Function to compute Mann-Kendall e Sen-Theil tests
mk_sen <- function(x) {
  if (any(is.na(x))) return(c(NA, NA))
  
  mk <- trend::mk.test(x)
  ss <- trend::sens.slope(x)
  
  c(p_value = mk$p.value, sen = unname(ss$estimates))
}










totals_list <- list()
for(y in years){
  
  # Importing first chunk
  start <- 335; end <- 365
  if((y-1) %%4 == 0){ start <- 336; end <- 366 }
  first_chunk <- rast(paste0("../../Dataset/PCPD/", y-1, ".nc"))[[start: end]]
  
  # Importing second chunk
  start <- 1; end <- 59
  if(y %% 4 == 0){ start <- 1; end <- 60 }
  second_chunk <- rast(paste0("../../Dataset/PCPD/", y, ".nc"))[[start:end]]
  
  # Computing winter totals
  winter <- c(first_chunk, second_chunk)
  totals_list[[as.character(y)]] <- sum(winter, na.rm = TRUE)
  cat("Done precipitation total computation for year ", y, "\n")
}





# Converting to raster format and computing trends
winter_totals <- rast(totals_list)
winter_clima <- mean(winter_totals[[1:30]], na.rm = TRUE)
winter_anom <- winter_totals/winter_clima
trend_res <- app(winter_anom, mk_sen, cores = 8)

# Saving to file
names(trend_res) <- c("p_value", "sen_slope")
trend_res[["sen_slope"]] <- trend_res[["sen_slope"]] * 1000
writeRaster(trend_res, "Results/winter_precipitation_anomaly_trends.tif", overwrite = TRUE)