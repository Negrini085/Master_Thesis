# The main goal of this script is to compute winter precipitation totals on IH-GAR, to 
# check whether some localized increase in precipitation can be detected over 1951-2023.
rm(list = ls())
gc()

library(terra)
library(trend)

years <- 1951:2023
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_inputs/TEMP/")

# Function to compute Mann-Kendall e Sen-Theil tests
mk_sen <- function(x) {
  if (any(is.na(x))) return(c(NA, NA))
  
  mk <- trend::mk.test(x)
  ss <- trend::sens.slope(x)
  
  c(p_value = mk$p.value, sen = unname(ss$estimates))
}










totals_list <- list()
for(y in years){
  
  # Importing january temperatures
  appo_temp <- (rast(paste0("../../Dataset/TEMP/", y, ".nc"))["tmnd"] + rast(paste0("../../Dataset/TEMP/", y, ".nc"))["tmxd"])/2
  
  # Computing winter totals
  totals_list[[as.character(y)]] <- mean(appo_temp)
  cat("Done precipitation total computation for year ", y, "\n")
}





# Converting to raster format and computing trends
winter_totals <- rast(totals_list)
winter_clima <- mean(winter_totals[[1:30]])
winter_anom <- winter_totals - winter_clima
trend_res <- app(winter_anom, mk_sen, cores = 8)

# Saving to file
names(trend_res) <- c("p_value", "sen_slope")
trend_res[["sen_slope"]] <- trend_res[["sen_slope"]] * 10
writeRaster(trend_res, "Results/annual_mean_temp_trends.tif", overwrite = TRUE)