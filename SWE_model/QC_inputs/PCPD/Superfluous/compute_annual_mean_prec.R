# The main goal of this script is to compute mean annual precipitation values over 
# the IH-GAR study area.
rm(list = ls())
gc()

library(ncdf4)

years <- 1951:2023
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_inputs/PCPD/")



# Cycle over years to compute seasonal climatologies
mean_precs <- numeric(0)
for(y in years){
  
  # Importing netCDF file
  fname <- paste0("../../Dataset/PCPD/", y, ".nc")
  nc <- nc_open(fname)
  lon <- ncvar_get(nc,"lon")
  lat <- ncvar_get(nc,"lat")
  prec <- ncvar_get(nc,"total_precipitation")
  nc_close(nc)
  
  
  # Updating annual climatology
  annual_prec <- rowSums(prec, dims = 2, na.rm = FALSE)
  mean_annual_prec <- mean(annual_prec, na.rm = TRUE)

  print(paste0("Correctly computed mean  value ", y))
  mean_precs <- c(mean_precs, mean_annual_prec)
}



# Saving to file
df_print <- data.frame(
  year = years,
  mean_prec = mean_precs
)

write.table(df_print, "Results/mean_annual_prec.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)