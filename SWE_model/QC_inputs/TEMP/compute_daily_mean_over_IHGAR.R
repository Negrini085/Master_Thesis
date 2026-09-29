# The main goal of this script is to compute the daily mean temperature over the IH-GAR territory.
rm(list = ls())
gc()

library(ncdf4)

years <- 1951:2023
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_inputs/TEMP/")


# Cycle over years
temp_val <- numeric(0)
for(y in years){
  
  # Importing netCDF file
  fname <- paste0("../../Dataset/TEMP/", y, ".nc")
  nc <- nc_open(fname)
  temp <- ncvar_get(nc, "tmd")
  appo <- apply(temp, 3, mean, na.rm = TRUE)
  nc_close(nc)
  
  temp_val <- c(temp_val, appo)
  cat("Taken care of year ", y, "\n")
}

dates <- seq(as.Date("1951-01-01"), as.Date("2023-12-31"), "day")
if(length(dates) != length(temp_val)) stop("Dates and temperature have no compatible length!")

df <- data.frame(
  date = dates,
  temp = temp_val
)
write.table(df, "Results/mean_temp_series.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)