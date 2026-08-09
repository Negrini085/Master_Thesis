# The main goal of this script is to convert hydrological SWE series to SC series
# in order to be ready to assess model performance
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Calibration/")

files <- list.files(path = "Results/hydro", full.names = TRUE)
station_names <- sub("Results/hydro/DV_SDH", "HSD", files)

appo_hs <- numeric(0)
appo_swe <- numeric(0)
for(name in station_names){
  if(!file.exists(paste0("../HS_series/Correct/Dataset/", name))) next
  
  # Imporing hs and swe series
  df_hs <- read.table(paste0("../HS_series/Correct/Dataset/", name), header = FALSE)
  hs_years <- as.numeric(df_hs$V1)
  hs_val <- as.numeric(df_hs$V2)
  
  df_swe <- read.table(paste0("Results/hydro/", sub("HSD", "DV_SDH", name)), header = FALSE)
  swe_years <- as.numeric(df_swe$V1)
  swe_val <- as.numeric(df_swe$V2)
  
  
  # Selecting only years with hs series
  mask <- swe_years %in% unique(hs_years)
  swe_val <- swe_val[mask]
  
  mask <- hs_years %in% unique(swe_years)
  hs_val <- hs_val[mask]
  
  if(length(hs_val) != length(swe_val)) stop("Length mismatch for SWE and HS series at ", name)
  
  
  # Conversion to binary series
  mask <- hs_val > 0
  hs_val[!mask] <- 0
  hs_val[mask] <- 1
  
  mask <- swe_val > 0
  swe_val[!mask] <- 0
  swe_val[mask] <- 1
  
  appo_hs <- c(appo_hs, hs_val)
  appo_swe <- c(appo_swe, swe_val)
  
  print(paste0("Taken care of ", name))
}


# Saving whole file
df_print <- data.frame(
  hs = appo_hs,
  swe = appo_swe
)
write.table(df_print, "Results/sc/total.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)