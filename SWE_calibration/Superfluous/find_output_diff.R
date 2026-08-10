# The main goal of this script is to find output differences, in order to assess 
# model performances (they should be equal)
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_calibration/")

# Importing station names
df <- read.table("Results/LIST_TO_COMPARE", header = FALSE)
files <- df$V1
station_names <- sub("HSD", "V_SDH", files)



# Cycle over stations
appo_year <- numeric(0)
df_total <- data.frame()
for(name in station_names){
  
  # Importing mine and Michele series
  fname_MINE <- paste0("Results/raw/", name)
  df_MINE <- read.table(fname_MINE, header = FALSE)
  
  mask <- as.numeric(df_MINE$V1) > 1951
  df_MINE <- df_MINE[mask, ]
  
  fname_MICH <- paste0("../Original_series/FILTERED_SNW/", name)
  df_MICH <- read.table(fname_MICH, header = FALSE)
  
  mask <- as.numeric(df_MICH$V1) > 1951
  df_MICH <- df_MICH[mask, ]
  
  if(nrow(df_MICH) != nrow(df_MINE)) stop(paste0("No compatible length for SWE series at ", sub("DV_SDH", "HSD", name)))
  
  
  # Ready to compare series
  appo_MINE <- as.numeric(df_MINE$V4)
  appo_MICH <- as.numeric(df_MICH$V5)
  
  df_appo <- data.frame(
    name = rep(sub("V_SDH", "HSD", name), nrow(df_MICH)),
    year = as.numeric(df_MINE$V1),
    month = as.numeric(df_MINE$V2),
    day = as.numeric(df_MINE$V3),
    mine = appo_MINE,
    mich = appo_MICH
  )
  
  mask <- appo_MINE != appo_MICH
  df_appo <- df_appo[mask, ]
  
  df_total <- rbind(df_total, df_appo)
  cat(paste0("Taken care of ", name, "\n"))
}

cat("\n\n\n")
write.table(df_total, "Results/swe_comparison.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)