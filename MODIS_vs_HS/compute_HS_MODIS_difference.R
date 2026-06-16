# The main goal of this script is to plot MODIS-HS bias, in order to check if
# we can make sense of some faulty stations
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/MODIS_vs_HS/")
files <- list.files(path = "../HS_series/Correct/MODIS_series/Dataset/modis_hydrological", full.names = TRUE)
names <- sub("../HS_series/Correct/MODIS_series/Dataset/modis_hydrological/", "", files)

diff <- numeric(0)
appo_name <- character(0)
for(name in names){
   
  # Importing HS and MODIS series
  fname_hs <- paste0("../HS_series/Correct/STATION_series/Dataset/", name)
  df_hs <- read.table(fname_hs, header = FALSE)
  
  fname_mod <- paste0("Dataset/sc_hydro/", name)
  df_mod <- read.table(fname_mod, header = FALSE)
  
  
  # Selecting only overlapping periods
  mask <- as.numeric(df_hs$V1) > 2000
  df_hs <- df_hs[mask, ]
  
  mask <- as.numeric(df_mod$V1) %in% unique(as.numeric(df_hs$V1))
  df_mod <- df_mod[mask, ]
  
  
  # HS to SC conversion
  mask <- as.numeric(df_hs$V2) > 0
  df_hs$V2[mask] <- 1
  
  if(length(unique(as.numeric(df_hs$V1))) < 5) next
  
  if(nrow(df_mod) != nrow(df_hs)) stop(paste0("Mismatch in data-frame lenght for ", name, "! ", nrow(df_mod), " & ", nrow(df_hs)))
  if(sum(as.numeric(df_hs$V2), na.rm = TRUE) == 0) stop("No HS datas for ", name)
  
  appo <- (sum(as.numeric(df_hs$V2), na.rm = TRUE) - sum(as.numeric(df_mod$V2), na.rm = TRUE))/sum(as.numeric(df_hs$V2), na.rm = TRUE)
  appo_name <- c(appo_name, name)
  diff <- c(diff, appo)
}


# Saving to file
df_print <- data.frame(
  name = appo_name, 
  diff = diff
)
write.table(df_print, "Results/HS_MODIS_diff.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)
