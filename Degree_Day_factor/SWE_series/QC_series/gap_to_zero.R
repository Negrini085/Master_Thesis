# The main goal of this script is to fill missing datas with zeros. I will have to use
# this kind of script only on few occasions, but will make everything faster.
rm(list = ls())
gc()

name <- "HSD_TAA_TN_MARMOLADA_SAS_DEL_MUL"
fname <- paste0("Dataset/Gap/", name)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/QC_series/")



# Importing HS series
df <- read.table(fname)
appo_y <- as.numeric(df$V1)
appo_hs <- as.numeric(df$V2)


# Cycle in order to asses if some years are only made of NAs or zeros
years <- numeric(0)
hs_series <- numeric(0)
for(y in unique(appo_y)){
  
  if(y == 2024) next
  
  # Selecting datas for a given hydrological year
  mask <- appo_y == y
  appo <- appo_hs[mask]
  
  # Checking if it's only made of zeros or NAs
  mask <- is.na(appo) | appo == 0
  if(all(mask)) next
  else{
    years <- c(years, rep(y, length(appo)))
    hs_series <- c(hs_series, appo)
  }
}

if(length(hs_series) != length(years)) stop(paste0("Length mismatch for ", name))
if(length(years) == 0) stop(paste0("No valid datas for ", name))

# Selecting gaps and filling them
mask <- is.na(hs_series)
hs_series[mask] <- 0



# Saving series to tmp directory
df_print <- data.frame(
  years = years, 
  hs_series = hs_series
)

write.table(df_print, paste0("Dataset/Tmp/", name), row.names = FALSE, col.names = FALSE, quote = FALSE)