# The main goal of this script is to check whether a bug is present on 
# solid precipitation computations.
rm(list = ls())
gc()

t_th <- 2.5
fname_name <- "Dataset/PCPD/lista"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/What/")

tp_series <- read.table(fname_name, header = FALSE)
name_tp <- tp_series$V1

for(name in name_tp){
  
  # Importing temperature and precipitation series
  df_tmax <- read.table(paste0("Dataset/TMXD/", name), header = FALSE)
  mask <- as.numeric(df_tmax$V1) <= 2023
  tmax <- as.numeric(df_tmax$V5)[mask]
  
  df_tmin <- read.table(paste0("Dataset/TMND/", name), header = FALSE)
  mask <- as.numeric(df_tmin$V1) <= 2023
  tmin <- as.numeric(df_tmin$V5)[mask]
  
  df_prec <- read.table(paste0("Dataset/PCPD/", name), header = FALSE)
  prec <- as.numeric(df_prec$V5)
  
  df_precs <- read.table(paste0("Dataset/SNW/", sub("DV_HSD", "DV_SNW", name)))
  precs <- as.numeric(df_precs$V5)
  
  
  # Check on container length
  if(length(tmax) != length(tmin)) stop(paste0("Length mismatch for temperature series at ", name))
  if(length(tmin) != length(prec)) stop(paste0("Length mismatch for temperature and precipitation series at ", name))
  if(length(prec) != length(precs)) stop(paste0("Length mismatch for precipitation series at ", name))
  
  
  # Looking for input series bug
  mask <- prec < 0
  if(any(mask, na.rm = TRUE)) next
  
  mask <- tmax <= tmin
  if(any(mask, na.rm = TRUE)) next
  
  
  # Checking conditions for mixed precipitations
  mask <- tmin < t_th & tmax > t_th & prec > 0
  appo_precs <- prec[mask] * (t_th - tmin[mask])/(tmax[mask] - tmin[mask])
  pippo_precs <- precs[mask]
  
  if(any(pippo_precs == appo_precs)) print(paste0("Mixed precipitations equal at " , name, " on at least one occasion!"))
  if(any(pippo_precs != 0)) print(paste0("At least one day with non-zero mixed precipitations according to Michele at ", name))
  
  
  # Checking conditions for only solid precipitations
  mask <- tmax <= t_th & prec > 0
  appo_precs <- prec[mask]
  pippo_precs <- precs[mask]
  
  if(any(appo_precs != pippo_precs)) print(paste0("Whole precipitations different at " , name, " on at least one occasion!"))
  
  
  # Checking non-zero solid precipitations
  mask <- precs > 0
  if(any(tmax[mask] > t_th)) print(paste0("Not only whole solid precipitations at ", name))
}