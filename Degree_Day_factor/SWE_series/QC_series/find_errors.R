# The main goal of this script is to find NAs or NaN values in model series in order
# to asses whether all datas are available or not
rm(list = ls())
gc()

repo <- "SNWD"
fname_ana <- "../../Ours/STATION_check/Correcting/ANAGRAFICA_CORRECT"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/QC_series/")


# Importing station names
df_ana <- read.table(fname_ana, header = TRUE)
station_names <- df_ana$station_name
station_names <- sub("^HSD_", "V_SDH_", station_names)


# Cycle across stations
appo_name <- character(0)
for(name in station_names){
  
  # Importing swe series
  fname <- paste0("../Dataset/model_runs/raw/", repo, "/", name)
  df <- read.table(fname)
  
  # Assesing series quality
  appo_swe <- df$V5
  if(!all(!is.nan(appo_swe))){
    print(paste0("Found some nan values for ", name))
    appo_name <- c(appo_name, name)
  }
}


# Saving faulty series
df <- data.frame(name = appo_name)
fname <- paste0("Results/", repo, "_nan.dat")
write.table(df, fname, row.names = FALSE, col.names = FALSE, quote = FALSE)