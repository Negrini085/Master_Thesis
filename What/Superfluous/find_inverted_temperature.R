# The main goal of this script is to find those days which have Tmax smaller than
# Tmin, in order to later tell Michele
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/What/")


# Variables
files <- list.files(path = "Dataset/TMND", full.names = TRUE)
files <- sub("Dataset/TMND/DV_", "", files)
appo_names <- character(0)
appo_month <- numeric(0)
appo_year <- numeric(0)
appo_tmax <- numeric(0)
appo_tmin <- numeric(0)
appo_day <- numeric(0)


# Cycle over precipitation dataset
for(name in files){
  
  # Importing precipitation series and looking for negative precipitations
  df_tmin <- read.table(paste0("Dataset/TMND/DV_", "", name))
  value_tmin <- as.numeric(df_tmin$V5)
  month_tmin <- as.numeric(df_tmin$V2)
  year_tmin <- as.numeric(df_tmin$V1)
  day_tmin <- as.numeric(df_tmin$V3)
  
  df_tmax <- read.table(paste0("Dataset/TMXD/DV_", "", name))
  value_tmax <- as.numeric(df_tmax$V5)
  month_tmax <- as.numeric(df_tmax$V2)
  year_tmax <- as.numeric(df_tmax$V1)
  day_tmax <- as.numeric(df_tmax$V3)  
  
  mask <- value_tmax < value_tmin
  if(any(mask)){
    cat(name, "\n")
    for(i in which(mask)){
      
      if(year_tmin[i] != year_tmax[i]) stop(paste0("Not compatible years for", name))
      if(month_tmin[i] != month_tmax[i]) stop(paste0("Not compatible month for", name))
      if(day_tmin[i] != day_tmax[i]) stop(paste0("Not compatible days for", name))
      
      appo_month <- c(appo_month, month_tmax[i])
      appo_tmin <- c(appo_tmin, value_tmin[i])
      appo_tmax <- c(appo_tmax, value_tmax[i])
      appo_year <- c(appo_year, year_tmax[i])
      appo_day <- c(appo_day, day_tmax[i])
      appo_names <- c(appo_names, name)
      
      cat(year_tmax[i], month_tmax[i], day_tmax[i], "\n")
    }
    cat("\n")
  }
}


df_print <- data.frame(
  name = appo_names, 
  year = appo_year,
  month = appo_month,
  day = appo_day,
  tmin = appo_tmin,
  tmax = appo_tmax
)
write.table(df_print, "Results/inverted_temperature.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)