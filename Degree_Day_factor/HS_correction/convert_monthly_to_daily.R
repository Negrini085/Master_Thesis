# The main goal is to convert file with monthly raws to file with daily raws
rm(list = ls())
gc()

fname_list <- "Dataset/model_runs/raw/SNWD/lista"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/")

# Importing names
df <- read.table(fname_list)
station_names <- df$V1
station_names <- sub("D_HSD", "SDH", station_names)

# Number of days for a given month (here I don't want to get rid of missing data for the 29th of April)
num_days <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

for(name in station_names){
  
  # Appo variables
  appo_data <- numeric(0)
  appo_days <- numeric(0)
  appo_years <- numeric(0)
  appo_count <- numeric(0)
  appo_months <- numeric(0)
  
  # Importing dataframe for a single station
  fname <- paste0("Dataset/model_runs/raw/SNWD/", name)
  df_sta <- read.table(fname, fill = TRUE)
  
  for(i in 1:nrow(df_sta)){
    
    # Selecting year and month
    year <- as.numeric(df_sta[i, 1])
    month <- as.integer(df_sta[i, 2])
    
    # Selecting datas
    if(year %% 4 != 0 & month == 2){
      appo_days <- c(appo_days, seq(1, num_days[month], 1))
      appo_years <- c(appo_years, rep(year, num_days[month]))
      appo_months <- c(appo_months, rep(month, num_days[month]))
      appo_data <- c(appo_data, as.numeric(df_sta[i,3:(1+num_days[month])]), -90.0)
    }
    else{
      appo_days <- c(appo_days, seq(1, num_days[month], 1))
      appo_years <- c(appo_years, rep(year, num_days[month]))
      appo_months <- c(appo_months, rep(month, num_days[month]))
      appo_data <- c(appo_data, as.numeric(df_sta[i,3:(2+num_days[month])]))
    }
    
  }
  
  # Day counter (from first to last day)
  appo_count <- seq(1, length(appo_months), 1)
  df_print <- data.frame(
    year = appo_years, 
    month = appo_months, 
    day = appo_days, 
    count = appo_count, 
    data = appo_data
  )
  
  fname_out <- paste0("Dataset/model_runs/raw/SNWD/DV_", name)
  write.table(df_print, fname_out, row.names = FALSE, col.names = FALSE, quote = FALSE)
  print(paste0("Made conversion for ", name))
}