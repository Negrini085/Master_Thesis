# The main goal of this script is to convert SWE series generated via the model 
# from classic annual format to hydrological format.
rm(list = ls())
gc()

repo_name <- "SNWD"
fname <- paste0("Dataset/model_runs/raw/", repo_name, "/lista_sdh")
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/")


# Importing list of stations that have been taken care of
df_list <- read.table(fname)
station_names <- sub("^D_", "V_", df_list$V1)


# Cycle over stations
for(name in station_names){
  
  # Importing temporal series
  df <- read.table(paste0("Dataset/model_runs/raw/", repo_name, "/", name))
  years <- unique(as.numeric(df$V1))
  swe_series <- as.numeric(df$V5)
  
  # Arrays to contain datas
  appo_swe <- numeric(0)
  appo_year <- numeric(0)
  
  # Cycle over years
  start <- 244
  for(y in years[2:length(years)]){
    
    if(y %% 4 == 0) len <- 366
    else len <- 365

    # Selecting swe for a given year
    hydro_swe <- swe_series[start:(start + len - 1)]
    start <- start + len

    mask <- hydro_swe == -90 | is.nan(hydro_swe)
    hydro_swe[mask] <- NA

    # If the year isn't leap, I want to delete -90 value on the 29th
    if(sum(is.na(hydro_swe), na.rm = TRUE) >= 2){
      next
    }
    # else if(y %% 4 != 0){
    #   len <- 366
    #   mask <- !is.na(hydro_swe)
    #   hydro_swe <- hydro_swe[mask]
    # 
    #   if(length(hydro_swe) != 365){
    #     print(length(hydro_swe))
    #     stop(paste0("Problems with missing datas in station ", name, " during ", y))
    #   }
    # }

    appo_year <- c(appo_year, rep(y, length(hydro_swe)))
    appo_swe <- c(appo_swe, hydro_swe)
  }
  
  # Saving hydro years to file
  df_print <- data.frame(year = appo_year, swe = appo_swe)
  write.table(df_print, paste0("Dataset/model_runs/hydro/", repo_name, "/", name), row.names = FALSE, col.names = FALSE, quote = FALSE)
  print(paste0("Made conversion for ", name))
}