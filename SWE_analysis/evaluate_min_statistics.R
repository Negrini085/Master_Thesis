# The main goal of this script is to assess why SWE volume keeps accumulating 
# or not
rm(list = ls())
gc()

years <- 1953:2023
fname <- "Results/SWE/total_snowpack_metrics.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")


# Importing SWE datas
df <- read.table(fname, header = TRUE)
dates <- as.Date(df$data)
swe <- as.numeric(df$swe)
mean_swe <- as.numeric(df$mean_swe)
number_covered <- as.numeric(df$covered)
mean_over_covered <- as.numeric(df$mean_over_covered)



# Cycle over years
min_swe <- numeric(0)
min_mean_swe <- numeric(0)
min_number_covered <- numeric(0)
min_data <- as.Date(character(0))
min_mean_over_covered <- numeric(0)
for(y in years){
  if(y %% 4 == 0) next
  
  # Selecting datas for a given hydrological year
  filter_dates <- seq(from = as.Date(paste0(y-1, "-10-01")), to = as.Date(paste0(y, "-09-30")), by = "day")
  if((y-1) %% 4 == 0){
    uno <- seq(from = as.Date(paste0(y-2, "-10-01")), to = as.Date(paste0(y-2, "-12-31")), by = "day")
    due <- seq(from = as.Date(paste0(y, "-01-01")), to = as.Date(paste0(y, "-09-30")), by = "day")
    filter_dates <- c(uno, due)
  }
  mask <- dates %in% filter_dates
  swe_hydro <- swe[mask]
  mean_swe_hydro <- mean_swe[mask]
  number_covered_hydro <- number_covered[mask]
  mean_over_covered_hydro <- mean_over_covered[mask]
  
  
  
  # Finding maximum SWE volume and 1st April SWE volume
  appo_min <- min(swe_hydro, na.rm = TRUE)
  idx <- which(swe_hydro == appo_min)[1]

  min_swe <- c(min_swe, appo_min)
  min_data <- c(min_data, dates[mask][idx])
  min_mean_swe <- c(min_mean_swe, mean_swe_hydro[idx])
  min_number_covered <- c(min_number_covered, number_covered_hydro[idx])
  min_mean_over_covered <- c(min_mean_over_covered, mean_over_covered_hydro[idx])
  
}


# Saving to file
df_save <- data.frame(
  min_data = min_data,
  min_swe = min_swe,
  min_mean_swe = min_mean_swe,
  min_number_covered = min_number_covered,
  min_mean_over_covered = min_mean_over_covered
)
write.table(df_save, "Results/SWE/appo.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)