# The main goal of this script it to find the maximum value of HS and SWE for a 
# given station, in order to be able to produce standardized plots.
rm(list = ls())
gc()

library(nixmass)

fname_with_gaps <- "../Dataset/hs_series/with_gaps/with_gaps.dat"
fname_complete <- "../Dataset/hs_series/all_complete/all_complete.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/HS_correction/QC_series/")


# Function to find maximum HS
compute_max_hs <- function(df_hs, hs_years, name){
  
  # New names for dataset columns
  year_for_max <- as.numeric(df_hs$V1)
  hs_for_max <- as.numeric(df_hs$V2)
  
  # Selecting only years which will be plotted (no 2024 or 2025)
  mask <- year_for_max != 2024 & year_for_max != 2025
  hs_for_max <- hs_for_max[mask]
  year_for_max <- year_for_max[mask]
  
  # Selecting only those year which will be plotted
  mask <- year_for_max %in% hs_years
  hs_for_max <- hs_for_max[mask]
  
  # Checking if we have a void hs container (or a container which does not have at least 
  # a year worth of datas)
  if(length(hs_for_max) < 365) stop(paste0("Not enough datas for ", name))
  
  # Finding actual hs max
  appo_max <- max(hs_for_max, na.rm = TRUE)
  return(appo_max)
}



# Function to compute maximum SWE value from model
compute_max_swe_from_model <- function(df_swe_model, total_years, name){
  
  # Selecting only those years which are covered in our HS dataset
  mask <- as.numeric(df_swe_model$V1) %in% total_years
  appo <- as.numeric(df_swe_model$V2)[mask]
  
  if(!any(mask)) stop(paste0("Not enough SWE datas for ", name))
  
  return(max(appo, na.rm = TRUE))
}



# Function to compute maximum SWE from DeltaSnow conversion
compute_max_swe_from_delta <- function(df_hs, delta_years, name){
  
  # Selecting hydrological years to convert into swe
  mask <- as.numeric(df_hs$V1) %in% delta_years
  appo_years <- as.numeric(df_hs$V1)[mask]
  appo_hs <- as.numeric(df_hs$V2)[mask]
  
  # Cycle over years to convert single hydrological series
  appo_swe <- numeric(0)
  for(y in unique(appo_years)){
    
    # Selecting single hydrological year
    mask <- appo_years == y
    hs_hydro <- appo_hs[mask]
    dates <- seq(as.Date(paste0(y-1, "-09-01")), as.Date(paste0(y, "-08-31")), by = "day")
    
    if(hs_hydro[1] != 0){
      hs_hydro <- c(0, hs_hydro)
      dates <- seq(as.Date(paste0(y-1, "-08-31")), as.Date(paste0(y, "-08-31")), by = "day")
    }
    
    # HS to SWE conversion
    hsdata <- data.frame(date = dates, hs = hs_hydro/100)
    tryCatch({
      appo_swe <- c(appo_swe, swe.delta.snow(hsdata, dyn_rho_max = FALSE))
    }, error = function(e) {
      stop(paste0("Delta snow fallito per ", name, " anno ", y, ": ", e$message))
    })
  }
  
  return(max(appo_swe, na.rm = TRUE))
}






# Importing complete and with gaps metadatas
df_complete <- read.table(fname_complete)
df_complete <- data.frame(name = df_complete$V1, year = as.numeric(df_complete$V2), status = rep("com", length(df_complete$V1)))

df_with_gaps <- read.table(fname_with_gaps)
df_with_gaps <- data.frame(name = df_with_gaps$V1, year = as.numeric(df_with_gaps$V2), status = rep("gap", length(df_with_gaps$V1)))

df_tot <- rbind(df_complete, df_with_gaps)



# Cycle over stations
max_hs <- numeric(0)
max_swe <- numeric(0)
appo_name <- character(0)
for(name in unique(df_tot$name)){
  
  # Checking if the model can reproduce swe series or not
  snw_name <- paste0("../Dataset/model_runs/hydro/SNWD/", sub("HSD_", "DV_SDH_", name))
  if(!file.exists(snw_name)) next
  
  
  # Importing hs dataset for a given station and finding the maximum HS
  fname <- paste0("../../Original/STATION_series/Dataset/station_series/", name)
  df_hs <- read.table(fname)
  hs_years <- unique(as.numeric(df_hs$V1))
  if(min(hs_years, na.rm = TRUE) > 2023) next
  
  appo_hs <- compute_max_hs(df_hs = df_hs, hs_years = hs_years, name = name)
  max_hs <- c(max_hs, appo_hs)
  
  
  # Importing swe from model fora given station and finding maximum SWE from model
  fname <- paste0("../Dataset/model_runs/hydro/SNWD/", sub("HSD_", "DV_SDH_", name))
  df_swe_model <- read.table(fname)
  
  mask <- df_tot$name == name
  total_years <- unique(as.numeric(df_tot$year)[mask])
  appo_swe_from_model <- compute_max_swe_from_model(df_swe_model = df_swe_model, total_years = total_years, name = name)
  
  
  # About to compute swe using model delta.snow (only for complete years)
  mask <- df_complete$name == name
  appo_swe_delta <- 0
  if(any(mask)){
    delta_years <- unique(as.numeric(df_complete$year)[mask])
    appo_swe_delta <- compute_max_swe_from_delta(df_hs = df_hs, delta_years = delta_years, name = name)
  }
  
  max_swe <- c(max_swe, max(c(appo_swe_delta, appo_swe_from_model), na.rm = TRUE))
  
  appo_name <- c(appo_name, name)
  print(paste0("Tehen care of: ", name, " - max HS: ", appo_hs, " - max SWE: ", max(c(appo_swe_delta, appo_swe_from_model), na.rm = TRUE)))
}



# Check for NAs
if(any(is.na(max_hs))) {
  warning(paste0("Found ", sum(is.na(max_hs)), " NAs in max_hs"))
  print(paste0("Stations which have NAs in max_hs: ", paste(appo_name[is.na(max_hs)], collapse = ", ")))
}

if(any(is.na(max_swe))) {
  warning(paste0("Found ", sum(is.na(max_swe)), " NAs in max_swe"))
  print(paste0("Stations which have NAs in max_swe: ", paste(appo_name[is.na(max_swe)], collapse = ", ")))
}



# Saving datas to file
df_print <- data.frame(
  name = appo_name, 
  max_hs = max_hs, 
  max_swe = max_swe
)

write.table(df_print, "Results/max_hs_swe_values.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)