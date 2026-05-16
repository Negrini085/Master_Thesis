# The main goal of this script is to create lists to divide manual checks between
# the members of the team
rm(list = ls())
gc()

fname_with_gaps <- "../Dataset/hs_series/with_gaps/with_gaps.dat"
fname_complete <- "../Dataset/hs_series/all_complete/all_complete.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/QC_series/")


# Importing df for complete and gapped series
df_complete <- read.table(fname_complete)
df_complete <- data.frame(name = df_complete$V1, year = as.numeric(df_complete$V2), mask = rep("com", nrow(df_complete)))

df_with_gaps <- read.table(fname_with_gaps)
df_with_gaps <- data.frame(name = df_with_gaps$V1, year = as.numeric(df_with_gaps$V2), mask = rep("gap", nrow(df_with_gaps)))

df_total <- rbind(df_complete, df_with_gaps)



# Selecting only series which took place before 2024
mask <- as.numeric(df_total$year) < 2024
df_total <- df_total[mask, ]

print(paste0("Total numeber of hydrological years taken care of: ", nrow(df_total)))
print(paste0("Total number of stations taken care of: ", length(unique(df_total$name))))



# Selecting only stations which have model SWE
appo_names <- df_total$name
appo_names <- sub("HSD_", "DV_SDH_", appo_names)

fname <- paste0("../Dataset/model_runs/hydro/SNWD/", appo_names)
mask_model_swe <- file.exists(fname)
df_total <- df_total[mask_model_swe, ]



# Selecting stations for given regions. I will divide stations based on the nation they 
# sit on: Italy, France, Swiss and Austria.
mask_swiss <- grepl("HSD_CH_", df_total$name)
df_swiss <- df_total[mask_swiss, ]
print(paste0("Number of swiss station is ", length(unique(df_swiss$name))))

mask_at <- grepl("HSD_AT_", df_total$name)
df_at <- df_total[mask_at, ]
print(paste0("Number of austrian station is ", length(unique(df_at$name))))

mask_fr <- grepl("HSD_FR_", df_total$name)
df_fr <- df_total[mask_fr, ]
print(paste0("Number of french station is ", length(unique(df_fr$name))))

mask_it <- !mask_swiss & !mask_at & !mask_fr
df_it <- df_total[mask_it, ]
print(paste0("Number of italian station is ", length(unique(df_it$name))))



# Dividing stations between five lists
name_swiss <- unique(df_swiss$name)
name_at <- unique(df_at$name)
name_fr <- unique(df_fr$name)
name_it <- unique(df_it$name)

for(i in 1:4){
  
  # Selecting swiss stations
  sel_swiss <- sample(name_swiss, size = 27)
  name_swiss <- name_swiss[!name_swiss %in% sel_swiss]
  
  # Selecting austrian stations
  sel_at <- sample(name_at, size = 34)
  name_at <- name_at[!name_at %in% sel_at]
  
  # Selecting french stations
  sel_fr <- sample(name_fr, 40)
  name_fr <- name_fr[!name_fr %in% sel_fr]
  
  # Selecting italian stations
  sel_it <- sample(name_it, 49)
  name_it <- name_it[!name_it %in% sel_it]
  
  
  # Saving stations to file
  sel_names <- c(sel_swiss, sel_at, sel_fr, sel_it)
  df_print <- data.frame(name = sel_names)
  write.table(df_print, paste0("Series_to_send/selected_", sprintf("%02d", i), ".dat"), row.names = FALSE, col.names = FALSE, quote = FALSE)
}

final_names <- c(name_swiss, name_at, name_fr, name_it)
df_print <- data.frame(name = final_names)
write.table(df_print, "Series_to_send/stations_filippo.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)