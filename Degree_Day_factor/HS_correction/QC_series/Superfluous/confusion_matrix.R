# The main goal of this script is to create a confusion matrix in order to asses
# whether model predictions are in agreement with SWE series derived from HS 
# measurements via DeltaSnow.
rm(list = ls())
gc()

library(ggplot2)
library(nixmass)
library(patchwork)

fname_hs_dataset <- "../Dataset/hs_series/all_complete/all_complete.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/QC_series/")


# Importing station names and years to be taken care of
df <- read.table(fname_hs_dataset)
station_names <- unique(df$V1)
all_years <- as.numeric(df$V2)


# Cycle across stations
appo_matrix <- array(0, dim = c(4))
for(name in station_names){
  
  # Importing hs dataset for a given station
  df_hs <- read.table(paste0("../Dataset/hs_series/all_complete/", name))
  hs_series <- as.numeric(df_hs$V2)
  hs_years <- as.numeric(df_hs$V1)
  
  # Importing swe dataset for a given station
  df_swe <- read.table(paste0("../Dataset/model_runs/hydro/SNWD/V_SDH_", sub("HSD_", "", name)))
  swe_series <- as.numeric(df_swe$V2)
  swe_years <- as.numeric(df_swe$V1)
  
  # Selecting hydrological years that can be a part of our comparison
  mask <- df$V1 == name
  station_years <- unique(all_years[mask])
  
  # Cycle across years
  for(y in station_years){
    
    # Selecting hs datas for a given year
    mask <- hs_years == y
    hs_hydro <- hs_series[mask]/100
    if(hs_hydro[1] != 0) next
    
    # Creating dataset as input to deltasnow model
    dates <- seq(as.Date(paste0(y-1, "-09-01")), as.Date(paste0(y, "-08-31")), by = "day")
    hsdata <- data.frame(date = dates, hs = hs_hydro)
    
    # SWE conversion
    swe_from_hs <- swe.delta.snow(hsdata)
    
    # Selecting model SWE datas
    mask <- swe_years == y
    swe_from_model <- swe_series[mask]

    
    # Updating matrix
    mask <- swe_from_hs > 0 & swe_from_model > 0
    appo_matrix[1] <- appo_matrix[1] + sum(mask, na.rm = TRUE)
    
    mask <- swe_from_hs > 0 & swe_from_model == 0
    appo_matrix[2] <- appo_matrix[2] + sum(mask, na.rm = TRUE)
    
    mask <- swe_from_hs == 0 & swe_from_model > 0
    appo_matrix[3] <- appo_matrix[3] + sum(mask, na.rm = TRUE)
    
    mask <- swe_from_hs == 0 & swe_from_model == 0
    appo_matrix[4] <- appo_matrix[4] + sum(mask, na.rm = TRUE)
  }
  
  print(paste0("Taken care of ", name))
}




# Plotting procedure
cm_df <- data.frame(
  DELTA = factor(c("SWE",    "SWE",      "No SWE", "No SWE"), levels = c("SWE", "No SWE")),
  MODEL = factor(c("SWE",    "No SWE",   "SWE",    "No SWE"), levels = c("SWE", "No SWE")),
  val = appo_matrix
)
total <- sum(appo_matrix)
cm_df$pct <- paste0(cm_df$val, "\n(", round(100 * cm_df$val / total, 1), "%)")

p_cm <- ggplot(cm_df, aes(x = MODEL, y = DELTA, fill = val)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = pct), size = 4.5, fontface = "bold") +
  scale_fill_gradient(low = "#d6eaf8", high = "#1a5276") +
  scale_y_discrete(limits = c("No SWE", "SWE")) +
  labs(
    title = "Confusion Matrix",
    x     = "Model",
    y     = "DeltaSnow"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        panel.grid = element_blank())

ggsave(
  filename = "truth_mat.png",
  plot     = p_cm,
  dpi      = 300,
  width    = 10,
  height   = 10
)