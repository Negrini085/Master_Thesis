# The main goal of this script is to create a confusion matrix in order to asses
# whether model predictions are in agreement with HS measurements.
rm(list = ls())
gc()

library(ggplot2)
library(patchwork)

fname_hs <- "../HS_series/Correct/STATION_check/Dataset/ANAGRAFICA"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Calibration/")


# Importing station names and years to be taken care of
df <- read.table(fname_hs, header = TRUE)
station_names <- unique(df$station_name)


# Cycle across stations
appo_matrix <- array(0, dim = c(4))
for(name in station_names){
  
  # Importing hs dataset for a given station
  df_hs <- read.table(paste0("../HS_series/Correct/Dataset/", name))
  station_years <- unique(as.numeric(df_hs$V1))
  hs_series <- as.numeric(df_hs$V2)
  hs_years <- as.numeric(df_hs$V1)
  
  # Importing swe dataset for a given station
  df_swe <- read.table(paste0("../HS_series/HS_correction/Dataset/model_runs/hydro/SNWD/DV_SDH_", sub("HSD_", "", name)))
  swe_series <- as.numeric(df_swe$V2)
  swe_years <- as.numeric(df_swe$V1)
  
  # Cycle across years
  for(y in station_years){
    
    # Selecting hs datas for a given year
    mask <- hs_years == y
    hs_hydro <- hs_series[mask]
    
    # Selecting model SWE datas
    mask <- swe_years == y
    swe_from_model <- swe_series[mask]

    
    # Updating matrix
    mask <- hs_hydro > 0 & swe_from_model > 0
    appo_matrix[1] <- appo_matrix[1] + sum(mask, na.rm = TRUE)
    
    mask <- hs_hydro > 0 & swe_from_model == 0
    appo_matrix[2] <- appo_matrix[2] + sum(mask, na.rm = TRUE)
    
    mask <- hs_hydro == 0 & swe_from_model > 0
    appo_matrix[3] <- appo_matrix[3] + sum(mask, na.rm = TRUE)
    
    mask <- hs_hydro == 0 & swe_from_model == 0
    appo_matrix[4] <- appo_matrix[4] + sum(mask, na.rm = TRUE)
  }
  
  print(paste0("Taken care of ", name))
}




# Plotting procedure
cm_df <- data.frame(
  DELTA = factor(c("Snow",    "Snow",      "No snow", "No snow"), levels = c("Snow", "No snow")),
  MODEL = factor(c("Snow",    "No snow",   "Snow",    "No snow"), levels = c("Snow", "No snow")),
  val = appo_matrix
)
total <- sum(appo_matrix)
cm_df$pct <- paste0(cm_df$val, "\n(", round(100 * cm_df$val / total, 1), "%)")

p_cm <- ggplot(cm_df, aes(x = MODEL, y = DELTA, fill = val)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = pct), size = 4.5, fontface = "bold") +
  scale_fill_gradient(low = "#d6eaf8", high = "#1a5276") +
  scale_y_discrete(limits = c("No snow", "Snow")) +
  labs(
    title = "Confusion Matrix",
    x     = "Model",
    y     = "HS series"
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