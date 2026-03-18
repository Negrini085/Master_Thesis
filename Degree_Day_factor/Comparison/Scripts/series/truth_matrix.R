# The main goal of this script is to make a truth matrix taking care of every 
# compatible station series in order to asses simple hs filter performances.
rm(list = ls())
gc()

library(ggplot2)

fname <- "../MODIS_series/Datas/compatible/start_end_years_filtered.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Comparison/")


# Importing station names to asses which ones have the longest recording streak
df <- read.table(fname, header = FALSE)
station_names <- df$V1


# Cycle on station names
truth_mat <- array(0, dim = c(4))
for(name in station_names){
  
  # Importing MODIS and station snow cover series
  fname_MOD <- paste0("../MODIS_series/Datas/modis_hydrological/compatible/", name)
  df_MOD <- read.table(fname_MOD, header = FALSE)
  
  fname_STA <- paste0("../STATION_series/Datas/sc_series/raw/", name)
  df_STA <- read.table(fname_STA, header = FALSE)
  
  
  # Now taking only datas which appear in both df
  mask <- df_STA$V1 %in% df_MOD$V1
  df_STA <- df_STA[mask, ]
  
  mask <- df_MOD$V1 %in% df_STA$V1
  df_MOD <- df_MOD[mask, ]
  
  
  # Checking on snow cover series length (those should be the same) and omitting NAs
  appo_MOD <- as.numeric(df_MOD$V2)
  appo_STA <- as.numeric(df_STA$V2)

  mask <- !is.na(appo_STA)
  appo_MOD <- appo_MOD[mask]
  appo_STA <- appo_STA[mask]

  
  # Making checks on values
  mask <- appo_MOD == 1 & appo_STA == 1
  truth_mat[1] <- truth_mat[1] + sum(mask, na.rm = TRUE)
  
  mask <- appo_MOD == 1 & appo_STA == 0
  truth_mat[2] <- truth_mat[2] + sum(mask, na.rm = TRUE)
  
  mask <- appo_MOD == 0 & appo_STA == 1
  truth_mat[3] <- truth_mat[3] + sum(mask, na.rm = TRUE)
  
  mask <- appo_MOD == 0 & appo_STA == 0
  truth_mat[4] <- truth_mat[4] + sum(mask, na.rm = TRUE)
  
  
  # Message
  print(paste0("Taken care of ", name, " series."))
}

print(truth_mat)



# Plotting procedure
cm_df <- data.frame(
  MOD = factor(c("Snow",    "Snow",      "No Snow", "No Snow"), levels = c("Snow", "No Snow")),
  STA = factor(c("Snow",    "No Snow",   "Snow",    "No Snow"), levels = c("Snow", "No Snow")),
  val = truth_mat
)

total <- sum(truth_mat)
cm_df$pct <- paste0(cm_df$val, "\n(", round(100 * cm_df$val / total, 1), "%)")

p_cm <- ggplot(cm_df, aes(x = STA, y = MOD, fill = val)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = pct), size = 4.5, fontface = "bold") +
  scale_fill_gradient(low = "#d6eaf8", high = "#1a5276") +
  scale_y_discrete(limits = c("No Snow", "Snow")) +  # Snow in alto
  labs(
    title = "Confusion Matrix",
    x     = "Station",
    y     = "MODIS"
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