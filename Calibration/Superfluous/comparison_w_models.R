# The main goal of this script is to compare the toy model swe series to Michele
# series, in order to check if our model is accurate or not.
rm(list = ls())
gc()

library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Calibration/")

# Importing station names
files <- list.files(path = "Results/raw", full.names = TRUE)
station_names <- sub("Results/raw/", "", files)


# Cycle over stations
truth_mat <- array(0, dim = c(4))
for(name in station_names){
  
  # Importing mine and Michele series
  fname_MINE <- paste0("Results/raw/", name)
  df_MINE <- read.table(fname_MINE, header = FALSE)
  
  fname_MICH <- paste0("../HS_series/HS_correction/Dataset/model_runs/raw/SNWD/", name)
  df_MICH <- read.table(fname_MICH, header = FALSE)
  
  
  # No more -90 in michele dataset
  mask <- df_MICH$V5 == -90
  df_MICH <- df_MICH[!mask, ]
  
  if(nrow(df_MICH) != nrow(df_MINE)) stop(paste0("No compatible length for SWE series at ", sub("DV_SDH", "HSD", name)))
  
  
  # Ready to compare series
  appo_MINE <- as.numeric(df_MINE$V4)
  appo_MICH <- as.numeric(df_MICH$V5)
  
  
  # Making checks on values
  mask <- appo_MINE > 0 & appo_MICH > 0
  truth_mat[1] <- truth_mat[1] + sum(mask, na.rm = TRUE)
  
  mask <- appo_MINE >0 & appo_MICH == 0
  truth_mat[2] <- truth_mat[2] + sum(mask, na.rm = TRUE)
  
  mask <- appo_MINE == 0 & appo_MICH > 0
  truth_mat[3] <- truth_mat[3] + sum(mask, na.rm = TRUE)
  
  mask <- appo_MINE == 0 & appo_MICH == 0
  truth_mat[4] <- truth_mat[4] + sum(mask, na.rm = TRUE)
  
  
  # Message
  print(paste0("Taken care of ", name, " series."))
}
print(truth_mat)



# Plotting procedure
cm_df <- data.frame(
  MINE = factor(c("SWE",    "SWE",      "No SWE", "No SWE"), levels = c("SWE", "No SWE")),
  MICH = factor(c("SWE",    "No SWE",   "SWE",    "No SWE"), levels = c("SWE", "No SWE")),
  val = truth_mat
)

total <- sum(truth_mat)
cm_df$pct <- paste0(cm_df$val, "\n(", round(100 * cm_df$val / total, 1), "%)")

p_cm <- ggplot(cm_df, aes(x = MICH, y = MINE, fill = val)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = pct), size = 4.5, fontface = "bold") +
  scale_fill_gradient(low = "#d6eaf8", high = "#1a5276") +
  scale_y_discrete(limits = c("No SWE", "SWE")) +  # Snow in alto
  labs(
    title = "Confusion Matrix",
    x     = "Michele",
    y     = "Mine"
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