# The main goal of this script is to generate confusion matrices for investigating 
# whether some elevation bias exists or not.
rm(list = ls())
gc()

library(ggplot2)
library(patchwork)

fname_ana <- "../HS_series/Correct/STATION_check/Dataset/ANAGRAFICA"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_calibration/")



# Importing anagrafica for selecting stations on a given elevation band
df <- read.table(fname_ana, header = TRUE)
df <- df[df$flag != "NO", ]
ele <- as.numeric(df$ele)
station_names <- df$name

files <- list.files(path = "Results/raw", full.names = TRUE)
names_swe <- sub("Results/raw/V_SDH", "HSD", files)



################################################################################
#                         First elevation band                                 #
################################################################################
mask <- ele <= 1000
appo_names <- station_names[mask]
appo_names <- intersect(names_swe, appo_names)
# print(sum(mask, na.rm = TRUE))

truth_mat <- array(0, dim = c(4))
for(name in appo_names){
  
  # Importing mine and Michele series
  fname_MINE <- paste0("Results/hydro/", sub("HSD", "V_SDH", name))
  df_MINE <- read.table(fname_MINE, header = FALSE)
  
  fname_HS <- paste0("../HS_series/Correct/Dataset/", name)
  df_HS <- read.table(fname_HS, header = FALSE)
  years <- unique(as.numeric(df_HS$V1))
  
  
  # Cycle on station years
  for(y in unique(years)){
    
    if(y > 2023) next
    
    # Selecting SWE and HS series
    mask <- as.numeric(df_MINE$V1) == y
    appo_MINE <- as.numeric(df_MINE$V2)[mask]
    
    mask <- as.numeric(df_HS$V1) == y
    appo_HS <- as.numeric(df_HS$V2)[mask]
    
    if(length(appo_MINE) != length(appo_HS)) stop(paste0("No compatible length for SWE & HS series at ", name, " during ", y))
    
    
    # Making checks on values
    mask <- appo_MINE > 0 & appo_HS > 0
    truth_mat[1] <- truth_mat[1] + sum(mask, na.rm = TRUE)
    
    mask <- appo_MINE > 0 & appo_HS == 0
    truth_mat[2] <- truth_mat[2] + sum(mask, na.rm = TRUE)
    
    mask <- appo_MINE == 0 & appo_HS > 0
    truth_mat[3] <- truth_mat[3] + sum(mask, na.rm = TRUE)
    
    mask <- appo_MINE == 0 & appo_HS == 0
    truth_mat[4] <- truth_mat[4] + sum(mask, na.rm = TRUE)
  }
  
  
  # Message
  print(paste0("Taken care of ", name, " series."))
}
print(truth_mat)



# Plotting procedure
cm_df <- data.frame(
  MINE = factor(c("Yes",    "Yes",      "No", "No"), levels = c("Yes", "No")),
  HS = factor(c("Yes",    "No",   "Yes",    "No"), levels = c("Yes", "No")),
  val = truth_mat
)

total <- sum(truth_mat)
cm_df$pct <- paste0(cm_df$val, "\n(", round(100 * cm_df$val / total, 1), "%)")

p1 <- ggplot(cm_df, aes(x = HS, y = MINE, fill = val)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = pct), size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#d6eaf8", high = "#1a5276") +
  scale_y_discrete(limits = c("No", "Yes")) +
  labs(
    title = "0 - 1000 m a.s.l.",
    x     = "HS",
    y     = "Model"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )





################################################################################
#                         Second elevation band                                #
################################################################################
mask <- ele > 1000 & ele <= 2000
appo_names <- station_names[mask]
appo_names <- intersect(names_swe, appo_names)
# print(sum(mask, na.rm = TRUE))

truth_mat <- array(0, dim = c(4))
for(name in appo_names){
  
  # Importing mine and Michele series
  fname_MINE <- paste0("Results/hydro/", sub("HSD", "V_SDH", name))
  df_MINE <- read.table(fname_MINE, header = FALSE)
  
  fname_HS <- paste0("../HS_series/Correct/Dataset/", name)
  df_HS <- read.table(fname_HS, header = FALSE)
  years <- unique(as.numeric(df_HS$V1))
  
  
  # Cycle on station years
  for(y in unique(years)){
    
    if(y > 2023) next
    
    # Selecting SWE and HS series
    mask <- as.numeric(df_MINE$V1) == y
    appo_MINE <- as.numeric(df_MINE$V2)[mask]
    
    mask <- as.numeric(df_HS$V1) == y
    appo_HS <- as.numeric(df_HS$V2)[mask]
    
    if(length(appo_MINE) != length(appo_HS)) stop(paste0("No compatible length for SWE & HS series at ", name, " during ", y))
    
    
    # Making checks on values
    mask <- appo_MINE > 0 & appo_HS > 0
    truth_mat[1] <- truth_mat[1] + sum(mask, na.rm = TRUE)
    
    mask <- appo_MINE > 0 & appo_HS == 0
    truth_mat[2] <- truth_mat[2] + sum(mask, na.rm = TRUE)
    
    mask <- appo_MINE == 0 & appo_HS > 0
    truth_mat[3] <- truth_mat[3] + sum(mask, na.rm = TRUE)
    
    mask <- appo_MINE == 0 & appo_HS == 0
    truth_mat[4] <- truth_mat[4] + sum(mask, na.rm = TRUE)
  }
  
  
  # Message
  print(paste0("Taken care of ", name, " series."))
}
print(truth_mat)



# Plotting procedure
cm_df <- data.frame(
  MINE = factor(c("Yes",    "Yes",      "No", "No"), levels = c("Yes", "No")),
  HS = factor(c("Yes",    "No",   "Yes",    "No"), levels = c("Yes", "No")),
  val = truth_mat
)

total <- sum(truth_mat)
cm_df$pct <- paste0(cm_df$val, "\n(", round(100 * cm_df$val / total, 1), "%)")

p2 <- ggplot(cm_df, aes(x = HS, y = MINE, fill = val)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = pct), size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#d6eaf8", high = "#1a5276") +
  scale_y_discrete(limits = c("No", "Yes")) +
  labs(
    title = "1000 - 2000 m a.s.l.",
    x     = "HS",
    y     = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )





################################################################################
#                         Third elevation band                                 #
################################################################################
mask <- ele > 2000
appo_names <- station_names[mask]
appo_names <- intersect(names_swe, appo_names)
# print(sum(mask, na.rm = TRUE))

truth_mat <- array(0, dim = c(4))
for(name in appo_names){
  
  # Importing mine and Michele series
  fname_MINE <- paste0("Results/hydro/", sub("HSD", "V_SDH", name))
  df_MINE <- read.table(fname_MINE, header = FALSE)
  
  fname_HS <- paste0("../HS_series/Correct/Dataset/", name)
  df_HS <- read.table(fname_HS, header = FALSE)
  years <- unique(as.numeric(df_HS$V1))
  
  
  # Cycle on station years
  for(y in unique(years)){
    
    if(y > 2023) next
    
    # Selecting SWE and HS series
    mask <- as.numeric(df_MINE$V1) == y
    appo_MINE <- as.numeric(df_MINE$V2)[mask]
    
    mask <- as.numeric(df_HS$V1) == y
    appo_HS <- as.numeric(df_HS$V2)[mask]
    
    if(length(appo_MINE) != length(appo_HS)) stop(paste0("No compatible length for SWE & HS series at ", name, " during ", y))
    
    
    # Making checks on values
    mask <- appo_MINE > 0 & appo_HS > 0
    truth_mat[1] <- truth_mat[1] + sum(mask, na.rm = TRUE)
    
    mask <- appo_MINE > 0 & appo_HS == 0
    truth_mat[2] <- truth_mat[2] + sum(mask, na.rm = TRUE)
    
    mask <- appo_MINE == 0 & appo_HS > 0
    truth_mat[3] <- truth_mat[3] + sum(mask, na.rm = TRUE)
    
    mask <- appo_MINE == 0 & appo_HS == 0
    truth_mat[4] <- truth_mat[4] + sum(mask, na.rm = TRUE)
  }
  
  
  # Message
  print(paste0("Taken care of ", name, " series."))
}
print(truth_mat)



# Plotting procedure
cm_df <- data.frame(
  MINE = factor(c("Yes",    "Yes",      "No", "No"), levels = c("Yes", "No")),
  HS = factor(c("Yes",    "No",   "Yes",    "No"), levels = c("Yes", "No")),
  val = truth_mat
)

total <- sum(truth_mat)
cm_df$pct <- paste0(cm_df$val, "\n(", round(100 * cm_df$val / total, 1), "%)")

p3 <- ggplot(cm_df, aes(x = HS, y = MINE, fill = val)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = pct), size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#d6eaf8", high = "#1a5276") +
  scale_y_discrete(limits = c("No", "Yes")) +
  labs(
    title = "> 2000 m a.s.l.",
    x     = "HS",
    y     = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )


(p1 + p2 + p3) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 15))