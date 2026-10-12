# The main goal of this script is to make a comparison plot between corrected HS series 
# and MODIS values, in order to show the advantages of the correction.
rm(list = ls())
gc()

library(dplyr)
library(ggplot2)
library(patchwork)

fname_MOD     <- "../MODIS_series/Results/los.dat"
fname_STA     <- "../STATION_series/Results/los.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/Correct/Comparison/")


# Importing both dataset
df_MOD <- read.table(fname_MOD, header = TRUE)
df_MOD <- data.frame(name = df_MOD$station, year = as.numeric(df_MOD$year), los = as.numeric(df_MOD$los), mark = df_MOD$flag)

df_STA <- read.table(fname_STA, header = FALSE)
df_STA <- data.frame(name = df_STA$V1, year = as.numeric(df_STA$V2), los = as.numeric(df_STA$V3), mark = df_STA$V4)

mask <- df_MOD$name == "HSD_IT_PIE_TO_MONCENISIO_LAGO_MAN"
df_MOD <- df_MOD[!mask, ]

mask <- df_MOD$name == "HSD_IT_PIE_CN_LIMONE_PANCANI_2008m"
df_MOD <- df_MOD[!mask, ]


# Plotting procedure
df_plot <- merge(df_MOD, df_STA, by = c("name", "year", "mark"), suffixes = c("_MOD", "_STA"))
mask <- !is.na(df_plot$los_STA)
df_plot <- df_plot[mask, ]

p1 <- ggplot(df_plot, aes(x = los_MOD, y = los_STA, color = mark)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", linewidth = 0.8) +
  scale_color_manual(
    values = c("NO" = "tomato", "OK" = "steelblue"),
    name   = "Compatible"
  ) +
  labs(
    title = NULL,
    x     = "SCD MODIS [days]",
    y     = "SCD Station [days]"
  ) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "none") +
  coord_equal(xlim = c(0, 370), ylim = c(0, 370))



# Importing station names to asses which ones have the longest recording streak
fname <- "../MODIS_series/Dataset/modis_hydrological/usable_modis.dat"
files <- list.files(path = "../MODIS_series/Dataset/modis_hydrological", full.names = TRUE)
station_names <- sub("../MODIS_series/Dataset/modis_hydrological/", "", files)


# Cycle on station names
truth_mat <- array(0, dim = c(4))
for(name in station_names){
  
  # Importing MODIS and station snow cover series
  fname_MOD <- paste0("../MODIS_series/Dataset/modis_hydrological/", name)
  df_MOD <- read.table(fname_MOD, header = FALSE)
  
  fname_STA <- paste0("../STATION_series/Dataset/", name)
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


# Plotting procedure
cm_df <- data.frame(
  MOD = factor(c("Yes",    "Yes",      "No", "No"), levels = c("Yes", "No")),
  STA = factor(c("Yes",    "No",   "Yes",    "No"), levels = c("Yes", "No")),
  val = truth_mat
)

total <- sum(truth_mat)
cm_df$pct <- paste0(cm_df$val, "\n(", round(100 * cm_df$val / total, 1), "%)")

p2 <- ggplot(cm_df, aes(x = STA, y = MOD, fill = val)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = pct), size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#d6eaf8", high = "#1a5276") +
  scale_y_discrete(limits = c("No", "Yes")) +  # Snow in alto
  labs(
    title = NULL,
    x     = "Station",
    y     = "MODIS"
  ) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "none",
        panel.grid = element_blank())


p1 <- p1 +
  theme(plot.margin = margin(t = 5, r = 25, b = 5, l = 5))

p2 <- p2 +
  theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 25))

(p1 | p2) +
  plot_annotation(tag_levels = "A")