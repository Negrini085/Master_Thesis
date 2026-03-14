# The main goal of this script is to plot LOS and CSC values for the stations that
# have been found to produce at least one outlier.
rm(list = ls())
gc()

library(ggplot2)
library(patchwork)

fname_los_MOD <- "../MODIS_series/Datas/compatible/los.dat"
fname_los_STA <- "../STATION_series/Datas/los_compatible.dat"
fname_csc_STA <- "../STATION_series/Datas/longest_periods_sc_compatible.dat"
fname_csc_MOD <- "../MODIS_series/Datas/compatible/longest_periods_sc_correct.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/FindBug/")


# Importing and merging LOS
df_los_MOD <- read.table(fname_los_MOD, header = FALSE)
df_los_STA <- read.table(fname_los_STA, header = FALSE)

df_los <- merge(
  data.frame(name = df_los_MOD$V1, year = as.numeric(df_los_MOD$V2), los_MOD = as.numeric(df_los_MOD$V3)),
  data.frame(name = df_los_STA$V1, year = as.numeric(df_los_STA$V2), los_STA = as.numeric(df_los_STA$V3)),
  by = c("name", "year")
)


# Importing and merging CSC
df_csc_MOD <- read.table(fname_csc_MOD, header = FALSE)
df_csc_STA <- read.table(fname_csc_STA, header = FALSE)

df_csc <- merge(
  data.frame(name = df_csc_MOD$V1, year = as.numeric(df_csc_MOD$V2), csc_MOD = as.numeric(df_csc_MOD$V3)),
  data.frame(name = df_csc_STA$V1, year = as.numeric(df_csc_STA$V2), csc_STA = as.numeric(df_csc_STA$V3)),
  by = c("name", "year")
)


# Importing bug list
df <- read.table("Datas/huge_outliers.dat", header = FALSE)
station_names <- unique(df$V1)

for(name in station_names){
  
  # Selecting datas
  mask <- df_los$name == name
  appo_los <- df_los[mask, ]
  
  mask <- df_csc$name == name
  appo_csc <- df_csc[mask, ]
  
  
  
  # Plotting procedure
  scatter_style <- list(
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 0.8),
    geom_point(alpha = 0.4, color = "steelblue", size = 2),
    theme_minimal(base_size = 13),
    coord_equal(),
    xlim(0, NA),
    ylim(0, NA)
  )
  
  p_csc <- ggplot(appo_csc, aes(x = csc_MOD, y = csc_STA)) +
    scatter_style +
    labs(
      title = NULL,
      x     = "CSC MODIS [days]",
      y     = "CSC Station [days]"
    )
  
  p_los <- ggplot(appo_los, aes(x = los_MOD, y = los_STA)) +
    scatter_style +
    labs(
      title = NULL,
      x     = "LOS MODIS [days]",
      y     = "LOS Station [days]"
    )
  
  combined <- p_csc + p_los +
    plot_annotation(title = name) 
  
  fname_out <- paste0("Images/outliers/", name, ".png")
  ggsave(fname_out, plot = combined, width = 15, height = 5, dpi = 150)
}