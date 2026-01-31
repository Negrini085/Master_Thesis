# The main goal of this script is to plot SCD with respect to elevation
rm(list = ls())
gc()

library(ncdf4)
library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")
fname <- "Datas/scd_elevation_bands.dat"

# Loading mean SCD values for elevation band
scdE <- read.table("Datas/scd_elevation_bands.dat")
bands <- seq(50, 4000, 50)
scdE <- scdE$V1

# Creating dataframe for plotting
df <- data.frame(
  elevation = bands,
  scd = scdE
)

ggplot(df, aes(x = elevation, y = scd)) + 
  geom_point(color = "blue", size = 3) +
  labs(
    title = "Mean SCD vs Elevation",
    x = "Elevation (m)",
    y = "Mean SCD (days)"
  ) +
  theme_minimal()

