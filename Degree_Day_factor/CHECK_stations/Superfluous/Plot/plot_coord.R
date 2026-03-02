# The main goal of this script is to plot AWS stations across the whole alpine arc
# before filtering for good datas.
rm(list = ls())
gc()

library(sf)
library(ncdf4)
library(ggplot2)
library(rnaturalearth)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor")



appo <- read.table("Dataset/coord.dat", header = FALSE)
df_coord <- as.data.frame(lapply(appo[, 2:4], as.numeric))
colnames(df_coord) <- c("lon", "lat", "elev")
df_coord <- na.omit(df_coord)

europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")

ggplot() +
  geom_sf(data = europe, fill = "antiquewhite1", color = "grey70") +
  geom_point(data = df_coord, aes(x = lon, y = lat, color = elev), 
             size = 1.2, alpha = 0.8) +
  scale_color_viridis_c(option = "turbo", name = "Declared elevation (m a.s.l.)") +
  coord_sf(xlim = c(3.5, 17), ylim = c(43, 49), expand = FALSE) +
  theme_minimal() +
  labs(title = "AWS Stations across alpine arc",
       x = "Longitude",
       y = "Latitude") +
  theme(panel.background = element_rect(fill = "aliceblue"))
