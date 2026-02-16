# The main goal of this script is to find out whether a bias of faulty stations
# exists or not. To find out, I will make a plot similar to the previous, but now
# the color code won't be based on station elevation, but on compatibility between 
# my dem elevation and the reported one. I've set a threshold of 15 meters, and
# that's enough to take out almost half of the stations.
rm(list = ls())
gc()

library(sf)
library(ggplot2)
library(rnaturalearth)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor")



# Reading dataset with dem elevation and station one
appo <- read.table("Datas/check_brunetti.dat", header = TRUE, fill = TRUE)
diff <- as.numeric(appo[[4]]) - as.numeric(appo[[7]])
df_plot <- data.frame(lon = as.numeric(appo[, 2]), lat = as.numeric(appo[, 3]), status = ifelse(abs(diff) < 100, "OK", "NO"))
df_plot <- na.omit(df_plot)
df_plot$status <- factor(df_plot$status, levels = c("OK", "NO"))



# Plotting procedure
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")

ggplot() +
  geom_sf(data = europe, fill = "antiquewhite1", color = "grey70") +
  geom_point(data = df_plot, aes(x = lon, y = lat, color = status), size = 1.5, alpha = 0.9) +
  scale_color_manual(values = c("OK" = "forestgreen", "NO" = "firebrick1"), name = "Compatibility") +
  coord_sf(xlim = c(3.5, 17), ylim = c(43, 49), expand = FALSE) +
  theme_minimal() +
  labs(title = "AWS Station reliability analysis: Brunetti analysis", x = "Longitude", y = "Latitude") +
  theme(panel.background = element_rect(fill = "aliceblue"), legend.position = "bottom")