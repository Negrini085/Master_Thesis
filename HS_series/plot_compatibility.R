# The main goal of this script is to find out whether a bias of faulty stations
# exists or not. To find out, I will make a plot similar to the previous, but now
# the color code won't be based on station elevation, but on compatibility between 
# my dem elevation and the reported one. I've set a threshold of 15 meters, and
# that's enough to take out almost half of the stations.
rm(list = ls())
gc()

library(sf)
library(ggplot2)
library(patchwork)
library(rnaturalearth)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/")



# Plot parameters
size_title        <- 16
size_subtitle     <- 12
size_legend_title <- 15
size_legend_text  <- 15
size_axis_title   <- 20
size_axis_text    <- 15

theme_fonts <- theme(
  plot.title    = element_text(size = size_title, face = "bold"),
  plot.subtitle = element_text(size = size_subtitle),
  legend.title  = element_text(size = size_legend_title),
  legend.text   = element_text(size = size_legend_text),
  axis.title    = element_text(size = size_axis_title),
  axis.title.x  = element_text(margin = margin(t = 10)),
  axis.title.y  = element_text(margin = margin(r = 10)),
  axis.text     = element_text(size = size_axis_text)
)



# Reading original and corrected dataset
appo <- read.table("Original/STATION_check/ANAGRAFICA", header = TRUE, fill = TRUE)
df_original <- data.frame(lon = as.numeric(appo$lon), lat = as.numeric(appo$lat), status = appo$flag)
df_original <- na.omit(df_original)


appo <- read.table("Correct/STATION_check/Dataset/ANAGRAFICA", header = TRUE, fill = TRUE)
df_corrected <- data.frame(lon = as.numeric(appo$lon), lat = as.numeric(appo$lat), status = appo$flag)
df_corrected$status[df_corrected$status == "REV"] <- "OK"
df_corrected <- na.omit(df_corrected)


# Plotting procedure
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")

p1 <- ggplot() +
  geom_sf(data = europe, fill = "antiquewhite1", color = "grey70") +
  geom_point(data = df_original, aes(x = lon, y = lat, color = status), size = 1.5, alpha = 0.9) +
  scale_color_manual(values = c("OK" = "forestgreen", "NO" = "firebrick1"), name = "Compatibility") +
  coord_sf(xlim = c(3.5, 17), ylim = c(43, 49), expand = FALSE) +
  theme_minimal() +
  labs(title = NULL, x = "Longitude [°E]", y = "Latitude [°N]") +
  theme(panel.background = element_rect(fill = "aliceblue"), legend.position = "bottom")


p2 <- ggplot() +
  geom_sf(data = europe, fill = "antiquewhite1", color = "grey70") +
  geom_point(data = df_corrected, aes(x = lon, y = lat, color = status), size = 1.5, alpha = 0.9) +
  scale_color_manual(values = c("OK" = "forestgreen", "REV" = "forestgreen", "NO" = "firebrick1"), name = "Compatibility") +
  coord_sf(xlim = c(3.5, 17), ylim = c(43, 49), expand = FALSE) +
  theme_minimal() +
  labs(title = NULL, x = "Longitude [°E]", y = "") +
  theme(panel.background = element_rect(fill = "aliceblue"), legend.position = "bottom")


p <- (p1 | p2) &
  theme_fonts &
  plot_annotation(tag_levels = 'A') &
  theme(
    plot.tag = element_text(size = 15, face = "bold"),
    plot.tag.position = c(0.05, 0.95)
  )#& theme(legend.position = "none")
print(p)