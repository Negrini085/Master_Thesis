# The main goal of this script is to plot SWE seasonal maps for the whole investigated
# period. I would like to organize them as 3 rows x 5 columns. I will need to make two
# plots to cover the whole investigated period.
rm(list = ls())
gc()

library(terra)
library(geodata)
library(ggplot2)
library(tidyterra)
library(patchwork)
library(RColorBrewer)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")


# Importing SCD maps
years <- 1992:2006
fnames <- paste0("Datas/seasonal_swe_maps/swe_map_", years, ".tif")
seasonal_swe <- rast(fnames)



# Plotting procedure
names(seasonal_swe) <- as.character(years)
italy_border <- gadm(country = "ITA", level = 0, path = tempdir())

e <- ext(seasonal_swe)
my_breaks <- c(0, 20, 40, 60, 80, 100, 150)
my_palette <- rev(brewer.pal(7, "YlGnBu"))

ggplot() +
  geom_spatraster(data = seasonal_swe) +
  geom_spatvector(data = italy_border, fill = NA, color = "black", linewidth = 0.2) +
  facet_wrap(~lyr, nrow = 3, ncol = 5) +
  coord_sf(xlim = c(e$xmin, e$xmax), ylim = c(e$ymin, e$ymax), expand = FALSE) +
  scale_fill_gradientn(name = "SWE [mm w.e.]", colors = my_palette, limits = c(0, 1200), na.value = "transparent") +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15)
  ) +
  labs(title = "Seasonal SWE maps in Po Basin")