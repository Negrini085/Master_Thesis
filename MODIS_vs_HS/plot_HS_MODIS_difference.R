# The main goal of this script is to compute how biased is MODIS according to 
# HS values measured across the italian territory
rm(list = ls())
gc()

library(sf)
library(ncdf4)
library(ggplot2)
library(rnaturalearth)

fname_ita <- "Dataset/ANAGRAFICA_ITA"
fname_diff <- "Results/HS_MODIS_diff.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/MODIS_vs_HS/")


# Importing ANAGRAFICA and differences
df_ita <- read.table(fname_ita, header = TRUE)
df_diff <- read.table(fname_diff, header = TRUE)

mask <- df_ita$name %in% df_diff$name
df_ita <-df_ita[mask, ]

if(any(df_diff$name != df_ita$name)) stop("Names are ordered in different ways!")


# Plotting procedure
df_coord <- data.frame(lon = as.numeric(df_ita$lon), lat = as.numeric(df_ita$lat), diff = as.numeric(df_diff$diff))
df_coord <- na.omit(df_coord)

europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")

ggplot() +
  geom_sf(data = europe, fill = "antiquewhite1", color = "grey70") +
  geom_point(data = df_coord, aes(x = lon, y = lat, color = diff), 
             size = 1.2, alpha = 0.8) +
  scale_color_viridis_c(option = "turbo", name = "Bias", limits = c(-1, 1)) +
  coord_sf(xlim = c(6, 14.2), ylim = c(43.8, 47.2), expand = FALSE) +
  theme_minimal() +
  labs(title = "MODIS vs HS comparison",
       x = "Longitude",
       y = "Latitude") +
  theme(panel.background = element_rect(fill = "aliceblue"))
