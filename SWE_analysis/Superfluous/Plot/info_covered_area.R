# The main goal of this script is to plot SWE distribution, while also having
# plotted Italian topography.
rm(list = ls())
gc()
  
library(sf)
library(ncdf4)
library(ggplot2)
library(patchwork)
library(rnaturalearth)
  
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")










#------------------------------------------------------------------------------#
#                                FIRST PLOT                                    #
#------------------------------------------------------------------------------#
f_name <- "Results/years_continuous_snow.nc"
nc <- nc_open(f_name)
lat_name = "lat"
lon_name = "lon"
swe_name = "years_continuous_snow"
lat <- ncvar_get(nc, lat_name)
lon <- ncvar_get(nc, lon_name)
swe <- ncvar_get(nc, swe_name)
swe[swe == 0] <- NA
nc_close(nc)
  
  
# Selecting the geographical background, in order to really understand where the
# snow coverage actually is
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")
  
# Plotting snapshot without Italian boundaries. We covered this topic in the
# previous file, so we will skip documenting lines.
grid <- expand.grid(lon = lon, lat = lat)
grid$swe <- as.vector(swe)
  
p1 <- ggplot() +
  geom_sf(data = europe, fill = "grey90", color = "black", inherit.aes = FALSE) +
  coord_sf(xlim = c(6.2, 14.5), ylim = c(43, 47)) +
  geom_raster(data = grid, aes(x = lon, y = lat, fill = swe)) +
  scale_fill_viridis_c(option = "C", na.value = "transparent") +
  labs(title = NULL, x = "Longitude [°E]", y = "Latitude [°N]", fill = "# years") +
  theme_minimal() + 
  theme(
    axis.title   = element_text(size = 20),
    axis.text    = element_text(size = 15),
    legend.title = element_text(size = 18),
    legend.text  = element_text(size = 14),
    legend.key.height = unit(1.5, "cm"),
    legend.key.width  = unit(1.0, "cm")
  )










#------------------------------------------------------------------------------#
#                               SECOND PLOT                                    #
#------------------------------------------------------------------------------#
df <- read.table("Results/csc_covered_area.dat", header = TRUE)
df <- data.frame(
  year = as.numeric(df$year),
  area = as.numeric(df$area)
)


p2 <- ggplot(df, aes(x = year, y = area)) +
  geom_line(
    linewidth = 1.2,
    colour = "#1F4E79",
    lineend = "round"
  ) +
  scale_x_continuous(
    breaks = seq(
      floor(min(df$year) / 5) * 5,
      ceiling(max(df$year) / 5) * 5,
      by = 5
    ),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.04))
  ) +
  labs(
    x = "Year",
    y = "Area [km²]"
    ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "sans", colour = "black"),
    axis.title = element_text(size = 20),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.text = element_text(size = 15, colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.line = element_line(linewidth = 0.6, colour = "black"),
    axis.ticks = element_line(linewidth = 0.5, colour = "black"),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major.x = element_line(colour = "grey80", linewidth = 0.35),
    panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )


p <- p1 / p2 +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(
    plot.tag = element_text(size = 15, face = "bold"),
    plot.tag.position = c(0, 1)
  )
print(p)