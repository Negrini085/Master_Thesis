# Seasonal precipitation climatology over Italian hydrological portion of the Greater 
# Alpine Region (GAR).
rm(list = ls())
gc()

library(ncdf4)
library(ggplot2)

fname_winter <- "Results/winter_prec_clim_1991_2020.nc"
fname_spring <- "Results/spring_prec_clim_1991_2020.nc"
fname_summer <- "Results/summer_prec_clim_1991_2020.nc"
fname_autumn <- "Results/autumn_prec_clim_1991_2020.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_inputs/PCPD/")



# Importing seasonal maps
nc <- nc_open(fname_winter)
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
prec_winter <- ncvar_get(nc, "winter_precipitation")
nc_close(nc)

nc <- nc_open(fname_spring)
prec_spring <- ncvar_get(nc, "spring_precipitation")
nc_close(nc)

nc <- nc_open(fname_summer)
prec_summer <- ncvar_get(nc, "summer_precipitation")
nc_close(nc)

nc <- nc_open(fname_autumn)
prec_autumn <- ncvar_get(nc, "autumn_precipitation")
nc_close(nc)

seasons <- list(
  Winter = prec_winter, Spring = prec_spring, 
  Summer = prec_summer, Autumn = prec_autumn
  )

grid_ll <- expand.grid(lon = lon, lat = lat)
df_plot <- do.call(rbind, lapply(names(seasons), function(s) {
  data.frame(grid_ll, precipitation = as.vector(seasons[[s]]), season = s)
}))
df_plot$season <- factor(df_plot$season, levels = names(seasons))

df_lab <- data.frame(
  season = factor(names(seasons), levels = names(seasons)),
  label  = paste0(letters[1:4], ") ", names(seasons)),
  x = min(lon) + 0.3,
  y = max(lat) - 0.1
)

max_p <- 1000

p <- ggplot(df_plot, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = precipitation)) +
  geom_text(data = df_lab, aes(x = x, y = y, label = label),
            hjust = 0, vjust = 1, size = 7) +
  facet_wrap(~ season, ncol = 2, axes = "all") +
  coord_quickmap() +
  scale_fill_viridis_c(
    name = "Precipitation \n [mm]",
    limits = c(0, max_p),
    oob = scales::squish,
    na.value = "transparent",
    direction = -1
  ) +
  labs(x = "Longitude [°E]", y = "Latitude [°N]") +
  theme_minimal() +
  theme(
    strip.text        = element_blank(),
    panel.border      = element_rect(fill = NA, colour = "grey50"),
    panel.spacing     = unit(1, "cm"),
    axis.title.x      = element_text(size = 20, margin = margin(t = 15)),
    axis.title.y      = element_text(size = 20, margin = margin(r = 15)),
    axis.text         = element_text(size = 15),
    legend.title      = element_text(size = 20, margin = margin(b = 15)),
    legend.text       = element_text(size = 15),
    legend.key.width  = unit(1.5, "cm"),
    legend.key.height = unit(2, "cm")
  )

print(p)

# out_dir <- "Images/"
# if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
# ggsave(file.path(out_dir, "seasonal_prec_climatology.png"),
#        plot = p, width = 14, height = 11, dpi = 300, bg = "white")