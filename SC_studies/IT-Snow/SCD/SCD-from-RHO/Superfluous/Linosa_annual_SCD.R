# The main goal of this script is to plot Linosa island and its SCD values across
# the investigated period. We expect to have values close to 100 for the first 
# year and then 365 (or 366 in case of a leap year) for the following ones.
rm(list = ls())
gc()

library(sf) 
library(ncdf4)
library(ggplot2) 
library(rnaturalearth) 

library(grid) 
library(gtable) 
library(gridExtra) 

get_legend <- function(p){ 
  g <- ggplotGrob(p) 
  gtable_filter(g, "guide-box") 
}

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD-from-RHO")
fname <- paste0("Datas/season_maps_SCD.nc")

nc <- nc_open(fname)
lat <- ncvar_get(nc, "lat") 
lon <- ncvar_get(nc, "lon") 
scd <- ncvar_get(nc, "SCD") 
nc_close(nc) 

# Selecting Linosa island (focus of our study --> full of faulty pixels)
lat <- lat[140 : 150]
lon <- lon[1260 : 1270]
scd <- scd[1260:1270, 140:150, ]
scd[scd == 0] <- NA

grid_df <- expand.grid(lon = lon, lat = lat)
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf") 
titles <- c(
  "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018",
  "2019", "2020", "2021", "2022", "2023", "2024", "2025"
)

scd[scd == 0] <- NA 
lims <- c(1, 365)

plots <- vector("list", length(titles)) 
for (i in 1:length(titles)) {
  grid_df$scd <- as.vector(scd[,,i])
  
  p <- ggplot() +
    geom_sf(data = europe, fill = "grey90", color = "black", linewidth = 0.2) +
    coord_sf(xlim = c(lon[1], lon[length(lon)]), ylim = c(lat[1], lat[length(lat)])) +
    scale_x_continuous(
      breaks = scales::pretty_breaks(n = 3),
      labels = function(x) paste0(sprintf("%.2f", x), "°E")
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    geom_raster(data = grid_df, aes(x = lon, y = lat, fill = scd)) +
    scale_fill_viridis_c( option = "C", limits = lims, na.value = "transparent", oob = scales::squish) +
    labs(title = titles[i], x = NULL, y = NULL, fill = "SCD") +
    theme_minimal(base_size = 10) +
    theme( plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "right" )
  
  plots[[i]] <- p 
} 

legend <- get_legend(plots[[1]]) 
plots_noleg <- lapply(plots, \(p) p + theme(legend.position = "none")) 
panel_grid <- arrangeGrob(grobs = plots_noleg, nrow = 3, ncol = 5) 

title_grob <- textGrob(
  "Linosa: faulty pixels across years",
  gp = gpar(fontsize = 16, fontface = "bold")
)

final <- arrangeGrob(
  panel_grid,
  legend,
  ncol = 2,
  widths = c(12, 1.2),
  top = title_grob
)

grid.newpage()
grid.draw(final)