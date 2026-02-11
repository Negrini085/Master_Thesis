# The main goal of this script is to produce a plot containing all SCD maps 
# for the period under investigation.
rm(list = ls())
gc() 

library(sf) 
library(ncdf4)
library(terra)
library(ggplot2) 
library(rnaturalearth) 

library(grid) 
library(gtable) 
library(gridExtra) 

get_legend <- function(p){ 
  g <- ggplotGrob(p) 
  gtable_filter(g, "guide-box") 
}

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD/Comparison")
fname <- paste0("Datas/difference_maps.nc")

nc <- nc_open(fname)
lat <- ncvar_get(nc, "lat") 
lon <- ncvar_get(nc, "lon") 
scd <- ncvar_get(nc, "Diff") 
nc_close(nc)

demR <- rast("Datas/DEM_Italy.tif")
dem <- as.matrix(demR, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)


# Plotting procedure
grid_df <- expand.grid(lon = lon, lat = lat)
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf") 
titles <- c(
  "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018",
  "2019", "2020", "2021", "2022", "2023", "2024", "2025"
)


lims <- c(min(scd), max(scd))

plots <- vector("list", length(titles)) 
for (i in 1:length(titles)) {
  appo_scd <- scd[,,i]
  appo_scd[is.na(dem)] <- NA
  grid_df$scd <- as.vector(appo_scd)
  
  p <- ggplot() +
    geom_sf(data = europe, fill = "grey90", color = "black", linewidth = 0.2) +
    coord_sf(xlim = c(6, 19), ylim = c(36.5, 47.2), expand = FALSE) +
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
  "Difference between methods: from rho - from swe",
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