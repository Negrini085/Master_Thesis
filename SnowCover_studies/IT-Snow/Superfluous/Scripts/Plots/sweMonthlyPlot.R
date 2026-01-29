# The main goal of this script is to produce a plot containing all monthly SWE 
# maps for a given year. I would like to have a unique and common colorbar, in 
# order to enable the reader to compare maps together
rm(list = ls()); gc()

library(sf)
library(ncdf4)
library(ggplot2)
library(rnaturalearth)
library(grid)
library(gtable)
library(gridExtra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")

# Output folder
out_dir <- "Figures/MonthlyMaps"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Function to extract legend from ggplot
getLegend <- function(p) {
  g <- ggplotGrob(p)
  gtable_filter(g, "guide-box")
}

years <- 2015:2025

# Background and titles (define once)
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")
titles <- c("September","October","November","December","January","February",
            "March","April","May","June","July","August")

lims <- c(0, 3250)

for (year in years) {
  
  fname <- paste0("Datas/MonthlyMaps", year, ".nc")
  if (!file.exists(fname)) {
    message("Skipping (file not found): ", fname)
    next
  }
  
  nc <- nc_open(fname)
  lat <- ncvar_get(nc, "lat")
  lon <- ncvar_get(nc, "lon")
  swe <- ncvar_get(nc, "SWE")
  nc_close(nc)
  
  # 0 -> NA (trasparente). Se 0 per te è dato reale, commenta questa riga.
  swe[swe == 0] <- NA
  
  # Grid dataframe (DON'T call it `grid`)
  df_grid <- expand.grid(lon = lon, lat = lat)
  
  # Build 12 plots
  plots <- vector("list", 12)
  for (i in 1:12) {
    
    df_grid$swe <- as.vector(swe[,,i])
    
    p <- ggplot() +
      geom_sf(data = europe, fill = "grey90", color = "black", linewidth = 0.2) +
      coord_sf(xlim = c(6, 19), ylim = c(36.5, 47.2), expand = FALSE) +
      geom_raster(data = df_grid, aes(x = lon, y = lat, fill = swe)) +
      scale_fill_viridis_c(
        option = "C",
        limits = lims,
        na.value = "transparent",
        oob = scales::squish
      ) +
      labs(title = titles[i], x = NULL, y = NULL, fill = "SWE (mm w.e.)") +
      theme_minimal(base_size = 10) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "right")
    
    plots[[i]] <- p
  }
  
  # One shared legend
  legend <- getLegend(plots[[1]])
  plots_noleg <- lapply(plots, function(p) p + theme(legend.position = "none"))
  
  gridP <- arrangeGrob(grobs = plots_noleg, nrow = 3, ncol = 4)
  title_grob <- textGrob(paste("Hydrological year", year),
                         gp = gpar(fontsize = 16, fontface = "bold"))
  
  final <- arrangeGrob(gridP, legend, ncol = 2, widths = c(12, 1.2), top = title_grob)
  
  # Save PNG
  file_out <- file.path(out_dir, paste0("MonthlyMaps_", year, ".png"))
  png(filename = file_out, width = 3800, height = 2100, res = 300, bg = "white")
  grid.newpage()
  grid.draw(final)
  dev.off()
  print("Ciao")
}
