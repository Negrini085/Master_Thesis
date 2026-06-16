# The main goal of this script is to visually assess whether some trends are 
# visible or not in MODIS bias towards HS series.
rm(list = ls())
gc()

library(terra)
library(ggplot2)
library(patchwork)

fname_DEM <- "DEM/DEM_MODIS.tif"
fname_ita <- "Dataset/ANAGRAFICA_ITA"
fname_diff <- "Results/only_over_five_HS_MODIS_diff.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/MODIS_vs_HS/")



# Importing difference values and creating final dataframe
df_diff <- read.table(fname_diff, header = TRUE)
df_ita <- read.table(fname_ita, header = TRUE)

mask <- df_ita$name %in% df_diff$name
df_ita <- df_ita[mask, ]

if(any(df_ita$name != df_diff$name)) stop("Non compatible name disposition!")
df_final <- data.frame(name = df_ita$name, lon = as.numeric(df_ita$lon), lat = as.numeric(df_ita$lat), diff = as.numeric(df_diff$diff))



# Importing dem and evaluating aspect/slope
dem <- rast(fname_DEM)
slope  <- terrain(dem, v = "slope",  unit = "degrees")
aspect <- terrain(dem, v = "aspect", unit = "degrees")


# Data extraction
pts <- vect(df_final, geom = c("lon", "lat"), crs = "EPSG:4326")
pts <- project(pts, crs(dem))

df_final$elevation <- extract(dem,   pts)[, 2]
df_final$slope     <- extract(slope, pts)[, 2]
df_final$aspect    <- extract(aspect, pts)[, 2]



# Plotting procedure
theme_thesis <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(
      axis.title       = element_text(size = base_size + 1, face = "bold"),
      axis.text        = element_text(size = base_size - 1, color = "black"),
      panel.grid.major = element_line(color = "grey88", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color = "black", linewidth = 0.7),
      plot.margin      = margin(8, 12, 8, 8)
    )
}


p1 <- ggplot(df_final, aes(x = elevation, y = diff)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey40", linewidth = 0.6) +
  geom_point(alpha = 0.45, color = "#2166ac", size = 1.6) +
  labs(x = "Elevation (m a.s.l.)",
       y = "Normalized Bias") +
  theme_thesis()

p2 <- ggplot(df_final, aes(x = slope, y = diff)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey40", linewidth = 0.6) +
  geom_point(alpha = 0.45, color = "#2166ac", size = 1.6) +
  labs(x = expression(bold(Slope~~(degree))),
       y = "") +
  theme_thesis()


p3 <- ggplot(df_final, aes(x = aspect, y = diff)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey40", linewidth = 0.6) +
  geom_point(alpha = 0.45, color = "#2166ac", size = 1.6) +
  scale_x_continuous(breaks = seq(0, 360, 90),
                     labels = c("N", "E", "S", "W", "N")) +
  labs(x = "Aspect",
       y = "") +
  theme_thesis()

combined <- (p1 | p2 | p3) +
  plot_annotation(
    title   = "MODIS bias as a function of terrain characteristics",
    theme   = theme(
      plot.title   = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 9,  color = "grey50")
    )
  )

ggsave("Images/bias_vs_terrain.png",
       plot   = combined,
       width  = 14, height = 5,
       dpi    = 300,
       bg     = "white")