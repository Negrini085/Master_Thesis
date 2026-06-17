# The main goal of this script is to visually check whether tree presence is a key
# factor to predict MODIS snow detection accuracy.
rm(list = ls())
gc()

library(terra)
library(ggplot2)

fname_tcd <- "DEM/TCD_MOD.tif"
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
tcd <- rast(fname_tcd)

pts <- vect(df_final, geom = c("lon", "lat"), crs = "EPSG:4326")
pts <- project(pts, crs(tcd))

df_final$tcd <- extract(tcd,   pts)[, 2]

mask <- df_final$tcd <= 100
df_final <- df_final[mask, ]

mask <- df_final$tcd < 25 & df_final$diff > 0.6
print(df_final[mask, ])
  

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


p1 <- ggplot(df_final, aes(x = tcd, y = diff)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey40", linewidth = 0.6) +
  geom_point(alpha = 0.45, color = "#2166ac", size = 1.6) +
  labs(x = "TCD",
       y = "Normalized Bias", 
       title = "Bias as a function of TCD") +
  theme_thesis()

ggsave("Images/bias_vs_tcd.png",
       plot   = p1,
       width  = 14, height = 5,
       dpi    = 300,
       bg     = "white")