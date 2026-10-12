# The main goal of this script is to make an aesthetically pleasing plot to show 
# position difference between recovered coordinates and the previously given.
rm(list = ls())
gc()

library(png)
library(grid)
library(ggplot2)
library(patchwork)
library(geosphere)

fname_corrected <- "Dataset/ANAGRAFICA"
fname_original <- "../../Original/STATION_check/ANAGRAFICA"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/Correct/STATION_check/")


# Importing metadata to compare modified locations
df_original <- read.table(fname_original, header = TRUE)
df_corrected <- read.table(fname_corrected, header = TRUE)


# Selecting only modified stations
mask <- df_corrected$flag == "REV"
df_corrected <- df_corrected[mask, ]

mask <- df_original$name %in% df_corrected$name
df_original <- df_original[mask, ]


# Extraction of useful datas
lon_ana <- as.numeric(df_original$lon)
lat_ana <- as.numeric(df_original$lat)

lon_correct <- as.numeric(df_corrected$lon)
lat_correct <- as.numeric(df_corrected$lat)

rm(df_corrected, df_original)
gc()


# Actual distance computation
bearing <- numeric(0)
distance <- numeric(0)
for(i in seq_len(length(lon_ana))){
  
  # Evaluating before and after conditions
  faulty <- c(lon_ana[i], lat_ana[i])
  correct <- c(lon_correct[i], lat_correct[i])
  
  # Evaluating distance
  appo <- distHaversine(faulty, correct)
  if(appo > 2000) next
  distance <- c(distance, appo)
  
  # Evaluating bearing
  appo <- bearing(faulty, correct)
  bearing <- c(bearing, appo)
}


# Plotting procedure
theta <- (90 - bearing) * pi / 180

x <- distance * cos(theta)
y <- distance * sin(theta)
df_plot <- data.frame(x = x, y = y, distance = distance)

r_breaks <- pretty(distance, n = 4)
circles <- do.call(rbind, lapply(r_breaks, function(r) {
  ang <- seq(0, 2*pi, length.out = 200)
  data.frame(x = r*cos(ang), y = r*sin(ang), r = r)
}))

angle_breaks <- seq(0, 315, by = 45)
labels_dir   <- c("N", "NE", "E", "SE", "S", "SO", "O", "NO")
r_max        <- max(r_breaks)

df_angles <- data.frame(
  xend  = r_max * cos((90 - angle_breaks) * pi / 180),
  yend  = r_max * sin((90 - angle_breaks) * pi / 180),
  xlab  = (r_max * 1.1) * cos((90 - angle_breaks) * pi / 180),
  ylab  = (r_max * 1.1) * sin((90 - angle_breaks) * pi / 180),
  label = labels_dir
)

p2 <- ggplot(df_plot, aes(x = x, y = y)) +
  geom_path(data = circles, aes(group = r), color = "grey80", linewidth = 0.3) +
  geom_segment(data = df_angles,
               aes(x = 0, y = 0, xend = xend, yend = yend),
               color = "grey80", linewidth = 0.3) +
  geom_text(data = df_angles, aes(x = xlab, y = ylab, label = label),
            size = 5, color = "grey50") +
  geom_text(data = data.frame(r = r_breaks),
            aes(x = r, y = 0, label = paste0(round(r), "m")),
            size = 5, color = "grey60", vjust = -0.5) +
  geom_segment(aes(x = 0, y = 0, xend = x, yend = y), alpha = 0.3, linewidth = 0.4) +
  geom_point(aes(color = distance), size = 3) +
  annotate("point", x = 0, y = 0, color = "red", size = 4, shape = 3) +
  scale_color_viridis_c(name = "Distance \n [m]") +
  coord_equal() +
  labs(title = NULL, x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text = element_blank(), 
    panel.grid = element_blank(), 
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20)
  )


img <- readPNG("Images/looking_for_station.png")
p1 <- wrap_elements(full = rasterGrob(img))
p_final <- p1 + p2 +
  plot_annotation(tag_levels = "a")
p_final