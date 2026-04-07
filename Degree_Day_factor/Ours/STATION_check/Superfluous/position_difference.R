# The main goal of this script is to make an aesthetically pleasing plot to show 
# position difference between recovered coordinates and the previously given.
rm(list = ls())
gc()

library(ggplot2)
library(geosphere)

fname_ana <- "../Dataset/ANAGRAFICA"
fname_correct <- "Correcting/correcting_dataset.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Ours/STATION_check/")


# Importing ANAGRAFICA and retrivied informations
df_ana <- read.table(fname_ana, header = FALSE)
df_correct <- read.table(fname_correct, header = FALSE)


# Selecting only modified stations
mask <- df_correct$V5 == "REV"
df_correct <- df_correct[mask, ]
df_correct$V1 <- paste0("HSD_", df_correct$V1)


# Selecting original datas for modified stations
df_ana <- df_ana[df_ana$V1 %in% df_correct$V1, ]
print(df_correct$V1)
print(df_ana$V1)

# Extraction of useful datas
lon_ana <- as.numeric(df_ana$V2)
lat_ana <- as.numeric(df_ana$V3)

lon_correct <- as.numeric(df_correct$V2)
lat_correct <- as.numeric(df_correct$V3)

rm(df_correct, df_ana)
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

ggplot(df_plot, aes(x = x, y = y)) +
  geom_path(data = circles, aes(group = r), color = "grey80", linewidth = 0.3) +
  geom_segment(data = df_angles,
               aes(x = 0, y = 0, xend = xend, yend = yend),
               color = "grey80", linewidth = 0.3) +
  geom_text(data = df_angles, aes(x = xlab, y = ylab, label = label),
            size = 3.5, color = "grey50") +
  geom_text(data = data.frame(r = r_breaks),
            aes(x = r, y = 0, label = paste0(round(r), "m")),
            size = 3, color = "grey60", vjust = -0.5) +
  geom_segment(aes(x = 0, y = 0, xend = x, yend = y), alpha = 0.3, linewidth = 0.4) +
  geom_point(aes(color = distance), size = 3) +
  annotate("point", x = 0, y = 0, color = "red", size = 4, shape = 3) +
  scale_color_viridis_c(name = "Distance [m]") +
  coord_equal() +
  labs(title = "Position difference", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text = element_blank(), panel.grid = element_blank())