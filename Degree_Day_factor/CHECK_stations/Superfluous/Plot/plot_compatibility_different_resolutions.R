# The main goal of this script is to plot AWS stations across the whole alpine
# arc. Every subplot refers to a specific DEM resolution
rm(list = ls())
gc()

library(sf)
library(ggplot2)
library(rnaturalearth)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor")

# Function to load and clear datas
load_and_clean <- function(file_path, label, diff_lim) {
  appo <- read.table(file_path, header = TRUE, fill = TRUE)
  mark <- appo[[9]]
  
  df <- data.frame(
    lon = as.numeric(appo[, 2]),
    lat = as.numeric(appo[, 3]),
    status = ifelse(mark == "ok", "OK", "NO"),
    dataset = label
  )
  return(na.omit(df))
}



# Loading different files
df1 <- load_and_clean("Datas/faulty_vs_res/check_ele_60.dat", "60 m", 24)
df2 <- load_and_clean("Datas/faulty_vs_res/check_ele_120.dat", "120 m", 48)
df3 <- load_and_clean("Datas/faulty_vs_res/check_ele_240.dat", "240 m", 96)
df4 <- load_and_clean("Datas/faulty_vs_res/check_ele_480.dat", "480 m", 192)



# Creating a big dataframe to later plot
df_total <- rbind(df1, df2, df3, df4)
df_total$dataset <- factor(df_total$dataset, levels = c("60 m", "120 m", "240 m", "480 m"))
df_total$status <- factor(df_total$status, levels = c("OK", "NO"))



# Plotting procedure
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")

ggplot() +
  geom_sf(data = europe, fill = "antiquewhite1", color = "grey70") +
  geom_point(data = df_total, aes(x = lon, y = lat, color = status), size = 0.8, alpha = 0.8) +
  scale_color_manual(values = c("OK" = "forestgreen", "NO" = "firebrick1"), name = "Compatibility") +
  coord_sf(xlim = c(3.5, 17), ylim = c(43, 49), expand = FALSE) +
  facet_wrap(~dataset, nrow = 2, ncol = 2) + 
  theme_minimal() +
  labs(title = "AWS station reliability analysis: different DEM resolutions",
       subtitle = "Threshold: 2/5 of pixel size",
       x = "Longitude", y = "Latitude") +
  theme(panel.background = element_rect(fill = "aliceblue"),
        legend.position = "bottom",
        strip.text = element_text(face = "bold"))