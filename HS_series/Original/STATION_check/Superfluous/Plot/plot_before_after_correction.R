# The main goal of this script is to show how much the picture changed thanks to 
# the effort e made to retrieve actual station positions.
rm(list = ls())
gc()

library(patchwork)

fname_before <- "Datas/check_ele.dat"
fname_after <- "Correcting/ANAGRAFICA_REV"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Ours/STATION_check/")

# Function to make plots
make_plot <- function(df, title, subtitle) {
  europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")
  
  ggplot() +
    geom_sf(data = europe, fill = "antiquewhite1", color = "grey70") +
    geom_point(data = df, aes(x = lon, y = lat, color = status), size = 1.5, alpha = 0.9) +
    scale_color_manual(values = c("OK" = "forestgreen", "NO" = "firebrick1"), name = "Compatibility") +
    coord_sf(xlim = c(3.5, 17), ylim = c(43, 49), expand = FALSE) +
    theme_minimal() +
    labs(title = title, subtitle = subtitle, x = "Longitude", y = "Latitude") +
    theme(
      panel.background = element_rect(fill = "aliceblue"),
      legend.position = "bottom"
    )
}



# Importing both datasets
df_before <- read.table(fname_before, header = TRUE)
df_before <- data.frame(lon = as.numeric(df_before$lon), lat = as.numeric(df_before$lat), status = df_before$mark)

df_after <- read.table(fname_after, header = TRUE)
mask <- df_after$flag == "REV"
df_after$flag[mask] <- "OK"
df_after <- data.frame(lon = as.numeric(df_after$lon), lat = as.numeric(df_after$lat), status = df_after$flag)



# Plotting procedure
p1 <- make_plot(df_before, title = "Before correction", subtitle = NULL)
p2 <- make_plot(df_after, title = "After correction", subtitle = NULL)

combined <- p1 * p2 +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(combined)