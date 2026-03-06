# The main goal of this script is to plot snow cover series for the selected stations
# scattered across the italian territory.
rm(list = ls())
gc()

library(ggplot2)
library(tidyr)
library(dplyr)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/MODIS_series/")


# Reading datas & elevations in order to later sort stations from the lowest to the highest
mat <- read.table("Datas/station_series_all.dat", header = TRUE, sep = "", check.names = FALSE)

appo <- as.matrix(read.table("Datas/start_end_years_filtered.dat", header = FALSE))
station_names <- appo[, 1]
rm(appo)
gc()

appo <- as.matrix(read.table("Datas/quality_check_filtered.dat", header = FALSE))
station_names_for_ele <- appo[, 1]
station_ele <- appo[, 4]
rm(appo)
gc()

elevations <- character(0)
for(name in station_names){
  mask <- station_names_for_ele == name
  elevations <- c(elevations, station_ele[mask])
}

rm(station_names_for_ele, station_ele)
gc()


# Sorting stations elevation-wise from lowest to highest
order_idx  <- order(elevations)
mat_sorted <- mat[, order_idx]
elev_sorted <- elevations[order_idx]


# Creating dates
n_days <- nrow(mat_sorted)
dates  <- seq(as.Date("2000-09-01"), by = "day", length.out = n_days)


# Plotting procedure
df <- mat_sorted |>
  mutate(date = dates) |>
  pivot_longer(-date, names_to = "station", values_to = "value") |>
  mutate(station = factor(station, levels = colnames(mat_sorted)))

ggplot(df, aes(x = date, y = station, fill = factor(value))) +
  geom_tile() +
  scale_fill_manual(
    values  = c("0" = "white", "1" = "#2166AC"),
    na.value = "white",
    name    = NULL,
    labels  = c("0" = "Snow free", "1" = "Snow covered"),
    breaks  = c("0", "1")
  ) +
  scale_x_date(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = NULL, y = "Stations (lowest to highest)", title = "MODIS SC series: Italian stations") +
  guides(fill = guide_legend(override.aes = list(color = "black", linewidth = 0.5))) +
  theme_minimal(base_size = 7) +
  theme(
    plot.title = element_text(size = 14),
    axis.text.y    = element_blank(),
    panel.grid     = element_blank(),
    legend.position = "bottom"
  )