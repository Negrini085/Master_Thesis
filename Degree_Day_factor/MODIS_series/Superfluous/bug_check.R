# The main goal of this script is to determine whether those white lines on MODIS
# series plot are a plot bug or not
rm(list = ls())
gc()

library(ggplot2)
library(tidyr)
library(dplyr)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/MODIS_series/")


# Taking care of every station located above 2500 meters in order to later address
# whether the missing pixel is a plotting bug or not
appo <- as.matrix(read.table("Datas/start_end_years_filtered.dat", header = FALSE))
station_names <- appo[, 1]
rm(appo)
gc()

appo <- as.matrix(read.table("Datas/quality_check_filtered.dat", header = FALSE))
station_names_for_ele <- appo[, 1]
station_ele <- appo[, 4]
rm(appo)
gc()

names_check <- character(0)
for(name in station_names){
  mask <- station_names_for_ele == name
  if(station_ele[mask] > 2500 & station_ele[mask] < 3100) names_check <- c(names_check, name)
}


# First thing first I want to check whether the values I am plotting are the same 
# that we saved in the specific station files
df <- read.table(
  "Datas/station_series_all.dat", header = TRUE, sep = "\t", 
  na.strings = "NA", check.names = FALSE
)

bug_count <- 0
for(name in names_check){
  
  # Reading MODIS series from two different sources
  appo <- read.table(paste0("Datas/modis_series/", name))
  sc_series_single <- as.numeric(appo$V2)
  
  sc_series_all <- df[[name]]
  sc_series_all <- na.omit(sc_series_all)
  
  
  # Actual comparison
  comparison <- !(sc_series_all == sc_series_single)
  if(sum(comparison) != 0){
    print(paste0("Some datas are not the same Station: ", name))
    bug_count <- bug_count + 1
  }
  
  rm(appo)
  gc()
}

if(bug_count == 0) print(paste0("All ", length(names_check), " series are well handled!"))




# Now we are gonna consider a given hydrological year in order to determine for 
# every station the two longest periods of snow coverage
for(name in names_check){
  
  # Importing the hydrological series for MODIS product
  df <- read.table(paste0("Datas/modis_hydrological/", name))
  sc_series <- as.numeric(df$V2)
  year <- as.numeric(df$V1)
  
  # Selecting a given year. I will use 2019 because it seems to be the one easier
  # to spot
  sel_year <- 2019
  mask <- year == sel_year
  appo <- sc_series[mask]
  
  # Finding out which are the two longest periods of continuous snow coverage
  r <- rle(appo)
  sorted_sc <- sort(r$lengths[r$values == 1])
  
  # Printing values to screen
  print(paste0("Two longest periods of PSC during ", sel_year, ": ", sorted_sc[length(sorted_sc)], " ", sorted_sc[length(sorted_sc) - 1], " for station ", name))
}





#------------------------------------------------------------------------------#
#                          Plot only on 2018 - 2019                            #
#------------------------------------------------------------------------------#
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
mat_filtered <- mat[, colnames(mat) %in% names_check]
elevations_filtered <- elevations[station_names %in% names_check]

order_idx  <- order(elevations_filtered)
mat_sorted <- mat_filtered[, order_idx]


# Creating dates
n_days <- nrow(mat_sorted)
dates  <- seq(as.Date("2000-02-24"), by = "day", length.out = n_days)


# Plotting procedure
df <- mat_sorted |>
  mutate(date = dates) |>
  filter(date >= as.Date("2018-01-01") & date <= as.Date("2019-12-31")) |>  # <-- filtro date
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