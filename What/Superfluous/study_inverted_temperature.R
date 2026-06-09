# The main goal of this script is to understand which stations have the biggest
# problems with inverted temperatures, so that Tmin is bigger than Tmax.
rm(list = ls())
gc()

library(ggplot2)

fname <- "Results/inverted_temperature.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Calibration/")


# Importing faulty instances
df <- read.table(fname, header = TRUE)
station_names <- df$name
year <- df$year
tmin <- df$tmin
tmax <- df$tmax


# Focus on stations
station_counts <- table(station_names)
worst_station <- names(which.max(station_counts))
n_faulty <- max(station_counts)

cat("A total of", length(unique(station_names)), "stations has at least one day where Tmin is bigger than Tmax!\n")
cat("The worst station is", worst_station, "with", n_faulty, "faulty instances!\n\n")

df_sta <- data.frame(name = names(station_counts), nfaulty = as.numeric(station_counts))
df_sta <- df_sta[order(df_sta$nfaulty, decreasing = TRUE), ]
print(df_sta)


# Focus on temperature difference magnitude
diff <- tmin - tmax
mask <- diff > 10
df_appo <- df[mask, ]
df_diff <- data.frame(diff = diff)

ggplot(df_diff, aes(x = diff)) +
  geom_histogram(bins = 50, color = "black", fill = "steelblue") +
  labs(
    title = "Distribution of Temperature Inversions",
    x = "Tmin - Tmax",
    y = "Number of Instances"
  ) +
  theme_minimal(base_size = 14)