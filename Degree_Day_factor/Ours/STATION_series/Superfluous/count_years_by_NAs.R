# The main goal of this script is to count how many hydrological years have at most
# a specific number of NAs. 
rm(list = ls())
gc()

library(ggplot2)

fname <- "Datas/usable_stations.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/STATION_series/")

# Function to compute how many stations have at most a given number of NAs
count_years_by_na <- function(station_names, threshold){
  count_na <- 0
  
  # Cycle over various station snow height series
  for(name in station_names){
    
    # Importing series
    df <- read.table(paste0("Datas/station_series/", name))
    years <- unique(df$V1)
    
    # Cycle on hydrological years to check whether they pass or not
    for(year in years){
      
      # Selecting specific hydrological year
      mask <- df$V1 == year
      appo <- df[mask, ]
      
      # Actual logic test
      mask <- is.na(as.numeric(appo$V2))
      if(sum(mask, na.rm = TRUE) <= threshold) count_na <- count_na + 1
    }
  }
  
  return(count_na)
}

# Importing station names and doing what I need to
na_numbers <- seq(0, 360, by = 10)
station_names <- as.matrix(read.table(fname)$V1)
appo <- sapply(na_numbers, function(th) count_years_by_na(station_names, th))


# Plotting procedure
df <- data.frame(
  na_threshold = na_numbers, 
  number = appo
)

ggplot(df, aes(x = na_threshold, y = number)) +
  geom_area(fill = "#C0DD97", alpha = 0.4) +
  geom_line(color = "#3B6D11", linewidth = 1) +
  geom_point(color = "#3B6D11", size = 2) +
  labs(
    x = "NA threshold",
    y = "Valid years",
    title = "Years with number of NA ≤ threshold"
  ) +
  theme_minimal()