# The main goal of this script is to compare snow cover series for the same station
# made from MODIS and station measured values. First thing first I need to find 
# the longest MODIS series that I have, and only then make a comparison plot
rm(list = ls())
gc()

fname <- "../MODIS_series/Datas/compatible/start_end_years_filtered.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Comparison/")


# Importing station names to asses which ones have the longest recording streak
df <- read.table(fname, header = FALSE)
station_names <- df$V1
end_yr <- as.numeric(df$V5)
start_yr <- as.numeric(df$V4)

good_st <- character(0)
for(i in seq_len(length(station_names))){
  if(start_yr[i] < 2000) start_yr[i] <- 2000
  if(end_yr[i] - start_yr[i] > 20){
    print(paste0("Recording series longer than 20 years: ", station_names[i]))
    good_st <- c(good_st, station_names[i])
  }
}


# Extracting randomly 10 stations from the ones which passed the previous check
set.seed(12)
selected_stations <- sample(good_st, size = 10)


# Cycle over stations in order to make a plot for each one
for(name in selected_stations){
  
  # Importing MODIS and STATION series. The main problem here is to make the two
  # series comparable, such as every day of the timeseries has datas in both df
  fname_MOD <- paste0("../MODIS_series/Datas/modis_hydrological/compatible/", name)
  df_MOD <- read.table(fname_MOD, header = FALSE)
  
  fname_STA <- paste0("../STATION_series/Datas/sc_series/", name)
  df_STA <- read.table(fname_STA, header = FALSE)
  
  
  # Deleting all station datas which don't intersect with MODIS time-window
  mask <- df_STA$V1 %in% df_MOD$V1
  df_STA <- df_STA[mask, ]
  
  
  # Plotting procedure
  # Building date vector starting from 01-09-(first hydro year - 1)
  start_date <- as.Date(paste0(df_STA$V1[1] - 1, "-09-01"))
  dates <- seq.Date(from = start_date, by = "day", length.out = nrow(df_MOD))
  
  df_plot <- data.frame(
    date = dates,
    MOD  = df_MOD$V2,
    STA  = df_STA$V2
  )
  
  p <- ggplot(df_plot, aes(x = date)) +
    geom_line(aes(y = MOD, color = "MODIS"),   alpha = 0.7) +
    geom_line(aes(y = STA, color = "Station"), alpha = 0.7) +
    scale_color_manual(values = c("MODIS" = "steelblue", "Station" = "tomato"), name = "") +
    scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") +
    labs(
      title = paste0(name),
      x     = "Date",
      y     = "Snow cover"
    ) +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(
    filename = paste0("Plots/sc_comparison_", name, ".png"),
    plot     = p,
    dpi      = 300,
    width    = 15,
    height   = 6
  )
  
}
