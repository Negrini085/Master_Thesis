# The main goal of this script is to plot all continuous snow cover periods I 
# have previously determined, in order to check whether their length depends on 
# elevation or not.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/MODIS_series/")


# Importing continuous snow coverage periods and station elevation
mat <- read.table("Datas/longest_periods_sc_correct.dat", header = TRUE, sep = "", check.names = FALSE)
sc_names <- mat[[1]]
sc_periods <- as.numeric(mat[[3]])

appo <- as.matrix(read.table("Datas/quality_check_filtered.dat", header = FALSE))
station_names_for_ele <- appo[, 1]
station_ele <- appo[, 4]
rm(appo)
gc()


# Creating a vector of elevations
ele_plot <- numeric(0)
for(name in sc_names){
  mask <- station_names_for_ele == name
  ele_plot <- c(ele_plot, station_ele[mask])
}


# Plotting procedure
df_plot <- data.frame(elevation = as.numeric(ele_plot), duration = sc_periods)

ggplot(df_plot, aes(x = elevation, y = duration)) +
  geom_point(alpha = 0.4, color = "#2166AC") +
  labs(x = "Elevation [m]", y = "Longest SC period [days]",
       title = "Continuous SC duration vs elevation: all datas") +
  theme_minimal()