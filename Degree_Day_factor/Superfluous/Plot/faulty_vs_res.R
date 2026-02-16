# The main goal of this script is to create a plot showing the number of stations
# to discard as a function of dem resolution. I feel like there will be a significant
# trend that need to be understood.
rm(list = ls())
gc()

library(ggplot2)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor")

# Function to load datas and compute the number of stations to be discarded.
num_discarded <- function(file_path, diff_lim){
  
  # Loading data and computing difference
  appo <- read.table(file_path, header = TRUE, fill = TRUE)
  diff <- as.numeric(appo[[4]]) - as.numeric(appo[[7]])
  
  # Evaluating how many discarded there will be
  mask <- diff > diff_lim | diff < -diff_lim
  num <- length(diff[mask])
  
  return(num)
}



# Loading different files
res <- c(30, 60, 120, 240, 480)
diff_lims <- c(12, 24, 48, 96, 192)
num_to_discard <- array(0, dim = c(length(res)))
for(i in 1:length(res)){
  fname <- paste0("Datas/faulty_vs_res/check_ele_", res[i], ".dat")
  num_to_discard[i] <- num_discarded(fname, diff_lims[i])
}



# Plotting procedure, first dataframe and then plot
df <- data.frame(
  res = res, 
  dis = num_to_discard
)

ggplot(df, aes(x = res, y = dis)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "firebrick", size = 3) +
  geom_text(aes(label = dis), vjust = -1.2, size = 4) +
  scale_x_continuous(breaks = res) +
  ylim(0, max(df$dis) * 1.2) +
  theme_minimal() +
  labs(
    title = "Stations to be discarded as a function of DEM resolution",
    subtitle = "Threshold: 2/5 of pixel",
    x = "DEM resolution [m]",
    y = "Number of discarded"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )
