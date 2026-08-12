# The main goal of this script is to plot loss as a function of temperature. The
# degree-day factor approach is the one suggested by Magnusson et al.
rm(list = ls())
gc()

library(ggplot2)

fname <- "Results/test_temp_ddf_magnusson.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_calibration/")


# Importing loss values
df <- read.table(fname, header = TRUE)
tval <- as.numeric(df$temperature)
loss <- as.numeric(df$loss)


# Plotting options (we will do a better job when it's all finished)
ggplot(df, aes(x = tval, y = loss)) + 
  geom_line(color = "blue", size = 1.5) +
  labs(x = "Temperature [°C]", y = "Loss [a.u.]") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(margin = margin(t = 10), size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold")
  )