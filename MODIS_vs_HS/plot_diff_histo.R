# The main goal of this script is to plot normalized bias frequency in order to 
# check if some behaviour is present.
rm(list = ls())
gc()


library(ggplot2)

fname_diff <- "Results/only_over_five_HS_MODIS_diff.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/MODIS_vs_HS/")


# Importing diff values
df <- read.table(fname_diff, header = TRUE)
diff <- df$diff


# Plotting procedure
df <- data.frame(diff = diff)

ggplot(df, aes(x = diff)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of normalized bias",
       x = "Normalized bias",
       y = "Number")
