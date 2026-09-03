# The main goal of this script is to find the best region in the parameter space, 
# which results in better model perfomance.
rm(list = ls())
gc()

fname <- "Results/uniform_calib.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_calibration/")


# Importing dataframe
df <- read.table(fname, header = TRUE)
df_sorted <- df[order(df$loss), ]
print(df_sorted)
