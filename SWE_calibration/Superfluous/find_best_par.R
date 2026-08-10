# The main goal of this script is to find the best parameters from SA. This will 
# enable the user to work in the best enviromental conditions possible.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_calibration/")
fname_loss <- "Runs/SimAnnealing.dat"
fname_par <- "Runs/moves.dat"


# Importing loss values and finding the minimum value
df <- read.table(fname_loss, header = FALSE)
loss <- as.numeric(df$V1)

min_loss <- min(loss, na.rm = TRUE)
min_idx <- which(loss == min_loss)[1]


# Finding parameters that led to this value
df <- read.table(fname_par, header = FALSE)
print(df[min_idx, ])