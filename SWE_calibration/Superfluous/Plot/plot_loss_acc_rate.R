# The main goal of this script is to plot loss evolution throughout the simulated 
# annealing procedure, altogether with acceptance rate and delta value.
rm(list = ls())
gc()

fname_loss <- "Runs/ddf_param/loss_evo_"
fname_accrate <- "Runs/ddf_param/accrate_evo_"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_calibration/")


# Importing loss and acceptance rate value