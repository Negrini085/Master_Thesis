# The main goal of this script is to find those years which have more than 50% 
# datas missing across the winter period. I will consider as winter the period
# stretching from the first of december till the 31st of march
rm(list = ls())
gc()

fname <- "Datas/usable_stations.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/STATION_series/")