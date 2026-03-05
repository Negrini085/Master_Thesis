# The main goal of this script is to filter stations based on difference with
# dem model.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor")
fname <- "Datas/faulty_vs_res/check_ele_30.dat"


# Importing file values
appo <- read.table(fname, header = TRUE, fill = TRUE)
mark <- appo[[9]]


# Masking on DEM compatibility or not
mask <- mark == "ok"
print(appo[[9]][mask])


# Building data-frame
results <- data.frame(
  name = appo[[1]][mask],
  lon = appo[[2]][mask],
  lat = appo[[3]][mask],
  ele = appo[[4]][mask],
  lon_DEM = appo[[5]][mask],
  lat_DEM = appo[[6]][mask],
  ele_DEM = appo[[7]][mask],
  diff = appo[[8]][mask],
  mark = appo[[9]][mask]
)

write.table(results, file = "Dataset/ok_stations_dem_30.dat", row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")