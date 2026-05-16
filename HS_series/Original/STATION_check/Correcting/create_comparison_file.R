# The main goal of this script is to create a file with old and new coordinates 
# in order to mail it to the big bosses
rm(list = ls())
gc()

library(terra)

fname_anag <- "../Dataset/ANAGRAFICA"
fname_dem <- "../DEM/DEM_stations_30.tif"
fname_corr <- "Correcting/correcting_dataset.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/STATION_check/")


# First thing first I import both data-frames
df_anag <- read.table(fname_anag, header = FALSE)
df_corr <- read.table(fname_corr, header = FALSE)


# Checking if names are corresponding or not
name_anag <- df_anag$V1
name_corr <- df_corr$V1
name_corr <- paste0("HSD_", name_corr)

mask <- name_anag == name_corr
if(sum(mask, na.rm = TRUE) == length(name_anag)) print("We good!")


# Creating dataframe with new elevation and coordinates
lon_rev <- array(0, dim = c(length(name_anag)))
lat_rev <- array(0, dim = c(length(name_anag)))
ele_rev <- array(0, dim = c(length(name_anag)))
flag <- array(NA_character_, dim = c(length(name_anag)))

mask <- df_corr$V5 == "MOD"
lon_rev[mask] <- as.numeric(df_corr$V2)[mask]
lat_rev[mask] <- as.numeric(df_corr$V3)[mask]
ele_rev[mask] <- as.numeric(df_corr$V4)[mask]
flag[mask] <- "REV"

mask <- df_corr$V5 != "MOD"
lon_rev[mask] <- as.numeric(df_anag$V2)[mask]
lat_rev[mask] <- as.numeric(df_anag$V3)[mask]
ele_rev[mask] <- as.numeric(df_anag$V4)[mask]

mask <- df_corr$V5 == "ok"
flag[mask] <- "OK"

mask <- df_corr$V5 != "MOD" & df_corr$V5 != "ok"
flag[mask] <- "NO"


# Creating dataset
df_merged <- data.frame(
  station_name = name_anag, 
  lon = df_anag$V2, 
  lat = df_anag$V3, 
  ele = df_anag$V4, 
  lon_rev = lon_rev, 
  lat_rev = lat_rev, 
  ele_rev = ele_rev, 
  flag = flag
)

sum(df_merged$flag == "REV", na.rm = TRUE)
sum(df_merged$flag == "OK", na.rm = TRUE)
sum(df_merged$flag == "NO", na.rm = TRUE)


# Checking compatibility with dem
diff_lim <- 12
dem <- rast(fname_dem)
dem_ele <- numeric(length = length(name_anag))
dem_lat <- numeric(length = length(name_anag))
dem_lon <- numeric(length = length(name_anag))
for(station in 1:length(name_anag)){
  # Selecting station to be analyzed
  point <- data.frame(x = df_merged$lon_rev[station], y = df_merged$lat_rev[station])
  
  # Raster point closest to station point
  ind <- cellFromXY(dem, point)
  coord_dem <- xyFromCell(dem, ind)
  dem_lon[station] <- coord_dem[1]
  dem_lat[station] <- coord_dem[2]
  
  # DEM grid point elevation
  dem_ele[station] <- as.numeric(dem[ind])
}

diff <- as.numeric(df_merged$ele_rev) - dem_ele


# Checking for faulty elevations on reviewed datas
mask <- df_merged$flag == "NO"
appo <- diff[mask]
print(sum(abs(appo) <= 12, na.rm = TRUE))

mask <- df_merged$flag == "REV"
appo <- diff[mask]
print(sum(abs(appo) <= 12, na.rm = TRUE))

mask <- df_merged$flag == "OK"
appo <- diff[mask]
print(sum(abs(appo) <= 12, na.rm = TRUE))


# Saving to file
write.table(df_merged, "Correcting/ANAGRAFICA_REV", row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")