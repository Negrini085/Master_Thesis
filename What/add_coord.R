# The main goal of this script is to add coordinates, in order to let the user 
# see where the station is.
rm(list = ls())
gc()

fname <- "Results/negative_prec.dat"
fname_old <- "../HS_series/Original/STATION_check/ANAGRAFICA"
fname_new <- "../HS_series/Correct/STATION_check/Dataset/ANAGRAFICA"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/What/")


# Importing negative precipitations and new/old ANAGRAFICA
df_neg <- read.table(fname, header = TRUE)
df_new <- read.table(fname_new, header = TRUE)
df_old <- read.table(fname_old, header = TRUE)


names <- df_neg$name
appo_lon <- numeric(0)
appo_lat <- numeric(0)
appo_ele <- numeric(0)
for(name in names){
  if(name %in% df_new$name){
    mask <- df_new$name == name
    appo_lon <- c(appo_lon, as.numeric(df_new$lon[mask]))
    appo_lat <- c(appo_lat, as.numeric(df_new$lat[mask]))
    appo_ele <- c(appo_ele, as.numeric(df_new$ele[mask]))
  }
  else{
    mask <- df_old$name == name
    appo_lon <- c(appo_lon, as.numeric(df_old$lon[mask]))
    appo_lat <- c(appo_lat, as.numeric(df_old$lat[mask]))
    appo_ele <- c(appo_ele, as.numeric(df_old$ele[mask]))
  }
}

df_print <- data.frame(
  name = df_neg$name,
  lon = appo_lon,
  lat = appo_lat,
  ele = appo_ele,
  year = df_neg$year,
  month = df_neg$month,
  day = df_neg$day,
  value = df_neg$value
)

write.table(df_print, "negative_prec.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)