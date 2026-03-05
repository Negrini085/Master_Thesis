# The main goal of this script is to quality check MODIS series. In particular, I
# want to asses whether there are some faulty days (should be -90.0). 
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/MODIS_series/")

appo <- as.matrix(read.table("Datas/start_end_years_filtered.dat", header = FALSE))
station_names <- appo[, 1]
rm(appo)
gc()


for(name in station_names[1]){
  
  # Creating filename and checking 
  fname <- paste0("Datas/station_series/", name)
  if(!file.exists(fname)) print(paste0("File does not exists: ", fname))
  
  # Importing dataset
  appo <- read.table(fname, header = FALSE)
  sc_series <- as.numeric(appo$V2)
  rm(appo)
  gc()
  
  # Looking for -90.0 (missing datas)
  mask <- sc_series == -90.0
  if(sum(mask) > 0) print(paste0("MODIS series with missing datas: ", name))
  
  # Looking for series with only one year (how I switch on hydrological?)
  if(length(sc_series) == 312 | length(sc_series) == 365 | length(sc_series) == 366){
    print(paste0("MODIS series with only one year: ", name))
  }
  
}