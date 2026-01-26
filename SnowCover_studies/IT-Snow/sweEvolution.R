# The main goal of this script is to determine swe evolution of the snowpack. I will 
# try to make use of functions, in order to make code easier to read.

library(ncdf4)
library(ggplot2)

start_time <- Sys.time()
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")
old_warn <- getOption("warn")  
options(warn = -1)

# Time window
years <- 2011:2025
months <- c("09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08")

swe_evolution <- numeric(0)
for(y in years){
  for(i in 1:length(months)){
    
    # Logic conditions in order to create the correct file path
    if(i > 4){
      fname <- paste("y", toString(y), "/ITSNOW_SWE_", toString(y), months[i], ".nc", sep="")
    }
    else{
      fname <- paste("y", toString(y), "/ITSNOW_SWE_", toString(y-1), months[i], ".nc", sep="")
    }
    
    # The real deal, opening netCDF4 datas and evaluating total SWE
    nc <- nc_open(fname)
    time <- ncvar_get(nc, names(nc$var[1]))
    for(j in time){
      k <- j+1
      swe <- ncvar_get(nc, names(nc$var[2]), start = c(1, 1, k), count = c(-1, -1, 1))
      
      if(i > 4){
        print(paste("Calcolando lo SWE per il giorno ", toString(k), "/", months[i], "/", y, sep=""))  
      }
      else{
        print(paste("Calcolando lo SWE per il giorno ", toString(k), "/", months[i], "/", y-1, sep=""))
      }
      
      # SWE evaluation and storing
      total_swe <- sum(swe, na.rm = TRUE)
      swe_evolution <- c(swe_evolution, total_swe)
    }
    print(swe_evolution)
    nc_close(nc)
  }
}

df <- data.frame(
  day = 1:length(swe_evolution),
  swe = swe_evolution*0.00000025
)

data0 <- as.Date("2010-09-01")

# Plotting options (we will do a better job when it's all finished)
write.table(df$swe, file = "swe_evolution.dat", row.names = FALSE, col.names = FALSE)
ggplot(df, aes(x = df$day, y = df$swe)) + 
  geom_line(color = "blue", size = 1.5) +
  labs(title = "SWE evolution: 2011 to 2025", x = "Days", y = "SWE Gm^3") +
  theme_minimal()

options(warn = old_warn)
end_time <- Sys.time()
elapsed <- end_time - start_time
elapsed