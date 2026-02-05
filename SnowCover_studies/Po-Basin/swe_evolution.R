# The main goal of this script is to determine swe evolution of the snowpack. I will 
# try to make use of functions, in order to make code easier to read.

library(terra)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")

years <- 1992:2021
months <- c("10", "11", "12", "01", "02", "03", "04", "05", "06", "07")
dayMax <- c(31, 30, 31, 31, 28, 31, 30, 31, 30, 31)

print(length(months))
print(length(dayMax))

swe_evolution <- numeric(0)
for(y in years){
  
  if(y%%4 == 0){
    dayMax[5] <- 29
  }
  
  for(i in 1:length(months)){

    # Creating the correct file path
    if(i > 3){
      fname <- paste0("Dataset/", toString(y), "/SWE_", toString(y), "-", months[i], "-")
      print(paste0("Considering month  ", months[i], "/", y, " for SWE evaluation."))
    }
    else{
      fname <- paste0("Dataset/", toString(y), "/SWE_", toString(y-1), "-", months[i], "-")
      print(paste0("Considering month  ", months[i], "/", y-1, " for SWE evaluation."))
    }
    
    # Considering single day swe maps
    for(d in 1:dayMax[i]){
      if(d < 3 & i == 1){
        next
      }
      else if(d > 1 & i == 10){
        break
      }
      
      # Final name
      appo <- paste0(fname, sprintf("%02d", d), ".tif")
      
      # SWE volume computation
      swe_tif <- rast(appo)
      area <- cellSize(swe_tif, unit="m")
      total_swe <- global(area*swe_tif*10^-12, fun="sum", na.rm=TRUE)$sum[1]
      
      # Storing swe value
      swe_evolution <- c(swe_evolution, total_swe)
    }
  }
  
  dayMax[5] <- 28
}


# Saving SWE volume in a .dat file
df <- data.frame(len = 1:length(swe_evolution), swe = swe_evolution)
write.table(df$swe, file = "swe_evolution.dat", row.names = FALSE, col.names = FALSE)

# Plot
ggplot(df, aes(x = len, y = swe)) + 
  geom_line(color = "blue", size = 1.5) +
  labs(title = "SWE evolution: 2011 to 2025", x = "Days", y = "SWE Gm^3") +
  theme_minimal()