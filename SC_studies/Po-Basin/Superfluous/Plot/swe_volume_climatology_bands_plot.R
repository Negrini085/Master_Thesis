# The main goal of this script is to create an elenation-based climatology for the 
# swe volume. In particular, I would like to plot the median SWE volume and the 
# region between the first and the third quantile for every elevation band.
rm(list = ls())
gc()


library(ggplot2)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")

# First thing we need to do is to make hydrological years comparable between 
# themself. What I will do is to make the 29th collapse on the previous day, 
# replacing the 28th value with the mean across the 2 day period
dat <- read.table("Datas/swe_evolution_elevation.dat")

years <- 1992:2021
dur <- 272
day <- 149            # Number of days to the 28th of February

# Taking care of the 29th value. We will replace the 28th value with a mean across
# the 28th -> 29th period!
conta <- 0
swe1_evolution <- numeric(0)
swe2_evolution <- numeric(0)
swe3_evolution <- numeric(0)
swe4_evolution <- numeric(0)
swe5_evolution <- numeric(0)
swe6_evolution <- numeric(0)
for(y in years){
  
  # Here we have to check if we are dealing with a leap year or not (we actually
  # have to apply the correction or not?)
  if(y%%4 == 0){
    
    appo1 <- dat$V1[(conta+1):(conta+dur+1)]
    appo2 <- dat$V2[(conta+1):(conta+dur+1)]
    appo3 <- dat$V3[(conta+1):(conta+dur+1)]
    appo4 <- dat$V4[(conta+1):(conta+dur+1)]
    appo5 <- dat$V5[(conta+1):(conta+dur+1)]
    appo6 <- dat$V6[(conta+1):(conta+dur+1)]
    
    
    # Taking care of 29th value. Here we have to apply the same procedure for 
    # every elevation band (so six time, considering that the last one contains
    # every pixel which sits higher than 2500 meters)
    swe1_evolution <- c(swe1_evolution, appo1[1:(day-1)])
    swe1_evolution <- c(swe1_evolution, mean(appo1[day:(day+1)]))
    swe1_evolution <- c(swe1_evolution, appo1[(day+2):length(appo1)])
    
    swe2_evolution <- c(swe2_evolution, appo2[1:(day-1)])
    swe2_evolution <- c(swe2_evolution, mean(appo2[day:(day+1)]))
    swe2_evolution <- c(swe2_evolution, appo2[(day+2):length(appo2)])
    
    swe3_evolution <- c(swe3_evolution, appo3[1:(day-1)])
    swe3_evolution <- c(swe3_evolution, mean(appo3[day:(day+1)]))
    swe3_evolution <- c(swe3_evolution, appo3[(day+2):length(appo3)])
    
    swe4_evolution <- c(swe4_evolution, appo4[1:(day-1)])
    swe4_evolution <- c(swe4_evolution, mean(appo4[day:(day+1)]))
    swe4_evolution <- c(swe4_evolution, appo4[(day+2):length(appo4)])
    
    swe5_evolution <- c(swe5_evolution, appo5[1:(day-1)])
    swe5_evolution <- c(swe5_evolution, mean(appo5[day:(day+1)]))
    swe5_evolution <- c(swe5_evolution, appo5[(day+2):length(appo5)])
    
    swe6_evolution <- c(swe6_evolution, appo6[1:(day-1)])
    swe6_evolution <- c(swe6_evolution, mean(appo6[day:(day+1)]))
    swe6_evolution <- c(swe6_evolution, appo6[(day+2):length(appo6)])
    
    conta <- conta + dur + 1
  }
  
  else{
    
    swe1_evolution <- c(swe1_evolution, dat$V1[(conta+1):(conta+dur)])
    swe2_evolution <- c(swe2_evolution, dat$V2[(conta+1):(conta+dur)])
    swe3_evolution <- c(swe3_evolution, dat$V3[(conta+1):(conta+dur)])
    swe4_evolution <- c(swe4_evolution, dat$V4[(conta+1):(conta+dur)])
    swe5_evolution <- c(swe5_evolution, dat$V5[(conta+1):(conta+dur)])
    swe6_evolution <- c(swe6_evolution, dat$V6[(conta+1):(conta+dur)])
    
    conta <- conta + dur
  }
}



# Next thing is evaluating quantiles and median for every elevation band
swe_mat <- matrix(swe6_evolution, nrow = dur, ncol = length(years), byrow = FALSE)
colnames(swe_mat) <- years

# Creating dataframe. "check.names = FALSE" means that column names stay
# untouched, just as we please.
swe_df <- data.frame(day = 1:dur, swe_mat, check.names = FALSE)


# Computing median and quantiles using apply function, so that we are all set to 
# create the desired plot
clim <- data.frame(
  day = 1:dur,
  q1  = apply(swe_mat, 1, quantile, probs = 0.25, na.rm = TRUE),
  med = apply(swe_mat, 1, quantile, probs = 0.50, na.rm = TRUE),
  q3  = apply(swe_mat, 1, quantile, probs = 0.75, na.rm = TRUE)
)


# Creating plot using ggplot
ggplot(clim, aes(x = day)) +
  geom_ribbon(aes(ymin = q1, ymax = q3), alpha = 0.4, color = "orange", fill = "orange") +
  geom_line(aes(y = med), linewidth = 1, color = "blue") +
  labs(title = "SWE Volume in Po Basin: over 2500 m", x = "Days (from 3rd of October)", y = "SWE [Gm^3]") +
  theme_minimal()