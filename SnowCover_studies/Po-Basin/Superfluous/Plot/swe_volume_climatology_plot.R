# The main goal is to create a climatology for the total swe volume. In particular, 
# I would like to plot the median SWE volume and the region between the first and 
# the third quantile.
rm(list=ls())
gc()

library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")

# First thing we need to do is to make hydrological years comparable between 
# themself. What I will do is to make the 29th collapse on the previous day, 
# replacing the 28th value with the mean across the 2 day period
dat <- read.table("Datas/swe_evolution.dat")
dat <- dat$V1

years <- 1992:2021
dur <- 272
day <- 149            # Number of days to the 28th of February


# Taking care of the 29th value. We will replace the 28th value with a mean across
# the 28th -> 29th period!
conta <- 0
swe_evolution <- NULL
for(y in years){
  if(y == 1992){
    appo <- dat[1:(dur+1)]
    
    # Taking care of 29th value
    swe_evolution <- appo[1:147]
    swe_evolution <- c(swe_evolution, mean(appo[148:149]))
    swe_evolution <- c(swe_evolution, appo[150:(dur+1)])
    
    conta <- conta + dur + 1
  }
  
  else if(y%%4 == 0){
    
    appo <- dat[(conta+1):(conta+dur+1)]
    
    # Taking care of 29th value
    swe_evolution <- c(swe_evolution, appo[1:147])
    swe_evolution <- c(swe_evolution, mean(appo[148:149]))
    swe_evolution <- c(swe_evolution, appo[150:(dur+1)])
    
    conta <- conta + dur + 1
  }
  
  else{
    swe_evolution <- c(swe_evolution, dat[(conta+1):(conta+dur)])
    conta <- conta + dur
  }
}

# Here I create a matrix starting from swe_evolution array. I want this container
# to have 30 columns, one for every hydrological year we are investigating. A key
# command that is given here is "byrow = FALSE" because that means that the matrix
# is filled column by column (actually one for hydrological year)
swe_mat <- matrix(swe_evolution, nrow = dur, ncol = length(years), byrow = FALSE)
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
  labs(title = "SWE Volume in Po Basin: Oct to July", x = "Days (from 3rd of October)", y = "SWE [Gm^3]") +
  theme_minimal()