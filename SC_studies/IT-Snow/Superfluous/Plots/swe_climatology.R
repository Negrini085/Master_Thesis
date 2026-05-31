# The main goal of this script is to create a climatology plot for SWE total 
# volume across an hydrological year. I would also like to show quantiles of the
# distribution.
rm(list = ls())
gc()

library(ggplot2)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/")


# Importing SWE series
dat <- read.table("Datas/swe_evolution.dat")
dat <- dat$V1

dur <- 365
years <- 2011:2021#2025


# Taking care of the 29th value. We will replace the 28th value with a mean across
# the 28th -> 29th period!
conta <- 0
swe_evolution <- numeric(0)
for(y in years){
  
  # Leap year
  if(y%%4 == 0){
    
    appo <- dat[(conta+1):(conta+dur+1)]
    
    # Taking care of 29th value
    swe_evolution <- c(swe_evolution, appo[1:180])
    swe_evolution <- c(swe_evolution, mean(appo[181:182]))
    swe_evolution <- c(swe_evolution, appo[183:(dur+1)])
    
    conta <- conta + dur + 1
  }
  
  # Normal year
  else{
    swe_evolution <- c(swe_evolution, dat[(conta+1):(conta+dur)])
    conta <- conta + dur
  }
}


# Creating dataframe as a first step of plotting procedure. Here I actually split years.
swe_mat <- matrix(swe_evolution, nrow = dur, ncol = length(years), byrow = FALSE)
colnames(swe_mat) <- years
swe_df <- data.frame(day = 1:dur, swe_mat, check.names = FALSE)


# Computing median and quantiles using apply function, so that we are all set to 
# create the desired plot
clim <- data.frame(
  day = 1:dur,
  q1  = apply(swe_mat, 1, quantile, probs = 0.25, na.rm = TRUE),
  med = apply(swe_mat, 1, quantile, probs = 0.50, na.rm = TRUE),
  q3  = apply(swe_mat, 1, quantile, probs = 0.75, na.rm = TRUE)
)

ggplot(clim, aes(x = day)) +
  geom_ribbon(aes(ymin = q1, ymax = q3), alpha = 0.4, color = "orange", fill = "orange") +
  geom_line(aes(y = med), linewidth = 1, color = "blue") +
  labs(title = "SWE Volume evolution", x = "Days (from 1st of September)", y = "SWE [Gm^3]") +
  theme_minimal()