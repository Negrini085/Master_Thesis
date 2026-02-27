# The main goal of this script is to plot SWE total volume across the whole
# investigated period, in order to show its seasonal variability.
rm(list=ls())
gc()

library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")


# We first read the table. In order to have a correct plot, we need to take into
# account that there are 93 days which are not considered in the SWE GeoTif 
# for every year, considering that the study period for a single year starts on
# the third of October and ends on the first on July.
appo <- read.table("Datas/swe_evolution.dat")
appo <- appo$V1

years <- 1992:2021
dur <- 272
com <- 93

# Creating a whole new time series with NAs for those 93 days.
conta <- 0
swe_evolution <- NULL
for(y in years){
  if(y == 1992){
    swe_evolution <- appo[1:(dur+1)]
    conta <- conta + dur + 1
  }
  else if(y%%4 == 0){
    swe_evolution <- c(swe_evolution, appo[(conta+1):(conta+dur+1)])
    conta <- conta + dur + 1
  }
  else{
    swe_evolution <- c(swe_evolution, appo[(conta+1):(conta+dur)])
    conta <- conta + dur
  }
  
  fill <- rep(NA_real_, 93)
  swe_evolution <- c(swe_evolution, fill)
  
}


# Creating dataframe and plotting
df <- data.frame(
  day = 1:length(swe_evolution), 
  swe = swe_evolution
)

data0 <- as.Date("1991-10-03")
dates <- as.Date(paste0(seq(1991, 2021, by = 3), "-10-03"))
dates_ind <- as.numeric(dates - data0) + 1


# Plotting options (we will do a better job when it's all finished)
ggplot(df, aes(x = day, y = swe)) + 
  scale_x_continuous(
    breaks = dates_ind,
    labels = format(dates, "%d %b %Y"), 
    expand = c(5e-3, 5e-3)
  ) +
  geom_line(color = "blue", size = 1.5) +
  labs(title = "SWE evolution on Po Basin: 1992 to 2021", x = "Days", y = "SWE Gm^3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
