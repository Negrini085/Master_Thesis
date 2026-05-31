# The main goal of this script is to make a comparison plot between IT-Snow 
# and Po water-jade dataset. We will have to focus on the time window spanning 
# from September 1st of 2010 to August 31st of 2021.
rm(list = ls())
gc()

library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Comparison/Po_vs_IT-Snow")

# Importing data to be compared
appo_po <- read.table("Datas/swe_Po_tot_evo.dat")
appo_it <- read.table("Datas/swe_IT-Snow_evo.dat")

# Period of observations
years <- 2011:2021

# We have to select the correct time-span for IT-Snow dataset. 
swe_it <- appo_it$V1[1:4018]

# We have to select the correct time-span for Po Basin dataset. Considering that 
# one of the two datasets contains only 272 days per year (or 273 days on a leap 
# year) we will also have to add NA for the 93 days spanning from July 2nd to 
# October 2nd
dur <- 272
start <- 5173
conta <- start
swe_po <- array(NA_real_, dim = c(32))
for(y in years){
  
  # Selecting correct datas for a given year
  if(y%%4 == 0){
    swe_po <- c(swe_po, appo_po$V1[(conta+1):(conta+dur+1)])
    conta <- conta + dur + 1
  }
  else{
    swe_po <- c(swe_po, appo_po$V1[(conta+1):(conta+dur)])
    conta <- conta + dur
  }
  
  # Adding not assigned period just to make sure that we are comparing right
  if(y != 2021){
    swe_po <- c(swe_po, array(NA_real_, dim = c(93)))
  }
  else{
    swe_po <- c(swe_po, array(NA_real_, dim = c(61)))
  }
}


# Plotting procedure
df_long <- data.frame(
  day = rep(1:length(swe_po), times = 2),
  swe = c(swe_po, swe_it),
  dataset = factor(rep(c("Po Basin", "IT-Snow"), each = length(swe_po)),
                   levels = c("Po Basin", "IT-Snow"))
)

data0 <- as.Date("2010-09-01")
dates <- as.Date(paste0(2010:2025, "-09-01"))
dates_ind <- as.numeric(dates - data0) + 1

ggplot(df_long, aes(x = day, y = swe, color = dataset)) +
  scale_x_continuous(
    breaks = dates_ind,
    labels = format(dates, "%d %b %Y"),
    expand = c(5e-3, 5e-3)
  ) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c("Po Basin" = "blue", "IT-Snow" = "orange")) +
  labs(
    title = "Comparison between IT-Snow and total Po-Basin: 2011 to 2021",
    x = "Days",
    y = "SWE (Gm^3)",
    color = "Dataset"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
