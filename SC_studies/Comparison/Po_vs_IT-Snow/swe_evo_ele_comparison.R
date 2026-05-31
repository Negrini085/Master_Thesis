# The main goal of this script is to make a comparison plot between IT-Snow 
# and Po water-jade dataset. We will have to focus on the time window spanning 
# from September 1st of 2010 to August 31st of 2021. This particular script will
# deal with elevation bands.
rm(list = ls())
gc()

library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Comparison/Po_vs_IT-Snow")

# Importing data to be compared
appo_po <- as.matrix(read.table("Datas/swe_Po_tot_evo_ele.dat"))
appo_it <- as.matrix(read.table("Datas/swe_IT-Snow_evo_ele.dat"))

# Period of observations
years <- 2011:2021

# We have to select the correct time-span for IT-Snow dataset.
swe_it <- appo_it[1:4018, ]

# We have to select the correct time-span for Po Basin dataset. Considering that
# one of the two datasets contains only 272 days per year (or 273 days on a leap
# year) we will also have to add NA for the 93 days spanning from July 2nd to
# October 2nd
dur <- 272
start <- 5173
conta <- start
swe_po <- array(NA_real_, dim = c(32, 6))
for(y in years){

  # Selecting correct datas for a given year
  if(y%%4 == 0){
    swe_po <- rbind(swe_po, appo_po[(conta+1):(conta+dur+1), ])
    conta <- conta + dur + 1
  }
  else{
    swe_po <- rbind(swe_po, appo_po[(conta+1):(conta+dur), ])
    conta <- conta + dur
  }

  # Adding not assigned period just to make sure that we are comparing right
  if(y != 2021){
    swe_po <- rbind(swe_po, array(NA_real_, dim = c(93, 6)))
  }
  else{
    swe_po <- rbind(swe_po, array(NA_real_, dim = c(61, 6)))
  }
}



n <- min(nrow(swe_it), nrow(swe_po))
swe_it <- swe_it[1:n, ]
swe_po <- swe_po[1:n, ]

dates <- seq(as.Date("2010-09-01"), as.Date("2021-08-31"), by = "day")
dates <- dates[1:n]

band_labels <- c("0–500 m", "500–1000 m", "1000–1500 m", "1500–2000 m", "2000–2500 m", "Above 2500 m")

df_it <- data.frame(
  date = rep(dates, times = 6),
  band = rep(band_labels, each = length(dates)),
  dataset = "IT-SNOW",
  value = as.vector(swe_it)
)

df_po <- data.frame(
  date = rep(dates, times = 6),
  band = rep(band_labels, each = length(dates)),
  dataset = "WaterJade",
  value = as.vector(swe_po)
)

df <- rbind(df_it, df_po)
df$band <- factor(df$band, levels = band_labels)

ggplot(df, aes(x = date, y = value, color = dataset)) +
  geom_line(linewidth = 0.35) +
  facet_wrap(~ band, nrow = 3, ncol = 2, scales = "free_y") +
  labs(x = "Date", y = "SWE Volume [Gm^3]", color = NULL) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )