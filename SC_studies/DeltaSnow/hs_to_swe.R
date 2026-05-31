# The main goal of this script is to convert an hs series (2011 Magnolta) to a 
# SWE one in order to be able to compare model results with our dataset
rm(list = ls())
gc()

library(ggplot2)
library(nixmass)
library(patchwork)

fname <- "Dataset/Magnolta_hydro_2011.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/DeltaSnow/")


# Importing HS series and dates
df <- read.table(fname, header = FALSE)
hs_series <- as.numeric(df$V1)

dates <- seq(as.Date("2010-09-01"), as.Date("2011-08-31"), by = "day")
hsdata <- data.frame(date = dates, hs = hs_series)


# Converting from HS to SWE
swe <- swe.delta.snow(hsdata)
hsdata$swe <- swe

top3 <- hsdata[order(hsdata$hs, decreasing = TRUE)[1:3], "date"]


# HS plot
p1 <- ggplot(hsdata, aes(x = date, y = hs * 100)) +
  geom_line(color = "grey40") +
  geom_area(alpha = 0.2, fill = "grey40") +
  geom_vline(xintercept = top3, linetype = "dashed", color = "red", linewidth = 0.5) +
  labs(title = "Hydrological year 2011",
       x = NULL,
       y = "Spessore nevoso [cm]") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

# SWE plot
p2 <- ggplot(hsdata, aes(x = date, y = swe)) +
  geom_line(color = "#2171b5") +
  geom_area(alpha = 0.2, fill = "#2171b5") +
  geom_vline(xintercept = top3, linetype = "dashed", color = "red", linewidth = 0.5) +
  labs(x = "Date",
       y = "SWE [mm w.e.]") +
  theme_minimal()

p1 / p2