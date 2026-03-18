# The main goal of this script is to filter NDSI datas based on value and quality
# in order to later plot them. I want to actually check if there is any kind of
# relationship between those variables. I have to be really careful here, because 
# I have to deal with hydrological years
rm(list = ls())
gc()

library(ggplot2)

fname_NDSI <- "Datas/NDSI_raw.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Comparison/")
fname_STA <- "../STATION_series/Datas/station_series/HSD_IT_VDA_AO_GRESSONEY_SAINT_JEAN-BIELTSCHOCKE_3040"


# Importing raw NDSI and snow height datas
df_NDSI <- read.table(fname_NDSI, header = FALSE)
days_NDSI <- df_NDSI$V1
year_NDSI <- df_NDSI$V2
value_NDSI <- df_NDSI$V3
quality_NDSI <- df_NDSI$V4

df_STA <- read.table(fname_STA, header = FALSE)
year_STA <- as.numeric(df_STA$V1)
hs_STA <- as.numeric(df_STA$V2)


# Filtering station hs datas in order to match NDSI datas
years <- 2021:2023
start <- c(31, 1, 1)
end <- c(365, 365, 365)
correct_hs <- numeric(0)
for(i in seq_len(length(years))){
  
  # Actual data extraction
  appo <- hs_STA[year_STA == years[i]]
  appo <- appo[start[i]:end[i]]
  correct_hs <- c(correct_hs, appo)
}

print(correct_hs[93:100])
print(value_NDSI[93:100])

# Filtering over NDSI values
mask <- !is.na(value_NDSI)
value_NDSI <- value_NDSI[mask]
correct_hs <- correct_hs[mask]
quality_NDSI <- quality_NDSI[mask]

mask <- value_NDSI >= 0 & value_NDSI <= 100
value_NDSI <- value_NDSI[mask]
correct_hs <- correct_hs[mask]
quality_NDSI <- quality_NDSI[mask]

mask <- quality_NDSI <= 2
value_NDSI <- value_NDSI[mask]
correct_hs <- correct_hs[mask]
quality_NDSI <- quality_NDSI[mask]

mask <- !is.na(correct_hs)
value_NDSI <- value_NDSI[mask]
correct_hs <- correct_hs[mask]
quality_NDSI <- quality_NDSI[mask]


# Plotting procedure
df_plot <- data.frame(ndsi = value_NDSI*10^-2, hs = correct_hs)

ggplot(df_plot, aes(x = ndsi, y = hs)) +
  geom_point(alpha = 0.2, color = "steelblue", size = 2) +
  labs(
    title    = "Snow heigth vs ndsi",
    x        = "NDSI",
    y        = "HS"
  ) +
  scale_x_continuous(
    limits = c(0, 1.5),
    breaks = c(0, 1)
  ) +
  theme_minimal(base_size = 13)