# The main goal of this script is to compare snow height in-situ measurements with
# snow cover evaluated via MODIS product.
rm(list = ls())
gc()

library(ggplot2)

fname_sc <- "../MODIS_series/Datas/modis_hydrological/compatible/HSD_IT_VDA_AO_GRESSONEY_SAINT_JEAN-BIELTSCHOCKE_3040"
fname_hs <- "../STATION_series/Datas/station_series/HSD_IT_VDA_AO_GRESSONEY_SAINT_JEAN-BIELTSCHOCKE_3040"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Comparison/")


# Importing snow height and snow cover datas
df <- read.table(fname_sc, header = FALSE)
year_sc <- as.numeric(df$V1)
values_sc <- as.numeric(df$V2)

df <- read.table(fname_hs, header = FALSE)
year_hs <- as.numeric(df$V1)
values_hs <- as.numeric(df$V2)


# Mask to omit some years and to omit NAs
delete_years <- 2002:2005
for(year in delete_years){
  mask <- year_sc != year
  values_sc <- values_sc[mask]
  
  mask <- year_hs != year
  values_hs <- values_hs[mask]
}

mask <- !is.na(values_hs)
values_sc <- values_sc[mask]
values_hs <- values_hs[mask]


# Plotting procedure
df <- data.frame(
  values_sc = values_sc,
  values_hs = values_hs
)

ggplot(df, aes(x = values_sc, y = values_hs)) +
  geom_point(alpha = 0.01, color = "steelblue", size = 2) +
  labs(
    title = "Snow height vs snow cover",
    x     = "SC (MODIS)",
    y     = "HS (station) [cm]"
  ) +
  scale_x_continuous(
    limits = c(0, 1.5),
    breaks = c(0, 1)
  ) +
  theme_minimal(base_size = 13)