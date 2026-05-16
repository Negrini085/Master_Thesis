# The main goal of this script is to compare modis series los with modis actual 
# values. To be fair, this isn't a perfectly fine comparison because MODIS uses 
# 1st October --> 30th September as an hydrological year, whilst I worked with 
# 1st September --> 31th August. Considering that, I don't expect a perfect match, 
# but not even a big dispersion from the y = x diagonal if working with a scatter.
rm(list = ls())
gc()

library(ggplot2)

fname_daily <- "Results/los.dat"
fname_annual <- "Results/los_from_annual_modis_maps.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Matiu/MODIS_series/")


# Importing LOS values we got from daily or annual maps
df_day <- read.table(fname_daily, header = FALSE)
df_day <- data.frame(name = df_day$V1, year = as.numeric(df_day$V2), los = as.numeric(df_day$V3), mark = df_day$V4)

df_year <- read.table(fname_annual, header = FALSE)
df_year <- data.frame(name = df_year$V1, year = as.numeric(df_year$V2), los  = as.numeric(df_year$V3), mark = df_year$V4)


# Plotting procedure
df_plot <- merge(df_day, df_year, by = c("name", "year", "mark"), suffixes = c("_day", "_year"))

mask <- as.numeric(df_plot$los_year) == -90.0
df_to_omit <- df_plot[mask, 1:3]
write.table(df_to_omit, "Results/years_to_filter_out.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)

ggplot(df_plot, aes(x = los_day, y = los_year, color = mark)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", linewidth = 0.8) +
  scale_color_manual(
    values = c("OK" = "steelblue", "NO" = "tomato"),
    name   = "Recovered"
  ) +
  labs(
    title = "MODIS LOS",
    x     = "From daily maps [days]",
    y     = "From annual maps [days]"
  ) +
  theme_minimal(base_size = 13) +
  coord_equal()