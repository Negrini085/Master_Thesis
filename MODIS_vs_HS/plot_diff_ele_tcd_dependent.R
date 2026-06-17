# The main goal of this script is to plot normalized bias values as a function of 
# both tree cover density and elevation in order to check if a signiicant trend 
# can be detected.
rm(list = ls())
gc()

fname_geo <- "Results/diff_geo_dependent.dat"
fname_tcd <- "Results/diff_tcd_dependent.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/MODIS_vs_HS/")


# Importing both dataset in order to check if a significant trend is showing
df_geo <- read.table(fname_geo, header = TRUE)
df_tcd <- read.table(fname_tcd, header = TRUE)
df_merged <- merge(df_geo, df_tcd, by = "name")

df_final <- data.frame(ele = as.numeric(df_merged$elevation), tcd = as.numeric(df_merged$tcd), diff = as.numeric(df_merged$diff.x))


# Plotting procedure
ggplot() +
  geom_point(data = df_final, aes(x = ele, y = tcd, color = diff), 
             size = 1.2, alpha = 0.8) +
  scale_color_viridis_c(option = "turbo", name = "Bias", limits = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Bias: elevation & tcd dependence",
       x = "Elevation",
       y = "TCD") +
  theme(panel.background = element_rect(fill = "aliceblue"))