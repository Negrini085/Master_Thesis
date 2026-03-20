# The main goal of this script is to produce a plot that enables the user to visually
# compare LOS derived from MODIS series and from in-situ datas for all stations. I will
# plot here LOS calculated after having filtered out those years that consisted of only 
# missing data or zeros
rm(list = ls())
gc()

library(dplyr)
library(ggplot2)

fname_ana <- "../Dataset/ANAGRAFICA"
fname_MOD_comp     <- "../MODIS_series/Datas/compatible/los.dat"
fname_MOD_non_comp <- "../MODIS_series/Datas/non_compatible/los.dat"
to_omit_1 <- "../STATION_series/Datas/results/over_250_NAs_italian.dat"
to_omit_2 <- "../STATION_series/Datas/results/over_half_winter_NA_italian.dat"
fname_STA_comp     <- "../STATION_series/Datas/results/correct_with_summer_average/los_compatible.dat"
fname_STA_non_comp <- "../STATION_series/Datas/results/correct_with_summer_average/los_non_compatible.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Comparison/")



# Helper function to import, merge and label a pair of files
load_and_merge <- function(fname_MOD, fname_STA, label, df1, df2){
  df_MOD <- read.table(fname_MOD, header = FALSE)
  df_MOD <- data.frame(
    name = df_MOD$V1,
    year = as.numeric(df_MOD$V2),
    los  = as.numeric(df_MOD$V3)
  )
  
  df_STA <- read.table(fname_STA, header = FALSE)
  df_STA <- data.frame(
    name = df_STA$V1,
    year = as.numeric(df_STA$V2),
    los  = as.numeric(df_STA$V3)
  )
  
  print(sum(df_STA$los == 0, na.rm = TRUE))
  
  merged <- merge(df_MOD, df_STA, by = c("name", "year"), suffixes = c("_MOD", "_STA"))
  merged$group <- label
  
  merged <- merged  %>%
    anti_join(df_to_omit_1, by = c("name" = "V1", "year" = "V2")) %>%
    anti_join(df_to_omit_2, by = c("name" = "V1", "year" = "V2"))
  
  return(merged)
}



# Importing datas to filter out
df_to_omit_1 <- read.table(to_omit_1) 
df_to_omit_2 <- read.table(to_omit_2) 


# Importing elevations in order to filter out datas
df <- read.table(fname_ana, header = FALSE)
name_ana <- df$V1
elev_ana <- df$V4


# Loading and merging both pairs
merged_comp     <- load_and_merge(fname_MOD_comp,     fname_STA_comp,     "Compatible", df_to_omit_1, df_to_omit_2)
merged_non_comp <- load_and_merge(fname_MOD_non_comp, fname_STA_non_comp, "Non-compatible", df_to_omit_1, df_to_omit_2)


# Omitting those stations above 2500 meters (no correction) for compatible dataset
names_above_2500 <- c()
for(name in unique(merged_comp$name)){
  
  # Getting elevation
  mask <- name_ana == name
  elev <- elev_ana[mask]
  
  if(length(elev) > 0 && elev > 2500){
    names_above_2500 <- c(names_above_2500, name)
  }
}

merged_comp <- merged_comp %>%
  filter(!name %in% names_above_2500)


# Omitting those stations above 2500 meters (no correction) for non compatible dataset
names_above_2500 <- c()
for(name in unique(merged_non_comp$name)){
  
  # Getting elevation
  mask <- name_ana == name
  elev <- elev_ana[mask]
  
  if(length(elev) > 0 && elev > 2500){
    names_above_2500 <- c(names_above_2500, name)
  }
}

merged_non_comp <- merged_non_comp %>%
  filter(!name %in% names_above_2500)

# mask <- merged_non_comp$los_MOD > 80 & merged_non_comp$los_STA < 20
# print(merged_non_comp[mask, ])

# Plotting procedure
df_plot <- rbind(merged_comp, merged_non_comp)

ggplot(df_plot, aes(x = los_MOD, y = los_STA, color = group)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", linewidth = 0.8) +
  scale_color_manual(
    values = c("Compatible" = "steelblue", "Non-compatible" = "tomato"),
    name   = "Station type"
  ) +
  labs(
    title = "MODIS vs Station LOS comparison",
    x     = "LOS MODIS [days]",
    y     = "LOS Station [days]"
  ) +
  theme_minimal(base_size = 13) +
  coord_equal()