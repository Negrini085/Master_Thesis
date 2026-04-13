# The main goal of this script is to asses whether a certain way of SWE computation
# is biased or not
rm(list = ls())
gc()

library(ggplot2)
library(nixmass)

fname_ana <- "../../Ours/STATION_check/Correcting/ANAGRAFICA_CORRECT"
fname_hs_dataset <- "../Dataset/hs_series/all_complete/all_complete.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/QC_series/")


# Importing station names and years to be taken care of
df <- read.table(fname_hs_dataset)
station_names <- unique(df$V1)
all_years <- as.numeric(df$V2)


# Importing station names and elevation from ANAGRAFICA
df_ana <- read.table(fname_ana, header = TRUE)
names_ana <- df_ana$station_name
elev_ana <- as.numeric(df_ana$ele_rev)

# Cycle across stations
appo_bias <- numeric(0)
for(name in station_names){
  
  # Selecting elevation for a given station
  mask <- names_ana == name
  ele <- elev_ana[mask]
  if(length(ele) > 1) stop(paste0("Problem with elevation of ", name))
  
  # Importing hs dataset for a given station
  df_hs <- read.table(paste0("../Dataset/hs_series/all_complete/", name))
  hs_series <- as.numeric(df_hs$V2)
  hs_years <- as.numeric(df_hs$V1)
  
  # Importing swe dataset for a given station
  df_swe <- read.table(paste0("../Dataset/model_runs/hydro/SNWD/V_SDH_", sub("HSD_", "", name)))
  swe_series <- as.numeric(df_swe$V2)
  swe_years <- as.numeric(df_swe$V1)
  
  # Selecting hydrological years that can be a part of our comparison
  mask <- df$V1 == name
  station_years <- unique(all_years[mask])
  
  # Cycle across years
  for(y in station_years){
    
    # Selecting hs datas for a given year
    mask <- hs_years == y
    hs_hydro <- hs_series[mask]/100
    if(hs_hydro[1] != 0) next
    
    # Creating dataset as input to deltasnow model
    dates <- seq(as.Date(paste0(y-1, "-09-01")), as.Date(paste0(y, "-08-31")), by = "day")
    hsdata <- data.frame(date = dates, hs = hs_hydro)

    # SWE conversion
    swe_from_hs <- swe.delta.snow(hsdata)

    # Selecting model SWE datas
    mask <- swe_years == y
    swe_from_model <- swe_series[mask]
    
    if(length(swe_from_hs) != length(swe_from_model)) stop(paste0("Problem! No compatible lengths for ", name, " during ", y))
    hydro_bias <- swe_from_model - swe_from_hs
    appo_bias <- c(appo_bias, hydro_bias)
  }
}


# Plotting procedure
df_bias <- data.frame(bias = appo_bias)
mbe <- mean(df_bias$bias, na.rm = TRUE)

ggplot(df_bias, aes(x = bias)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_density(color = "darkblue", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_vline(xintercept = mbe, linetype = "solid", color = "darkgreen", linewidth = 1) +
  annotate("text", x = mbe, y = 0.01, label = paste("Mean Bias:", round(mbe, 2)), 
           color = "darkgreen", angle = 90, vjust = -0.5, fontface = "bold") +
  
  labs(
    title = "Distribution of Model Bias",
    subtitle = "Comparison: Hydrological Model vs. DeltaSnow SWE",
    x = "Bias (Model - DeltaSnow) [mm]",
    y = "Density"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

ggsave("bias_histogram.png", plot = last_plot(), width = 10, height = 7, dpi = 300, bg = "white")