# The main goal of this script is to create a comparison plot for every station (and
# as a whole). On the x axis I will put the DeltaSnow series, whilst on the y axis the
# modeled one
rm(list = ls())
gc()

library(ggplot2)
library(nixmass)
library(patchwork)

fname_ana <- "../../Ours/STATION_check/Correcting/ANAGRAFICA_CORRECT"
fname_hs_dataset <- "../Dataset/hs_series/all_complete/all_complete.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/QC_series/")

# Function to create comparison plot between HS, SWE from DeltaSnow and SWE from model
plot_swe_comparison <- function(swe_from_hs, swe_from_model, station_name, ele) {
  
  df_comp <- data.frame(swe_hs = swe_from_hs, swe_mod = swe_from_model)
  df_comp <- na.omit(df_comp)

  max_val <- max(c(swe_from_hs, swe_from_model), na.rm = TRUE)
  
  p <- ggplot(df_comp, aes(x = swe_hs, y = swe_mod)) +
    geom_point(color = "steelblue", alpha = 0.4, size = 1.2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey30", linewidth = 0.8) +
    labs(
      title = paste("Station: ", station_name),
      subtitle = paste("Elevation:", ele, "m a.s.l."),
      x = "SWE from Snow Height (HS) [mm]",
      y = "SWE from Model [mm]"
    ) +
    coord_fixed(ratio = 1, xlim = c(0, max_val), ylim = c(0, max_val)) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"),
      panel.grid.major = element_line(color = "grey90"),
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(face = "bold")
    )
  
  return(p)
}


# Importing station names and years to be taken care of
df <- read.table(fname_hs_dataset)
station_names <- unique(df$V1)
all_years <- as.numeric(df$V2)


# Importing station names and elevation from ANAGRAFICA
df_ana <- read.table(fname_ana, header = TRUE)
names_ana <- df_ana$station_name
elev_ana <- as.numeric(df_ana$ele_rev)

# Cycle across stations
appo_name <- character(0)
for(name in station_names){
  
  # Appo variables
  appo_model <- numeric(0)
  appo_delta <- numeric(0)
  
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

    # Plot creation
    appo_delta <- c(appo_delta, swe_from_hs)
    appo_model <- c(appo_model, swe_from_model)
  }
  
  # Checks before plotting
  if(length(appo_delta) != length(appo_model)) stop(paste0("Problems with ", name, " series: no compatible lenghts!"))
  if(length(appo_delta) == 0){
    print(paste0("No datas for ", name, "! Skipping to next station!"))
    next
  }
  
  # Saving plot
  p <- plot_swe_comparison(swe_from_hs = appo_delta, swe_from_model = appo_model, station_name = name, ele = ele)
  ggsave(paste0("Images/delta_vs_model/", name, ".png"), plot = p, width = 12, height = 10, dpi = 150)
  print(paste0("Made plot for ", name))
  appo_name <- c(appo_name, name)
}


# Saving station names for which I made a plot
df <- read.table(name <- appo_name)
write.table(df, "Images/delta_vs_model/plotted_stations.dat", col.names = FALSE, row.names = FALSE, quote = FALSE)