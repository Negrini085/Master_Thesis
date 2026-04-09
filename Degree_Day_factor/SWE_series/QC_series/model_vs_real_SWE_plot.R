# The main goal of this script is to enable the user to compare SWE series for a
# given hydrological year and a given station. I would like to make a plot made of
# three suplots. On the first row, I would like to show measured HS series. Below
# that, I would like to plot the SWE series one woul get starting from those HS 
# values using DeltaSnow model and finally, on the bottom, the model series.
rm(list = ls())
gc()

library(ggplot2)
library(nixmass)
library(patchwork)

fname_ana <- "../../Ours/STATION_check/Correcting/ANAGRAFICA_CORRECT"
fname_hs_dataset <- "../Dataset/hs_series/all_complete/all_complete.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/QC_series/")

# Function to create comparison plot between HS, SWE from DeltaSnow and SWE from model
plot_swe_comparison <- function(hsdata, swe_from_hs, swe_from_model, station_name, year, ele) {
  
  dates <- hsdata$date
  
  # Build dataframes
  df_hs <- data.frame(date = dates, value = hsdata$hs * 100)
  df_swe_hs <- data.frame(date = dates, value = swe_from_hs)
  df_swe_model <- data.frame(date = dates, value = swe_from_model)
  
  # Common y-range for SWE plots
  y_range <- range(c(swe_from_hs, swe_from_model), na.rm = TRUE)
  y_range[1] <- 0
  
  # 🔴 Find date of maximum HS
  max_idx <- which.max(hsdata$hs)
  max_date <- dates[max_idx]
  
  # Panel 1: HS
  p1 <- ggplot(df_hs, aes(x = date, y = value)) +
    geom_line(color = "grey40") +
    geom_area(alpha = 0.2, fill = "grey40") +
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
    labs(
      title = paste0(station_name, " (", ele, " m a.s.l.) — ", year),
      x = NULL,
      y = "HS [cm]"
    ) +
    theme_minimal() +
    theme(
      plot.title   = element_text(face = "bold"),
      axis.title.y = element_text(size = 14),
      axis.text.y  = element_text(size = 12),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  # Panel 2: SWE from DeltaSnow
  p2 <- ggplot(df_swe_hs, aes(x = date, y = value)) +
    geom_line(color = "#2171b5") +
    geom_area(alpha = 0.2, fill = "#2171b5") +
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
    coord_cartesian(ylim = y_range) +
    labs(
      x = NULL,
      y = "SWE ΔSnow [mm w.e.]"
    ) +
    theme_minimal() +
    theme(
      axis.title.y = element_text(size = 14),
      axis.text.y  = element_text(size = 12),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  # Panel 3: SWE from model
  p3 <- ggplot(df_swe_model, aes(x = date, y = value)) +
    geom_line(color = "#2171b5") +
    geom_area(alpha = 0.2, fill = "#2171b5") +
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
    coord_cartesian(ylim = y_range) +
    labs(
      x = "Date",
      y = "SWE model [mm w.e.]"
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x  = element_text(size = 12),
      axis.text.y  = element_text(size = 12)
    )
  
  # Combine plots
  p1 / p2 / p3
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
appo_years <- numeric(0)
appo_names <- character(0)
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
    # dates <- seq(as.Date(paste0(y-1, "-09-01")), as.Date(paste0(y, "-08-31")), by = "day")
    # hsdata <- data.frame(date = dates, hs = hs_hydro)
    # 
    # # SWE conversion
    # swe_from_hs <- swe.delta.snow(hsdata)
    # 
    # # Selecting model SWE datas
    # mask <- swe_years == y
    # swe_from_model <- swe_series[mask]
    # 
    # 
    # # Plot creation
    # p <- plot_swe_comparison(
    #   hsdata = hsdata,
    #   swe_from_hs = swe_from_hs,
    #   swe_from_model = swe_from_model,
    #   station_name  = name,
    #   year = y, 
    #   ele = ele
    # )
    # 
    # # Saving plot
    # ggsave(paste0("Images/start_with_zero/", name, "_", y, ".png"), plot = p, width = 12, height = 10, dpi = 150)
    # print(paste0("Made plot for ", name, "  -  ", y))
  }
}