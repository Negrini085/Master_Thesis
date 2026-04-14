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


# Function to find maximum swe value
compute_swe_series <- function(df_swe, df_hs, station_years){
  
  # Appo variables
  hs_swe <- numeric(0)
  mod_swe <- numeric(0)
  hs_series <- numeric(0)
  year_series <- numeric(0)
  
  for(j in station_years){
    # Selecting swe from model series
    mask <- as.numeric(df_hs$V1) == j
    hs_hydro <- as.numeric(df_hs$V2)[mask]/100
    if(hs_hydro[1] != 0) next
    
    # Creating dataset as input to delta-snow model
    dates <- seq(as.Date(paste0(j-1, "-09-01")), as.Date(paste0(j, "-08-31")), by = "day")
    hsdata <- data.frame(date = dates, hs = hs_hydro)
    
    # HS to SWE
    mask <- as.numeric(df_swe$V1) == j
    swe_from_model <- as.numeric(df_swe$V2)[mask]
    swe_from_hs <- swe.delta.snow(hsdata, dyn_rho_max = FALSE)
    
    hs_swe <- c(hs_swe, swe_from_hs)
    mod_swe <- c(mod_swe, swe_from_model)
    hs_series <- c(hs_series, hs_hydro * 100)
    year_series <- c(year_series, rep(j, length(swe_from_hs)))
  }
  
  df_return <- data.frame(
    year = year_series, 
    hs = hs_series, 
    mod = mod_swe, 
    del = hs_swe
  )
  
  return(df_return)
}

# Function to split a dataframe into pre-season, season and post-season series
split_series <- function(df, start_zero, end_zero, ele) {
  n     <- nrow(df)
  dates <- df$date
  
  idx_end   <- max(which(dates <= end_zero))
  idx_start <- min(which(dates >= start_zero))
  
  # No-season (to get rid of)
  df$value_not <- NA
  df$value_not[idx_start:idx_end]   <- df$value[idx_start:idx_end]
  
  # Season (to keep)
  df$value_season <- NA
  df$value_season[1:idx_start]  <- df$value[1:idx_end]
  df$value_season[idx_end:n]  <- df$value[idx_end:n]
  if(ele < 2500) df$value_season[n] <- NA
  
  df
}


# Function to create comparison plot between HS, SWE from DeltaSnow and SWE from model
plot_swe_comparison <- function(hs_series, swe_from_hs, swe_from_model, station_name, year, ele, end_zero, start_zero, max_swe, max_hs) {
  
  dates <- seq(as.Date(paste0(year-1, "-09-01")), as.Date(paste0(year, "-08-31")), by = "day")
  
  # Build dataframes
  df_hs        <- data.frame(date = dates, value = hs_series)
  df_swe_hs    <- data.frame(date = dates, value = swe_from_hs)
  df_swe_model <- data.frame(date = dates, value = swe_from_model)
  
  # Common y-range for SWE plots
  y_range    <- c(0, max_swe*1.1)
  
  # Find date of maximum HS
  max_idx  <- which.max(hs_series)
  max_date <- dates[max_idx]
  
  # Split each series into pre, season and post
  df_hs        <- split_series(df_hs, start_zero, end_zero, ele)
  df_swe_hs    <- split_series(df_swe_hs, start_zero, end_zero, ele)
  df_swe_model <- split_series(df_swe_model, start_zero, end_zero, ele)
  
  # Panel 1: HS
  p1 <- ggplot(df_hs, aes(x = date)) +
    geom_area(aes(y = value_not),    fill = "red",    alpha = 0.2) +
    geom_area(aes(y = value_season), fill = "grey40", alpha = 0.2) +
    geom_line(aes(y = value_not),    color = "red",    linewidth = 0.7) +
    geom_line(aes(y = value_season), color = "grey40", linewidth = 0.7) +
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
    coord_cartesian(ylim = c(0, max_hs*1.1)) +
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
  p2 <- ggplot(df_swe_hs, aes(x = date)) +
    geom_area(aes(y = value_not),    fill = "red",      alpha = 0.2) +
    geom_area(aes(y = value_season), fill = "#2171b5",  alpha = 0.2) +
    geom_line(aes(y = value_not),    color = "red",     linewidth = 0.7) +
    geom_line(aes(y = value_season), color = "#2171b5", linewidth = 0.7) +
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
  p3 <- ggplot(df_swe_model, aes(x = date)) +
    geom_area(aes(y = value_not),    fill = "red",      alpha = 0.2) +
    geom_area(aes(y = value_season), fill = "#2171b5",  alpha = 0.2) +
    geom_line(aes(y = value_not),    color = "red",     linewidth = 0.7) +
    geom_line(aes(y = value_season), color = "#2171b5", linewidth = 0.7) +
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
for(name in station_names){
  
  # Selecting elevation for a given station
  mask <- names_ana == name
  ele <- elev_ana[mask]
  if(length(ele) > 1) stop(paste0("Problem with elevation of ", name))
  else if(ele < 2000) next
  
  # Importing hs dataset and model swe series for a given station
  df_hs <- read.table(paste0("../Dataset/hs_series/all_complete/", name))
  df_swe <- read.table(paste0("../Dataset/model_runs/hydro/SNWD/V_SDH_", sub("HSD_", "", name)))
  
  # Selecting hydrological years that can be a part of our comparison
  mask <- df$V1 == name
  station_years <- unique(all_years[mask])
  
  # SWE series from model and from deltasnow
  merged_df <- compute_swe_series(df_swe = df_swe, df_hs = df_hs, station_years = station_years)
  max_swe_station <- max(c(max(as.numeric(merged_df$mod), na.rm = TRUE), max(as.numeric(merged_df$del), na.rm = TRUE)), na.rm = TRUE)
  max_hs_station <- max(as.numeric(merged_df$hs), na.rm = TRUE)
  
  # Cycle across years
  for(y in unique(as.numeric(merged_df$year))){
    
    # Selecting period to be neglected
    start_zero <- as.Date(c(paste0(y, "-07-01"), paste0(y, "-07-15")))
    end_zero   <- as.Date(c(paste0(y, "-08-31"), paste0(y, "-08-15")))
    
    if(ele < 2500) ind <- 1
    else ind <- 2

    appo_start <- start_zero[ind]
    appo_end <- end_zero[ind]

    # Plotting procedure
    mask <- as.numeric(merged_df$year) == y
    suppressWarnings({
      p <- plot_swe_comparison(
        hs_series      = as.numeric(merged_df$hs)[mask],
        swe_from_hs    = as.numeric(merged_df$del)[mask],
        swe_from_model = as.numeric(merged_df$mod)[mask],
        station_name   = name,
        year           = y,
        ele            = ele,
        end_zero       = appo_end,
        start_zero     = appo_start,
        max_swe        = max_swe_station, 
        max_hs         = max_hs_station
      )
      
      ggsave(paste0("Images/start_with_zero/", name, "_", y, ".png"), plot = p, width = 12, height = 10, dpi = 150)
    })

    print(paste0("Made plot for ", name, " (", ele, " m) -  ", y))
  }
}