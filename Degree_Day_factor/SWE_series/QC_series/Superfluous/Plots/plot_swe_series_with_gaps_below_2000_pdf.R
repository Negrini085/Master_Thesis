# The main goal of this script is to enable the user to asses whether gaps can be 
# recovered or not. I can't convert HS to SWE via DeltaSnow, so there will be only
# two series to be investigated. The main architecture of the plot will be the same
# as for complete years, because we want it to be as visually consistent as possible.
rm(list = ls())
gc()

library(dplyr)
library(ggplot2)
library(nixmass)
library(lubridate)
library(patchwork)

fname_hs_dataset <- "../Dataset/hs_series/with_gaps/with_gaps.dat"
fname_ana <- "../../Ours/STATION_check/Correcting/ANAGRAFICA_CORRECT"
fname_complete <- "../Dataset/hs_series/all_complete/all_complete.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/QC_series/")

# Find max HS value (I have to consider also year with gaps because we will be plotting both)
compute_max_hs <- function(station_name){
  
  # Importing whole hs series
  fname <- paste0("../../Ours/STATION_series/Dataset/station_series/", station_name)
  df_for_max <- read.table(fname)
  hs_for_max <- as.numeric(df_for_max$V2)
  year_for_max <- as.numeric(df_for_max$V1)
  
  # Selecting only years which will be plotted (no 2024 or 2025)
  mask <- year_for_max != 2024 & year_for_max != 2025
  hs_for_max <- hs_for_max[mask]
  
  # Finding actual hs max
  appo_max <- max(hs_for_max, na.rm = TRUE)
  return(appo_max)
}

# Function to compute max swe for model
compute_max_swe_model <- function(df_swe, total_years){
  
  # Mask to be in total years
  mask <- as.numeric(df_swe$V1) %in% total_years
  appo_swe <- df_swe$V2[mask]
  
  return(max(appo_swe, na.rm = TRUE))
}

# Function to compute max swe from delta snow
compute_max_swe_delta <- function(name, complete_years){
  
  # Importing whole hs series
  fname <- paste0("../../Ours/STATION_series/Dataset/station_series/", name)
  df_for_max <- read.table(fname)
  
  # Selecting only complete hydrological years
  mask <- as.numeric(df_for_max$V1) %in% complete_years
  df_appo <- df_for_max[mask, ]
  
  appo_swe <- numeric(0)
  for(y in unique(as.numeric(df_appo$V1))){
    
    # Selecting hs datas for a given year
    mask <- as.numeric(df_appo$V1) == y
    hs_hydro <- as.numeric(df_appo$V2)[mask]/100
    dates <- seq(as.Date(paste0(y-1, "-09-01")), as.Date(paste0(y, "-08-31")), by = "day")
    if(hs_hydro[1] != 0){
      hs_hydro <- c(0, hs_hydro)
      dates <- seq(as.Date(paste0(y-1, "-08-31")), as.Date(paste0(y, "-08-31")), by = "day")
    }
    
    # Creating dataframe to give as input to deltasnow model
    hsdata <- data.frame(date = dates, hs = hs_hydro)
    
    appo_swe <- c(appo_swe, swe.delta.snow(hsdata, dyn_rho_max = FALSE))
  }
  
  return(max(appo_swe, na.rm = TRUE))
}

# Function to create dataset to be plottes
compute_swe_series <- function(df_swe, df_hs, station_years){
  
  # Appo variables
  mod_swe <- numeric(0)
  hs_series <- numeric(0)
  year_series <- numeric(0)
  
  for(j in station_years){
    if(j %in% c(2024, 2025)) next
    
    # Selecting HS for a given year
    mask <- as.numeric(df_hs$V1) == j
    hs_hydro <- as.numeric(df_hs$V2)[mask]
    
    # Selecting model SWE for a given year
    mask <- as.numeric(df_swe$V1) == j
    swe_from_model <- as.numeric(df_swe$V2)[mask]
    
    if(length(swe_from_model) != length(hs_hydro)){
      print(length(hs_hydro))
      print(length(swe_from_model))
      stop(paste0("Length mismatch for year ", j))
    }
    
    hs_series <- c(hs_series, hs_hydro)
    mod_swe <- c(mod_swe, swe_from_model)
    year_series <- c(year_series, rep(j, length(swe_from_model)))
  }
  
  # Data-frame to return -> no HS to SWE conversion is made because there are gaps
  df_return <- data.frame(year = year_series, hs = hs_series, mod = mod_swe)
  return(df_return)
}

# Function to split a dataframe into pre-season, season and post-season series
split_series <- function(df, end_zero, start_zero, appo_gaps){
  n <- nrow(df)
  dates <- df$date
  
  idx_end <- max(which(dates <= end_zero))
  idx_start <- min(which(dates >= start_zero))
  
  
  # Pre-season
  df$value_pre <- NA
  df$value_pre[1:idx_end] <- df$value[1:idx_end]
  
  known_idx <- which(!is.na(df$value_pre[1:idx_end]))
  if (length(known_idx) >= 2) {
    df$value_pre[1:idx_end] <- approx(
      x    = known_idx,
      y    = df$value_pre[known_idx],
      xout = 1:idx_end,
      rule = 2
    )$y
  }
  else if(length(known_idx) == 1) {
    only_val <- df$value_pre[1:idx_end][known_idx]
    df$value_pre[1:idx_end] <- only_val
  }
  
  
  # Season
  df$value_season <- NA
  df$value_season[idx_end:idx_start] <- df$value[idx_end:idx_start]
  
  known_idx <- which(!is.na(df$value_season[idx_end:idx_start]))
  if (length(known_idx) >= 2) {
    df$value_season[idx_end:idx_start] <- approx(
      x    = known_idx,
      y    = df$value_season[idx_end:idx_start][known_idx],
      xout = 1:(idx_start - idx_end + 1),
      rule = 2
    )$y
  }
  else if(length(known_idx) == 1) {
    only_val <- df$value_season[idx_end:idx_start][known_idx]
    df$value_season[idx_end:idx_start] <- only_val
  }
  
  
  # Post-season
  df$value_post <- NA
  df$value_post[idx_start:n] <- df$value[idx_start:n]
  
  known_idx <- which(!is.na(df$value_post[idx_start:n]))
  if(length(known_idx) >= 2) {
    df$value_post[idx_start:n] <- approx(
      x    = known_idx,
      y    = df$value_post[idx_start:n][known_idx],
      xout = 1:(n - idx_start + 1),
      rule = 2
    )$y
  }
  else if(length(known_idx) == 1) {
    only_val <- df$value_post[idx_start:n][known_idx]
    df$value_post[idx_start:n] <- only_val
  }
  
  
  # Gap handling
  gap_flag <- is.na(appo_gaps)
  df$gap_flag <- gap_flag
  
  df$next_date <- c(df$date[-1], df$date[n] + 1)
  # df$previous_date <- c(df$date[1] - 1, df$date[-n])
  
  df$group_id <- cumsum(df$gap_flag != lag(df$gap_flag, default = !df$gap_flag[1]))

  gap_periods <- df %>%
    filter(gap_flag) %>%
    group_by(group_id) %>%
    summarise(
      start_date = min(date),
      end_date = max(next_date)
    )
  
  df$gap_periods <- list(gap_periods)
  df
}


# Function to create comparison plot between HS, SWE from DeltaSnow and SWE from model
plot_swe_comparison <- function(hs_series, swe_from_model, station_name, year, ele, end_zero, start_zero, max_swe, max_hs) {
  
  dates <- seq(as.Date(paste0(year-1, "-09-01")), as.Date(paste0(year, "-08-31")), by = "day")
  
  # Build dataframes
  df_hs <- data.frame(date = dates, value = hs_series)
  df_swe_model <- data.frame(date = dates, value = swe_from_model)
  
  # Common y-range for SWE plots
  y_range    <- c(0, max_swe*1.1)
  
  # Find date of maximum HS
  max_idx  <- which.max(hs_series)
  max_date <- dates[max_idx]
  
  # Split each series into pre, season and post
  df_hs <- split_series(df_hs, end_zero, start_zero, as.numeric(df_hs$value))
  df_swe_model <- split_series(df_swe_model, end_zero, start_zero, as.numeric(df_swe_model$value))
  
  # Panel 1: HS
  p1 <- ggplot(df_hs, aes(x = date)) +
    geom_area(aes(y = value_pre),    fill = "red",    alpha = 0.2) +
    geom_area(aes(y = value_season), fill = "grey40", alpha = 0.2) +
    geom_area(aes(y = value_post),   fill = "red",    alpha = 0.2) +
    geom_line(aes(y = value_pre),    color = "red",    linewidth = 0.7) +
    geom_line(aes(y = value_season), color = "grey40", linewidth = 0.7) +
    geom_line(aes(y = value_post),   color = "red",    linewidth = 0.7) +
    geom_rect(
      data = df_hs$gap_periods[[1]], 
      aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
      fill = "#C1FFC1", alpha = 1, color = NA, inherit.aes = FALSE
    ) +
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
    coord_cartesian(ylim = c(0, max_hs*1.1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(title = paste0(station_name, " (", ele, " m a.s.l.) — ", year - 1, " to ", year), x = NULL, y = "HS [cm]") +
    theme_minimal() +
    theme(
      plot.title   = element_text(face = "bold"),
      axis.title.y = element_text(size = 14),
      axis.text.y  = element_text(size = 12),
      axis.text.x  = element_text(size = 12),
    )
  
  # Panel 3: SWE from model
  p3 <- ggplot(df_swe_model, aes(x = date)) +
    geom_area(aes(y = value_pre),    fill = "red",      alpha = 0.2) +
    geom_area(aes(y = value_season), fill = "#2171b5",  alpha = 0.2) +
    geom_area(aes(y = value_post),   fill = "red",      alpha = 0.2) +
    geom_line(aes(y = value_pre),    color = "red",     linewidth = 0.7) +
    geom_line(aes(y = value_season), color = "#2171b5", linewidth = 0.7) +
    geom_line(aes(y = value_post),   color = "red",     linewidth = 0.7) +
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
    coord_cartesian(ylim = y_range) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(x = "Date", y = "SWE model [mm w.e.]") +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x  = element_text(size = 12),
      axis.text.y  = element_text(size = 12)
    )
  
  # Combine plots
  p1 / plot_spacer() / p3
}


# Importing station names and years to be taken care of
df <- read.table(fname_hs_dataset)
station_names <- unique(df$V1)
all_years <- as.numeric(df$V2)

df_complete <- read.table(fname_complete)
df_tot <- rbind(df, df_complete)


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
  else if(ele >= 2000) next
  
  # Checking if SWE series exists or not
  fname_swe_model <- paste0("../Dataset/model_runs/hydro/SNWD/DV_SDH_", sub("HSD_", "", name))
  if(!file.exists(fname_swe_model)) next
  
  # Selecting total years for a given station
  mask <- df_tot$V1 == name
  total_years <- as.numeric(df_tot$V2)[mask]
  
  # Importing hs dataset and model swe series for a given station
  df_hs <- read.table(paste0("../Dataset/hs_series/with_gaps/", name))
  df_swe <- read.table(fname_swe_model)
  
  # Selecting hydrological years that can be a part of our comparison
  mask <- df$V1 == name
  station_years <- unique(all_years[mask])
  
  # SWE series from model and from deltasnow
  merged_df <- compute_swe_series(df_swe = df_swe, df_hs = df_hs, station_years = station_years)
  if(nrow(merged_df) == 0){
    warning(paste0("No rows for ", name, " dataset!"))
    next
  }
  
  # Computing maximum swe from DeltaSnow
  max_swe_delta <- 0
  if(any(df_complete$V1 == name)){
    mask <- df_complete$V1 == name
    complete_years <- as.numeric(df_complete$V2)[mask]
    if(length(complete_years) == 0) stop(paste0("Not enough complete years for ", name))
    
    max_swe_delta <- compute_max_swe_delta(name = name, complete_years = complete_years)
  }
  
  max_swe_station <- max(c(compute_max_swe_model(df_swe, total_years), max_swe_delta), na.rm = TRUE)
  max_hs_station <- compute_max_hs(name)
  
  # Cycle across years
  for(y in unique(as.numeric(merged_df$year))){

    # Check over years that must not be plotted
    if(y %in% c(2024, 2025)) next
    
    # Selecting period to be neglected
    start_zero <- as.Date(c(paste0(y, "-05-01"), paste0(y, "-05-15"), paste0(y, "-06-01"), paste0(y, "-06-15")))
    end_zero   <- as.Date(c(paste0(y-1, "-10-31"), paste0(y-1, "-10-15"), paste0(y-1, "-10-01"), paste0(y-1, "-09-15")))
    
    ind <- ele %/% 500 + 1
    appo_start <- start_zero[ind]
    appo_end <- end_zero[ind]
    
    
    # Plotting procedure
    mask <- as.numeric(merged_df$year) == y
    suppressWarnings({
      p <- plot_swe_comparison(
        hs_series      = as.numeric(merged_df$hs)[mask],
        swe_from_model = as.numeric(merged_df$mod)[mask],
        station_name   = name,
        year           = y,
        ele            = ele,
        end_zero       = appo_end,
        start_zero     = appo_start,
        max_swe        = max_swe_station, 
        max_hs         = max_hs_station
      )
      
      ggsave(paste0("Pdf/with_gaps/", name, "_", y-1,"_to_", y, ".pdf"), plot = p, width = 12, height = 10, version = cairo_pdf)
    })
    
    print(paste0("Made plot for ", name, " (", ele, " m)  -  ", y-1," to ", y))
  }
}