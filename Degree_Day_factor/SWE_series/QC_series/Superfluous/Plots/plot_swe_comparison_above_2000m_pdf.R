# The main goal of this script is to develop a procedure that enables the user
# to produce comparison plots between snow height, swe from model and swe from 
# deltasnow. I have to be careful while developing the plotting procedure, because
# I need two different procedures based on completness or not of a given hydro year
rm(list = ls())
gc()

library(dplyr)
library(ggplot2)
library(nixmass)
library(patchwork)

fname_max_vals <- "Results/max_hs_swe_values.dat"
fname_with_gaps <- "../Dataset/hs_series/with_gaps/with_gaps.dat"
fname_ana <- "../../Ours/STATION_check/Correcting/ANAGRAFICA_CORRECT"
fname_complete <- "../Dataset/hs_series/all_complete/all_complete.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/QC_series/")


# Function to split a dataframe into pre-season, season and post-season series for an incomplete hydro series
split_series_gap <- function(df, start_zero, end_zero, appo_gaps, ele){
  n <- nrow(df)
  dates <- df$date
  
  idx_end <- max(which(dates <= end_zero))
  idx_start <- min(which(dates >= start_zero))
  
  
  # Values to be neglected
  df$value_not <- NA
  df$value_not[idx_start:idx_end]   <- df$value[idx_start:idx_end]
  
  known_idx <- which(!is.na(df$value_not[idx_start:idx_end]))
  if (length(known_idx) >= 2) {
    df$value_not[idx_start:idx_end] <- approx(
      x    = known_idx,
      y    = df$value_not[idx_start:idx_end][known_idx],
      xout = 1:(idx_end - idx_start + 1),
      rule = 2
    )$y
  }
  else if(length(known_idx) == 1) {
    only_val <- df$value_not[idx_start:idx_end][known_idx]
    df$value_not[idx_start:idx_end] <- only_val
  }
  
  
  # First part of the season
  df$first_season <- NA
  df$first_season[1:idx_start]  <- df$value[1:idx_start]
  
  known_idx <- which(!is.na(df$first_season[1:idx_start]))
  if(length(known_idx) >= 2) {
    df$first_season[1:idx_start] <- approx(
      x    = known_idx,
      y    = df$first_season[1:idx_start][known_idx],
      xout = 1:idx_start,
      rule = 2
    )$y
  }
  else if(length(known_idx) == 1) {
    only_val <- df$first_season[1:idx_start][known_idx]
    df$first_season[1:idx_start] <- only_val
  }
  
  
  # Second part of the season
  df$second_season <- NA
  df$second_season[idx_end:n]  <- df$value[idx_end:n]
  if(ele < 2500) df$second_season[n] <- NA
  
  known_idx <- which(!is.na(df$second_season[idx_end:n]))
  if(length(known_idx) >= 2) {
    df$second_season[idx_end:n] <- approx(
      x    = known_idx,
      y    = df$second_season[idx_end:n][known_idx],
      xout = 1:(n - idx_end + 1),
      rule = 2
    )$y
  }
  else if(length(known_idx) == 1) {
    only_val <- df$second_season[idx_end:n][known_idx]
    df$second_season[idx_end:n] <- only_val
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



# Function to split a dataframe into pre-season, season and post-season series for a complete hydrological year
split_series_com <- function(df, start_zero, end_zero, ele) {
  n     <- nrow(df)
  dates <- df$date
  
  idx_end   <- max(which(dates <= end_zero))
  idx_start <- min(which(dates >= start_zero))
  
  # No-season (to get rid of)
  df$value_not <- NA
  df$value_not[idx_start:idx_end]   <- df$value[idx_start:idx_end]
  
  # First part of the season
  df$first_season <- NA
  df$first_season[1:idx_start]  <- df$value[1:idx_start]
  
  # Second part of the season
  df$second_season <- NA
  df$second_season[idx_end:n]  <- df$value[idx_end:n]
  if(ele < 2500) df$second_season[n] <- NA
  
  df
}


# Function to create comparison plot between HS and SWE from model for an incomplete hydrological year
plot_swe_comparison_gap <- function(hs_series, swe_from_model, station_name, year, ele, end_zero, start_zero, max_swe, max_hs) {
  
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
  df_hs <- split_series_gap(df_hs, start_zero, end_zero, as.numeric(df_hs$value), ele)
  df_swe_model <- split_series_gap(df_swe_model, start_zero, end_zero, as.numeric(df_swe_model$value), ele)
  
  # Panel 1: HS
  p1 <- ggplot(df_hs, aes(x = date)) +
    geom_area(aes(y = value_not),    fill = "red",    alpha = 0.2) +
    geom_area(aes(y = first_season), fill = "grey40", alpha = 0.2) +
    geom_area(aes(y = second_season), fill = "grey40", alpha = 0.2) +
    geom_line(aes(y = value_not),    color = "red",    linewidth = 0.7) +
    geom_line(aes(y = first_season), color = "grey40", linewidth = 0.7) +
    geom_line(aes(y = second_season), color = "grey40", linewidth = 0.7) +
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
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  # Panel 3: SWE from model
  p3 <- ggplot(df_swe_model, aes(x = date)) +
    geom_area(aes(y = value_not),    fill = "red",      alpha = 0.2) +
    geom_area(aes(y = first_season), fill = "#2171b5",  alpha = 0.2) +
    geom_area(aes(y = second_season), fill = "#2171b5",  alpha = 0.2) +
    geom_line(aes(y = value_not),    color = "red",     linewidth = 0.7) +
    geom_line(aes(y = first_season), color = "#2171b5", linewidth = 0.7) +
    geom_line(aes(y = second_season), color = "#2171b5", linewidth = 0.7) +
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



# Function to create comparison plot between HS, SWE from DeltaSnow and SWE from model for a complete hydro series
plot_swe_comparison_com <- function(hs_series, swe_from_hs, swe_from_model, station_name, year, ele, end_zero, start_zero, max_swe, max_hs) {
  
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
  df_hs        <- split_series_com(df_hs, start_zero, end_zero, ele)
  df_swe_hs    <- split_series_com(df_swe_hs, start_zero, end_zero, ele)
  df_swe_model <- split_series_com(df_swe_model, start_zero, end_zero, ele)
  
  # Panel 1: HS
  p1 <- ggplot(df_hs, aes(x = date)) +
    geom_area(aes(y = value_not),    fill = "red",    alpha = 0.2) +
    geom_area(aes(y = first_season), fill = "grey40", alpha = 0.2) +
    geom_area(aes(y = second_season), fill = "grey40", alpha = 0.2) +
    geom_line(aes(y = value_not),    color = "red",    linewidth = 0.7) +
    geom_line(aes(y = first_season), color = "grey40", linewidth = 0.7) +
    geom_line(aes(y = second_season), color = "grey40", linewidth = 0.7) +
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
    coord_cartesian(ylim = c(0, max_hs*1.1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(title = paste0(station_name, " (", ele, " m a.s.l.) — ", year - 1, " to ", year), x = NULL, y = "HS [cm]") +
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
    geom_area(aes(y = value_not), fill = "red",      alpha = 0.2) +
    geom_area(aes(y = first_season), fill = "#2171b5",  alpha = 0.2) +
    geom_area(aes(y = second_season), fill = "#2171b5",  alpha = 0.2) +
    geom_line(aes(y = value_not), color = "red",     linewidth = 0.7) +
    geom_line(aes(y = first_season), color = "#2171b5", linewidth = 0.7) +
    geom_line(aes(y = second_season), color = "#2171b5", linewidth = 0.7) +
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
    coord_cartesian(ylim = y_range) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
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
    geom_area(aes(y = first_season), fill = "#2171b5",  alpha = 0.2) +
    geom_area(aes(y = second_season), fill = "#2171b5",  alpha = 0.2) +
    geom_line(aes(y = value_not),    color = "red",     linewidth = 0.7) +
    geom_line(aes(y = first_season), color = "#2171b5", linewidth = 0.7) +
    geom_line(aes(y = second_season), color = "#2171b5", linewidth = 0.7) +
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
    coord_cartesian(ylim = y_range) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
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





# Importing complete and with gaps metadatas
df_complete <- read.table(fname_complete)
df_complete <- data.frame(name = df_complete$V1, year = as.numeric(df_complete$V2), status = rep("com", length(df_complete$V1)))

df_with_gaps <- read.table(fname_with_gaps)
df_with_gaps <- data.frame(name = df_with_gaps$V1, year = as.numeric(df_with_gaps$V2), status = rep("gap", length(df_with_gaps$V1)))

df_tot <- rbind(df_complete, df_with_gaps)
df_max_vals <- read.table(fname_max_vals, header = TRUE)


# Importing ANAGRAFICA to be able to assess station elevation
df_ana <- read.table(fname_ana, header = TRUE)
name_ana <- df_ana$station_name
ele_ana <- df_ana$ele_rev


# Cycle over stations
for(name in unique(df_tot$name)){
  
  # Selecting elevation for a given station
  mask <- name_ana == name
  ele <- ele_ana[mask]
  if(length(ele) > 1) stop(paste0("Problem with elevation of ", name))
  else if(ele < 2000) next
  
  
  # Importing HS and SWE series for a given station
  fname <- paste0("../../Ours/STATION_series/Dataset/station_series/", name)
  df_hs <- read.table(fname)
  
  fname <- paste0("../Dataset/model_runs/hydro/SNWD/", sub("HSD_", "DV_SDH_", name))
  df_swe_model <- read.table(fname)
  
  if(min(as.numeric(df_hs$V1), na.rm = TRUE) > 2023) next
  
  
  # Selecting hydrological years to be plotted 
  mask <- df_tot$name == name
  status <- df_tot$status[mask]
  total_years <- unique(as.numeric(df_tot$year)[mask])
  if(length(total_years) != length(status)) stop(paste0("Some years are present twice in the dataset for ", name))
  
  
  # Selecting maximum scale values
  mask <- df_max_vals$name == name
  if(sum(mask, na.rm = TRUE) != 1) stop(paste0("Problems with maximum scale values evaluation for ", name))
  max_hs <- df_max_vals$max_hs[mask]
  max_swe <- df_max_vals$max_swe[mask]
  
  
  # Cycle over years
  for(i in seq_along(total_years)){
    
    # Check over years that must not be plotted
    y <- total_years[i]
    if(y %in% c(2024, 2025)) next
    
    # Selecting period to be neglected
    start_zero <- as.Date(c(paste0(y, "-07-01"), paste0(y, "-07-15")))
    end_zero   <- as.Date(c(paste0(y, "-08-31"), paste0(y, "-08-15")))
    
    if(ele < 2500) ind <- 1
    else ind <- 2
    
    appo_start <- start_zero[ind]
    appo_end <- end_zero[ind]
    
    
    
    
    if(status[i] == "gap"){
      
      # Selecting HS and SWE series
      mask <- as.numeric(df_hs$V1) == y
      hs_series <- as.numeric(df_hs$V2)[mask]
      if(all(!is.na(hs_series))) stop(paste0("Problems! HS series for ", name, " during ", total_years[i], " is indeed complete!"))
      
      mask <- as.numeric(df_swe_model$V1) == y
      swe_from_model <- as.numeric(df_swe_model$V2)[mask]
      
      
      # Actual plotting procedure
      suppressWarnings({
        p <- plot_swe_comparison_gap(
          hs_series      = hs_series,
          swe_from_model = swe_from_model,
          station_name   = name,
          year           = y,
          ele            = ele,
          end_zero       = appo_end,
          start_zero     = appo_start,
          max_swe        = max_swe, 
          max_hs         = max_hs
        )
        
        ggsave(paste0("Pdf/", name, "_", y-1,"_to_", y, ".pdf"), plot = p, width = 12, height = 10, version = cairo_pdf)
      })
    }
    
    
    
    
    else if(status[i] == "com"){
      
      # Selecting HS and SWE series
      mask <- as.numeric(df_hs$V1) == y
      hs_series <- as.numeric(df_hs$V2)[mask]
      if(any(is.na(hs_series))) stop(paste0("Problems! HS series for ", name, " during ", total_years[i], " is indeed incomplete!"))
      
      mask <- as.numeric(df_swe_model$V1) == y
      swe_from_model <- as.numeric(df_swe_model$V2)[mask]
      
      
      # Ready for HS to SWE conversion
      swe_from_hs <- numeric(0)
      hs_hydro <- hs_series/100
      
      if(hs_series[1] == 0){
        dates <- seq(as.Date(paste0(y-1, "-09-01")), as.Date(paste0(y, "-08-31")), by = "day")
        hsdata <- data.frame(date = dates, hs = hs_hydro)
        swe_from_hs <- swe.delta.snow(hsdata, dyn_rho_max = FALSE)
      }
      else{
        hs_hydro <- c(0, hs_hydro)
        dates <- seq(as.Date(paste0(y-1, "-08-31")), as.Date(paste0(y, "-08-31")), by = "day")
        hsdata <- data.frame(date = dates, hs = hs_hydro)
        
        appo <- swe.delta.snow(hsdata, dyn_rho_max = FALSE)
        swe_from_hs <- appo[-1]
      }
      
      if(length(swe_from_model) != length(swe_from_hs)) stop(paste0("Length mismatch for ", name, " during ", y))
      
      
      # Plotting procedure
      suppressWarnings({
        p <- plot_swe_comparison_com(
          hs_series      = hs_series,
          swe_from_hs    = swe_from_hs,
          swe_from_model = swe_from_model,
          station_name   = name,
          year           = y,
          ele            = ele,
          end_zero       = appo_end,
          start_zero     = appo_start,
          max_swe        = max_swe, 
          max_hs         = max_hs
        )
        
        ggsave(paste0("Pdf/", name, "_", y-1,"_to_", y, ".pdf"), plot = p, width = 12, height = 10, version = cairo_pdf)
      })
      
    }
    
    else stop(paste0("No correct distintion between complete and incomplete series!"))
    print(paste0("Made plot for ", name, " (", ele, " m)  -  ", y-1," to ", y))
  }
}