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
fname_ana <- "../../../HS_series/Correct/STATION_check/Dataset/ANAGRAFICA"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_outputs/SWE/")


# Function for SWE conversion of hs series
convert_hs_swe <- function(hs_series, year){
  
  # Selecting dates
  dates <- seq(as.Date(paste0(year-1, "-09-01")), as.Date(paste0(year, "-08-31")), by = "day")
  mask <- hs_series[1] != 0
  if(mask){
    hs_series <- c(0, hs_series)
    dates <- seq(as.Date(paste0(year-1, "-08-31")), as.Date(paste0(year, "-08-31")), by = "day")
  }
  
  hsdata <- data.frame(date = dates, hs = hs_series/100)
  tryCatch({
    appo_swe <- swe.delta.snow(hsdata, dyn_rho_max = FALSE)
  }, error = function(e) {
    stop(paste0("Delta snow failed for ", name, " during ", y, ": ", e$message))
  })
  
  if(mask) appo_swe <- appo_swe[2:length(appo_swe)]
  return(appo_swe)
}


# Function to select SWE model series for a given hydrological year
find_hydro_swe <- function(df_swe_model, year){
  
  # Comparing dates and selecting swe series
  dates <- seq(as.Date(paste0(year-1, "-09-01")), as.Date(paste0(year, "-08-31")), by = "day")
  swe_dates <- as.Date(df_swe_model$date)
  
  mask <- swe_dates %in% dates
  appo_swe <- as.numeric(df_swe_model$swe)[mask]
  return(appo_swe)
}


# Function to create comparison plot between HS, SWE from DeltaSnow and SWE from model
plot_swe_comparison <- function(hs_series, swe_from_hs, swe_from_model, station_name, year, max_swe, max_hs) {
  
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
  
  # Panel 1: HS
  p1 <- ggplot(df_hs, aes(x = date)) +
    geom_area(aes(y = value), fill = "grey40", alpha = 0.2) +
    geom_line(aes(y = value), color = "grey40", linewidth = 0.7) +
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
    coord_cartesian(ylim = c(0, max_hs*1.1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(title = paste0(station_name, " — ", year - 1, " to ", year), x = NULL, y = "HS [cm]") +
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
    geom_area(aes(y = value), fill = "#2171b5",  alpha = 0.2) +
    geom_line(aes(y = value), color = "#2171b5", linewidth = 0.7) +
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
    coord_cartesian(ylim = y_range) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(x = NULL, y = "SWE ΔSnow [mm w.e.]") +
    theme_minimal() +
    theme(
      axis.title.y = element_text(size = 14),
      axis.text.y  = element_text(size = 12),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  # Panel 3: SWE from model
  p3 <- ggplot(df_swe_model, aes(x = date)) +
    geom_area(aes(y = value), fill = "#2171b5",  alpha = 0.2) +
    geom_line(aes(y = value), color = "#2171b5", linewidth = 0.7) +
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
  p1 / p2 / p3
}




# Importing ANAGRAFICA and max values to be able to assess station elevation
df_ana <- read.table(fname_ana, header = TRUE)
df_max <- read.table(fname_max_vals, header = TRUE)




# Cycle over stations
for(name in unique(df_ana$name)){

  # Selecting max HS and SWE values
  mask <- df_max$name == name
  max_hs <- as.numeric(df_max$max_hs)[mask]
  max_swe <- as.numeric(df_max$max_swe)[mask]
  
  
  # Importing HS and SWE series for a given station
  fname <- paste0("../../../HS_series/Correct/Dataset/", name)
  df_hs <- read.table(fname)
  
  fname <- paste0("Results/SWE_series/", name)
  if(!file.exists(fname)) next
  df_swe_model <- read.table(fname, header = TRUE)
  
  
  for(y in unique(as.numeric(df_hs$V1))){
    
    # Selecting HS and SWE series
    mask <- as.numeric(df_hs$V1) == y
    hs_series <- as.numeric(df_hs$V2)[mask]
    swe_from_hs <- convert_hs_swe(hs_series, y)
    swe_from_model <- find_hydro_swe(df_swe_model, y)
    
    # Plotting procedure
    suppressWarnings({
      p <- plot_swe_comparison(
        hs_series      = hs_series,
        swe_from_hs    = swe_from_hs,
        swe_from_model = swe_from_model,
        station_name   = name,
        year           = y,
        max_swe        = max_swe, 
        max_hs         = max_hs
      )
      
      ggsave(paste0("Images/", name, "_", y-1,"_to_", y, ".png"), plot = p, width = 12, height = 10, dpi = 150)
    })
    
    cat("Made plot for ", name, " HS during ", y, "\n")
  }
  
}