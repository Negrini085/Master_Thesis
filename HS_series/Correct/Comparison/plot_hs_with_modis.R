# The main goal of this script is to evaluate which is the nature of los difference
# between hs datas and satellite ones.
rm(list = ls())
gc()

library(dplyr)
library(ggplot2)
library(patchwork)

fname <- "Dataset/over_1.3.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/Correct/Comparison/")


# Function to identify periods where MODIS == 1
find_one_mod <- function(df, flag) {
  n <- nrow(df)
  
  one_flag <- !is.na(flag) & flag == 1
  df$one_flag <- one_flag
  
  df$next_date <- c(df$date[-1], df$date[n] + 1)
  
  first_one <- if(is.na(df$one_flag[1])) FALSE else df$one_flag[1]
  df$group_id <- cumsum(df$one_flag != dplyr::lag(df$one_flag, default = !first_one))
  
  one_periods <- df %>%
    filter(one_flag) %>%
    group_by(group_id) %>%
    summarise(
      start_date = min(date),
      end_date   = max(next_date),
      .groups    = 'drop'
    )
  
  df$one_periods <- list(one_periods)
  
  return(df)
}


# Function to plot HS series
plot_hs_comparison <- function(hs_hydro, swe_hydro, mod_hydro, station_name, year, max_hs, max_swe) {
  
  dates <- seq(as.Date(paste0(year-1, "-09-01")), as.Date(paste0(year, "-08-31")), by = "day")
  if(length(dates) != length(hs_hydro)) stop(paste0("No compatible length for ", station_name, " in ", year))
  if(length(hs_hydro) != length(mod_hydro)) stop(paste0("No compatible length for ", station_name, " in ", year))
  
  df_new_hs <- find_one_mod(data.frame(date = dates, value = hs_hydro), flag = mod_hydro)
  df_swe_model <- data.frame(date = dates, value = swe_hydro)
  y_range <- c(0, max_swe * 1.1)
  
  
  # Panel 1: HS
  p1 <- ggplot(df_new_hs, aes(x = date)) +
    geom_rect(
      data = df_new_hs$one_periods[[1]],
      aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
      fill  = "#ADD8E6",
      alpha = 0.4,
      color = NA,
      inherit.aes = FALSE
    ) +
    geom_area(aes(y = value), fill = "grey40", alpha = 0.2) +
    geom_line(aes(y = value), color = "grey40", linewidth = 0.7) +
    coord_cartesian(ylim = c(0, max_hs * 1.1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(title = paste0(station_name, " — ", year - 1, " to ", year),
         x = NULL, y = "HS [cm]") +
    theme_minimal() +
    theme(
      plot.title    = element_text(face = "bold"),
      axis.title.y  = element_text(size = 14),
      axis.text.y   = element_text(size = 12),
      axis.text.x   = element_text(size = 12)
    )
  
  # Panel 3: SWE from model
  p3 <- ggplot(df_swe_model, aes(x = date)) +
    geom_area(aes(y = value), fill = "#2171b5", alpha = 0.2) +
    geom_line(aes(y = value), color = "#2171b5", linewidth = 0.7) +
    coord_cartesian(ylim = y_range) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(x = "Date", y = "SWE model [mm w.e.]") +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    )
  
  p1 / p3
}










# Importing dataset
df <- read.table(fname, header = TRUE)
hydro_years <- as.numeric(df$year)
station_names <- df$name


# Cycle across stations
for(name in unique(station_names)){
  
  # Importing HS, SWE and MODIS series
  fname_hs <- paste0("../Dataset/", name)
  if(!file.exists(fname_hs)) stop(paste0("No HS series for ", name))
  df_hs <- read.table(fname_hs)
  
  fname_mod <- paste0("../MODIS_series/Dataset/modis_hydrological/", name)
  if(!file.exists(fname_mod)) stop(paste0("No MODIS series for ", name))
  df_mod <- read.table(fname_mod)
  
  fname_swe <- paste0("../../HS_correction/Dataset/model_runs/hydro/SNWD/DV_SDH_", sub("HSD_", "", name))
  if(!file.exists(fname_swe)) stop(paste0("No SWE series for ", name))
  df_swe <- read.table(fname_swe)
  
  
  # Selecting values
  year_hs <- as.numeric(df_hs$V1)
  year_mod <- as.numeric(df_mod$V1)
  year_swe <- as.numeric(df_swe$V1)
  
  value_hs <- as.numeric(df_hs$V2)
  value_mod <- as.numeric(df_mod$V2)
  value_swe <- as.numeric(df_swe$V2)
  
  
  # Selecting which hydrological years have this kind of condition
  mask <- station_names == name
  years <- hydro_years[mask]
  
  
  # Selecting max hs + swe values
  mask <- year_hs %in% years
  if(!any(mask)) stop(paste0("No years in HS record for station", name))
  max_hs <- max(value_hs[mask], na.rm = TRUE)
  
  mask <- year_swe %in% years
  if(!any(mask)) stop(paste0("No years in SWE model output for station", name))
  max_swe <- max(value_swe[mask], na.rm = TRUE)
  
  cat("\n", "\n", "\n")
  cat("Station: ", name, "\n")
  cat("\n")
  
  
  # Cycle across years
  for(y in years){
    
    if(y > 2023) next
    
    # Is this year actually in both dataset
    if(!(y %in% year_hs)) stop(paste0("No year ", y, " in HS record for station ", name))
    if(!(y %in% year_mod)) stop(paste("No year ", y, " in MODIS record for station ", name))
    if(!(y %in% year_swe)) stop(paste("No year ", y, " in SWE model output for station ", name))
    cat("Processing year:", y, "\n")
    
    
    # Selecting hs seris and modis for a given hydrological year
    mask <- year_hs == y
    hs_hydro <- value_hs[mask]
    
    mask <- year_mod == y
    mod_hydro <- value_mod[mask]
    
    mask <- year_swe == y
    swe_hydro <- value_swe[mask]
    
    
    # Plotting procedure
    suppressWarnings({
      p <- plot_hs_comparison(
        hs_hydro = hs_hydro,
        swe_hydro = swe_hydro,
        mod_hydro = mod_hydro,
        station_name = name,
        year = y,
        max_hs = max_hs, 
        max_swe = max_swe
      )
      
      ggsave(paste0("Pdf/", name, "_", y-1, "_to_", y, ".pdf"), 
             plot = p, width = 12, height = 10, device = cairo_pdf)
    })
  }
}