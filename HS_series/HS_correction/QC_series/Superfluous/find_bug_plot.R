# The main goal of this script is to plot HS series in order to asses whether a 
# faulty procedure leaves us with no other choice than correcting again the time
# series for some given stations.
rm(list = ls())
gc()

library(dplyr)
library(ggplot2)
library(patchwork)

to_check <- "Superfluous/stations_to_check.dat"
ana <- "../../Ours/STATION_check/Correcting/ANAGRAFICA_CORRECT"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/QC_series/")


find_gap_hs <- function(df) {
  n <- nrow(df)
  
  gap_flag <- is.na(df$value)
  df$gap_flag <- gap_flag
  
  df$next_date <- c(df$date[-1], df$date[n] + 1)
  
  # Fix: gestisci correttamente il default value
  first_gap <- if(is.na(df$gap_flag[1])) FALSE else df$gap_flag[1]
  df$group_id <- cumsum(df$gap_flag != dplyr::lag(df$gap_flag, default = !first_gap))
  
  gap_periods <- df %>%
    filter(gap_flag) %>%
    group_by(group_id) %>%
    summarise(
      start_date = min(date),
      end_date = max(next_date),
      .groups = 'drop'
    )
  
  df$gap_periods <- list(gap_periods)
  
  return(df)
}


plot_hs_comparison <- function(new_hs, old_hs, swe_from_model, station_name, year, max_swe, max_hs) {
  
  dates <- seq(as.Date(paste0(year-1, "-09-01")), as.Date(paste0(year, "-08-31")), by = "day")
  if(length(dates) != length(new_hs)) stop(paste0("No compatible length for ", station_name, " in ", year))
  if(length(new_hs) != length(old_hs)) stop(paste0("No compatible length for ", station_name, " in ", year))
  if(length(new_hs) != length(swe_from_model)) stop(paste0("No compatible length for ", station_name, " in ", year))
  
  df_new_hs    <- data.frame(date = dates, value = new_hs)
  df_old_hs    <- find_gap_hs(data.frame(date = dates, value = old_hs))
  df_swe_model <- data.frame(date = dates, value = swe_from_model)
  y_range <- c(0, max_swe * 1.1)

  
  # Panel 1: HS
  p1 <- ggplot(df_new_hs, aes(x = date)) +
    geom_area(aes(y = value), fill = "grey40", alpha = 0.2) +
    geom_line(aes(y = value), color = "grey40", linewidth = 0.7) +
    coord_cartesian(ylim = c(0, max_hs * 1.1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(title = paste0(station_name, " — ", year - 1, " to ", year), 
         x = NULL, y = "HS new [cm]") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title.y = element_text(size = 14),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  # Panel 2: SWE from DeltaSnow
  p2 <- ggplot(df_old_hs, aes(x = date)) +
    geom_rect(
      data = df_old_hs$gap_periods[[1]], 
      aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
      fill = "#C1FFC1", alpha = 0.3, color = NA, inherit.aes = FALSE
    ) +
    geom_area(aes(y = value), fill = "grey40", alpha = 0.2) +
    geom_line(aes(y = value), color = "grey40", linewidth = 0.7) +
    coord_cartesian(ylim = c(0, max_hs * 1.1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(x = NULL, y = "HS old [cm]") +
    theme_minimal() +
    theme(
      axis.title.y = element_text(size = 14),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
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
  
  p1 / p2 / p3
}










# Importing station names
df <- read.table(to_check)
station_names <- df$V1

df <- read.table(ana, header = TRUE)
ana_names <- df$station_name
ana_ele <- as.numeric(df$ele_rev)



# Cycle across stations
for(name in station_names){
  
  # Check on elevation
  ele <- ana_ele[ana_names == name]
  if(length(ele) == 0 || ele < 1750) next
  
  
  # Reading dataset
  fname <- paste0("Dataset/Final/", name)
  df_new_hs <- read.table(fname)
  if(max(as.numeric(df_new_hs$V1), na.rm = TRUE) <= 2010) next
  
  fname <- paste0("Dataset/Filtered/", name)
  df_old_hs <- read.table(fname)
  
  fname_swe <- paste0("../Dataset/model_runs/hydro/SNWD/", sub("HSD_", "DV_SDH_", name))
  if(!file.exists(fname_swe)) stop(paste0("No SWE series from model for ", name))
  df_swe_model <- read.table(fname_swe)
  
  cat("\n")
  cat("\n")
  cat("\n")
  cat("Station: ", name, "\n")
  cat("\n")
  
  
  # Filter over years
  mask <- as.numeric(df_new_hs$V1) > 2010
  df_new_hs <- df_new_hs[mask, ]
  
  mask <- as.numeric(df_old_hs$V1) > 2010
  df_old_hs <- df_old_hs[mask, ]
  
  mask <- as.numeric(df_swe_model$V1) > 2010
  df_swe_model <- df_swe_model[mask, ]
  
  
  # Cycle over years
  years <- unique(as.numeric(df_new_hs$V1)) 
  for(i in seq_along(years)){
    
    y <- years[i]
    if(y %in% c(2024, 2025)){
      print(paste0("Station has ", y))
      next
    }
    cat("Processing year:", y, "\n")
    
    # Data selection for year y
    mask_year <- as.numeric(df_new_hs$V1) == y
    new_hs <- as.numeric(df_new_hs$V2)[mask_year]
    
    mask_year <- as.numeric(df_old_hs$V1) == y
    old_hs <- as.numeric(df_old_hs$V2)[mask_year]
    
    mask_swe <- as.numeric(df_swe_model$V1) == y
    swe_from_model <- as.numeric(df_swe_model$V2)[mask_swe]
    
    
    # Plot
    suppressWarnings({
      p <- plot_hs_comparison(
        new_hs = new_hs,
        old_hs = old_hs,
        swe_from_model = swe_from_model,
        station_name = name,
        year = y,
        max_swe = max(swe_from_model, na.rm = TRUE), 
        max_hs = max(c(max(new_hs, na.rm = TRUE), max(old_hs, na.rm = TRUE)), na.rm = TRUE)
      )
      
      ggsave(paste0("Appo/", name, "_", y-1, "_to_", y, ".pdf"), 
             plot = p, width = 12, height = 10, device = cairo_pdf)
    })
  }
}