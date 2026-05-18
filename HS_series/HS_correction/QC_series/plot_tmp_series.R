rm(list = ls())
gc()

library(dplyr)
library(ggplot2)
library(nixmass)
library(patchwork)

name <- "HSD_CH_SLFAL1"
fname <- paste0("Dataset/Tmp/", name)
fname_max_vals <- "Results/max_hs_swe_values.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/HS_correction/QC_series/")



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



plot_swe_comparison_com <- function(hs_series, swe_from_hs, swe_from_model, 
                                    station_name, year, max_swe, max_hs) {
  
  dates <- seq(as.Date(paste0(year-1, "-09-01")), 
               as.Date(paste0(year, "-08-31")), by = "day")
  
  df_hs        <- data.frame(date = dates, value = hs_series)
  df_swe_hs    <- data.frame(date = dates, value = swe_from_hs)
  df_swe_model <- data.frame(date = dates, value = swe_from_model)
  
  y_range <- c(0, max_swe * 1.1)
  max_idx <- which.max(hs_series)
  max_date <- dates[max_idx]
  # max_hs <- 350
  
  # Panel 1: HS
  p1 <- ggplot(df_hs, aes(x = date)) +
    geom_area(aes(y = value), fill = "grey40", alpha = 0.2) +
    geom_line(aes(y = value), color = "grey40", linewidth = 0.7) +
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
    coord_cartesian(ylim = c(0, max_hs * 1.1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(title = paste0(station_name, " — ", year - 1, " to ", year), 
         x = NULL, y = "HS [cm]") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title.y = element_text(size = 14),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  # Panel 2: SWE from DeltaSnow
  p2 <- ggplot(df_swe_hs, aes(x = date)) +
    geom_area(aes(y = value), fill = "#2171b5", alpha = 0.2) +
    geom_line(aes(y = value), color = "#2171b5", linewidth = 0.7) +
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
    coord_cartesian(ylim = y_range) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(x = NULL, y = "SWE ΔSnow [mm w.e.]") +
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
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
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


plot_swe_comparison_gap <- function(hs_series, swe_from_model, 
                                    station_name, year, max_swe, max_hs) {
  
  dates <- seq(as.Date(paste0(year-1, "-09-01")), 
               as.Date(paste0(year, "-08-31")), by = "day")
  
  df_hs <- data.frame(date = dates, value = hs_series)
  df_swe_model <- data.frame(date = dates, value = swe_from_model)
  
  y_range <- c(0, max_swe * 1.1)
  
  
  valid_idx <- which(!is.na(hs_series))
  if(length(valid_idx) > 0) {
    max_idx <- valid_idx[which.max(hs_series[valid_idx])]
    max_date <- dates[max_idx]
  } else {
    max_date <- dates[1]
  }
  

  df_hs <- find_gap_hs(df_hs)
  
  # Panel 1: HS con evidenziazione gap
  p1 <- ggplot(df_hs, aes(x = date)) +
    geom_rect(
      data = df_hs$gap_periods[[1]], 
      aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
      fill = "#C1FFC1", alpha = 0.3, color = NA, inherit.aes = FALSE
    ) +
    geom_area(aes(y = value), fill = "grey40", alpha = 0.2) +
    geom_line(aes(y = value), color = "grey40", linewidth = 0.7) +
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
    coord_cartesian(ylim = c(0, max_hs * 1.1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(title = paste0(station_name, " — ", year - 1, " to ", year), 
         x = NULL, y = "HS [cm]") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title.y = element_text(size = 14),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  # Panel 2: SWE from model
  p2 <- ggplot(df_swe_model, aes(x = date)) +
    geom_area(aes(y = value), fill = "#2171b5", alpha = 0.2) +
    geom_line(aes(y = value), color = "#2171b5", linewidth = 0.7) +
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
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
  
  p1 / plot_spacer() / p2
}


df_hs <- read.table(fname)
if(min(as.numeric(df_hs$V1), na.rm = TRUE) > 2023) {
  stop(paste0("No correct years for station ", name))
}

fname_swe <- paste0("../Dataset/model_runs/hydro/SNWD/", sub("HSD_", "DV_SDH_", name))
if(!file.exists(fname_swe)) {
  stop(paste0("No SWE series from model for ", name))
}
df_swe_model <- read.table(fname_swe)

df_max_vals <- read.table(fname_max_vals, header = TRUE)
mask_station <- df_max_vals$name == name
if(sum(mask_station, na.rm = TRUE) != 1) {
  stop(paste0("Problems with maximum scale values evaluation for ", name))
}
max_hs <- df_max_vals$max_hs[mask_station]
max_swe <- df_max_vals$max_swe[mask_station]



years <- unique(as.numeric(df_hs$V1)) 
for(i in seq_along(years)){
  
  y <- years[i]
  if(y %in% c(2024, 2025)){
    print(paste0("Station has ", y))
    next
  }
  cat("Processing year:", y, "\n")
  
  # Selezione dati per l'anno y
  mask_year <- as.numeric(df_hs$V1) == y
  hs_series <- as.numeric(df_hs$V2)[mask_year]
  has_gaps <- any(is.na(hs_series))
  
  mask_swe <- as.numeric(df_swe_model$V1) == y
  swe_from_model <- as.numeric(df_swe_model$V2)[mask_swe]
  

  if(has_gaps) {
    
    cat("  -> Series with gaps\n")
    
    suppressWarnings({
      p <- plot_swe_comparison_gap(
        hs_series = hs_series,
        swe_from_model = swe_from_model,
        station_name = name,
        year = y,
        max_swe = max_swe, 
        max_hs = max_hs
      )
      
      ggsave(paste0("Appo/", name, "_", y-1, "_to_", y, ".png"), 
             plot = p, width = 12, height = 10, dpi = 150)
    })
  }
  
  
  else {
    
    cat("  -> Complete series\n")
    hs_hydro <- round(hs_series) / 100
    
    if(hs_series[1] == 0) {
      dates <- seq(as.Date(paste0(y-1, "-09-01")), 
                   as.Date(paste0(y, "-08-31")), by = "day")
      hsdata <- data.frame(date = dates, hs = hs_hydro)
      swe_from_hs <- swe.delta.snow(hsdata, dyn_rho_max = FALSE)
    } else {
      hs_hydro <- c(0, hs_hydro)
      dates <- seq(as.Date(paste0(y-1, "-08-31")), 
                   as.Date(paste0(y, "-08-31")), by = "day")
      hsdata <- data.frame(date = dates, hs = hs_hydro)
      swe_from_hs <- swe.delta.snow(hsdata, dyn_rho_max = FALSE)[-1]
    }
    
    if(length(swe_from_model) != length(swe_from_hs)) {
      stop(paste0("Length mismatch for ", name, " during ", y))
    }
    
    # Plot
    suppressWarnings({
      p <- plot_swe_comparison_com(
        hs_series = hs_series,
        swe_from_hs = swe_from_hs,
        swe_from_model = swe_from_model,
        station_name = name,
        year = y,
        max_swe = max_swe, 
        max_hs = max_hs
      )
      
      ggsave(paste0("Appo/", name, "_", y-1, "_to_", y, ".png"), 
             plot = p, width = 12, height = 10, dpi = 150)
    })
  }
}

cat("\n✅ All plots created successfully!\n")