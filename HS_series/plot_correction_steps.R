# The main goal of this script is to make plots of correction procedure steps.
rm(list = ls())
gc()

library(dplyr)
library(ggplot2)
library(nixmass)
library(patchwork)

fname <- "HS_correction/QC_series/Dataset/appo.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/")

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



plot_swe_comparison_gap <- function(hs_series, station_name, year) {
  
  dates <- seq(as.Date(paste0(year-1, "-09-01")), as.Date(paste0(year, "-08-31")), by = "day")
  df_hs <- data.frame(date = dates, value = hs_series)

  
  valid_idx <- which(!is.na(hs_series))
  if(length(valid_idx) > 0) {
    max_idx <- valid_idx[which.max(hs_series[valid_idx])]
    max_date <- dates[max_idx]
  } else {
    max_date <- dates[1]
  }
  
  
  df_hs <- find_gap_hs(df_hs)
  max_hs <- hs_series[max_idx]
  
  # Panel 1: HS con evidenziazione gap
  p1 <- ggplot(df_hs, aes(x = date)) +
    geom_area(aes(y = value), fill = "grey40", alpha = 0.2) +
    geom_line(aes(y = value), color = "grey40", linewidth = 0.7) +
    geom_rect(
      data = df_hs$gap_periods[[1]], 
      aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
      fill = "#C1FFC1", alpha = 1, color = NA, inherit.aes = FALSE
    ) +
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
    scale_y_continuous(breaks = c(0, 50, 100)) +
    coord_cartesian(ylim = c(0, max_hs * 1.1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(title = NULL, x = NULL, y = "HS [cm]") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title.y = element_text(size = 20),
      axis.text.y = element_text(size = 15),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  p1
}



plot_swe_comparison_gap_tit <- function(hs_series, station_name, year) {
  
  dates <- seq(as.Date(paste0(year-1, "-09-01")), as.Date(paste0(year, "-08-31")), by = "day")
  df_hs <- data.frame(date = dates, value = hs_series)
  
  
  valid_idx <- which(!is.na(hs_series))
  if(length(valid_idx) > 0) {
    max_idx <- valid_idx[which.max(hs_series[valid_idx])]
    max_date <- dates[max_idx]
  } else {
    max_date <- dates[1]
  }
  
  
  df_hs <- find_gap_hs(df_hs)
  max_hs <- hs_series[max_idx]
  
  # Panel 1: HS con evidenziazione gap
  p1 <- ggplot(df_hs, aes(x = date)) +
    geom_area(aes(y = value), fill = "grey40", alpha = 0.2) +
    geom_line(aes(y = value), color = "grey40", linewidth = 0.7) +
    geom_rect(
      data = df_hs$gap_periods[[1]], 
      aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
      fill = "#C1FFC1", alpha = 1, color = NA, inherit.aes = FALSE
    ) +
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
    scale_y_continuous(breaks = c(0, 50, 100)) +
    coord_cartesian(ylim = c(0, max_hs * 1.1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(title = paste0(station_name, " — ", year - 1, " to ", year), x = NULL, y = "HS [cm]") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.y = element_text(size = 15),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  p1
}



plot_swe_comparison_gap_mon <- function(hs_series, station_name, year) {
  
  dates <- seq(as.Date(paste0(year-1, "-09-01")), as.Date(paste0(year, "-08-31")), by = "day")
  df_hs <- data.frame(date = dates, value = hs_series)
  
  
  valid_idx <- which(!is.na(hs_series))
  if(length(valid_idx) > 0) {
    max_idx <- valid_idx[which.max(hs_series[valid_idx])]
    max_date <- dates[max_idx]
  } else {
    max_date <- dates[1]
  }
  
  
  df_hs <- find_gap_hs(df_hs)
  max_hs <- hs_series[max_idx]
  
  # Panel 1: HS con evidenziazione gap
  p1 <- ggplot(df_hs, aes(x = date)) +
    geom_area(aes(y = value), fill = "grey40", alpha = 0.2) +
    geom_line(aes(y = value), color = "grey40", linewidth = 0.7) +
    geom_rect(
      data = df_hs$gap_periods[[1]], 
      aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
      fill = "#C1FFC1", alpha = 1, color = NA, inherit.aes = FALSE
    ) +
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
    scale_y_continuous(breaks = c(0, 50, 100)) +
    coord_cartesian(ylim = c(0, max_hs * 1.1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(title = NULL, x = NULL, y = "HS [cm]") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title.y = element_text(size = 20),
      axis.text.y = element_text(size = 15),
      axis.text.x = element_text(size = 15)
      )
  
  p1
}










# Importing HS series
p <- list()
df_to_plot <- read.table(fname, header = FALSE)
series_ixd <- unique(as.numeric(df_to_plot$V1))
for(i in series_ixd){
  mask <- as.numeric(df_to_plot$V1) %in% series_ixd[i]
  hs_series <- as.numeric(df_to_plot$V2)[mask]
  if(i == 1)  p[[i]] <- plot_swe_comparison_gap_tit(hs_series, "HSD_TAA_TN_PASSO_PIAN_DELLE_FUGAZZE", 2014)
  else if(i == 4)   p[[i]] <- plot_swe_comparison_gap_mon(hs_series, "HSD_TAA_TN_PASSO_PIAN_DELLE_FUGAZZE", 2014)
  else   p[[i]] <- plot_swe_comparison_gap(hs_series, "HSD_TAA_TN_PASSO_PIAN_DELLE_FUGAZZE", 2014)
}


p <- (p[[1]] / p[[2]] / p[[3]] / p[[4]]) +
  plot_layout(axis_titles = "collect_y") +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(size = 18, face = "bold"),
    plot.tag.position = c(0.99, 0.98)
  )

print(p)