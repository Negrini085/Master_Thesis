# The main goal of this script is to plot HS series coupled with model SWE series
# in order to make a quality check because I found out that some corrections are
# not as good as they seemed.
rm(list = ls())
gc()

library(dplyr)
library(ggplot2)
library(patchwork)

fname_ana <- "../STATION_check/Dataset/ANAGRAFICA"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/Correct/QC_series/")


# Function to identify zeros
find_zero_hs <- function(df, tol = 0) {
  n <- nrow(df)
  
  zero_flag <- !is.na(df$value) & df$value <= tol
  df$zero_flag <- zero_flag
  
  df$next_date <- c(df$date[-1], df$date[n] + 1)
  
  first_zero <- if(is.na(df$zero_flag[1])) FALSE else df$zero_flag[1]
  df$group_id <- cumsum(df$zero_flag != dplyr::lag(df$zero_flag, default = !first_zero))
  
  zero_periods <- df %>%
    filter(zero_flag) %>%
    group_by(group_id) %>%
    summarise(
      start_date = min(date),
      end_date   = max(next_date),
      .groups    = 'drop'
    )
  
  df$zero_periods <- list(zero_periods)
  
  return(df)
}

# Function to plot HS series
plot_hs_comparison <- function(new_hs, swe_from_model, station_name, year, max_swe, max_hs) {
  
  dates <- seq(as.Date(paste0(year-1, "-09-01")), as.Date(paste0(year, "-08-31")), by = "day")
  if(length(dates) != length(new_hs)) stop(paste0("No compatible length for ", station_name, " in ", year))
  if(length(new_hs) != length(swe_from_model)) stop(paste0("No compatible length for ", station_name, " in ", year))
  
  df_new_hs <- find_zero_hs(data.frame(date = dates, value = new_hs))
  df_swe_model <- data.frame(date = dates, value = swe_from_model)
  y_range <- c(0, max_swe * 1.1)
  
  
  # Panel 1: HS
  p1 <- ggplot(df_new_hs, aes(x = date)) +
    geom_rect(
      data = df_new_hs$zero_periods[[1]],
      aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
      fill  = "#FFB6C1",
      alpha = 0.4,
      color = NA,
      inherit.aes = FALSE
    ) +
    geom_area(aes(y = value), fill = "grey40", alpha = 0.2) +
    geom_line(aes(y = value), color = "grey40", linewidth = 0.7) +
    coord_cartesian(ylim = c(0, max_hs * 1.1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(title = paste0(station_name, " — ", year - 1, " to ", year),
         x = NULL, y = "HS new [cm]") +
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





# Importing station names. I don't want to deal with austrian aws because of data quality
df <- read.table(fname_ana, header = TRUE)
station_names <- df$station_name
station_names <- station_names[!grepl("^HSD_AT", station_names)]
station_names <- station_names[!grepl("^HSD_IT", station_names)]
station_names <- station_names[!grepl("^HSD_LOM", station_names)]
station_names <- station_names[!grepl("^HSD_TAA", station_names)]
station_names <- sort(station_names)



# Cycle over stations
for(name in station_names){
  
  # Importing HS and SWE series
  fname <- paste0("../Dataset/", name)
  df_new_hs <- read.table(fname)
  
  fname_swe <- paste0("../../HS_correction/Dataset/model_runs/hydro/SNWD/", sub("HSD_", "DV_SDH_", name))
  if(!file.exists(fname_swe)) stop(paste0("No SWE series from model for ", name))
  df_swe_model <- read.table(fname_swe)
  
  # Finding scale max
  max_hs <- max(as.numeric(df_new_hs$V2), na.rm = TRUE)
  max_swe <- max(as.numeric(df_swe_model$V2), na.rm = TRUE)

  cat("\n", "\n", "\n")
  cat("Station: ", name, "\n")
  cat("\n")
  
  
  
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
    
    mask_swe <- as.numeric(df_swe_model$V1) == y
    swe_from_model <- as.numeric(df_swe_model$V2)[mask_swe]
    
    
    # Plot
    suppressWarnings({
      p <- plot_hs_comparison(
        new_hs = new_hs,
        swe_from_model = swe_from_model,
        station_name = name,
        year = y,
        max_swe = max_swe, 
        max_hs = max_hs
      )
      
      ggsave(paste0("Pdf/", name, "_", y-1, "_to_", y, ".pdf"), 
             plot = p, width = 12, height = 10, device = cairo_pdf)
    })
  }
}