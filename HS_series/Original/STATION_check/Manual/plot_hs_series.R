# The main goal of this script is to make hs series plots in order to asses data 
# quality, because I think there there are some faulty datas. 
rm(list = ls())
gc()

library(dplyr)
library(ggplot2)
library(lubridate)

fname_hydro <- "Dataset/hydro/hydro_stations.dat"
fname_ana <- "Dataset/solar/ANAGRAFICA_MANUALE_TOT"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Ours/STATION_check/Manual/")



# Function to get gaps
get_gap_periods <- function(date, value) {
  stopifnot(length(date) == length(value))
  
  is_gap <- is.na(value)
  
  if (!any(is_gap)) {
    return(data.frame(
      start_date = as.Date(character()),
      end_date   = as.Date(character()),
      n_days     = integer()
    ))
  }
  
  r <- rle(is_gap)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  
  gap_id <- which(r$values)
  
  data.frame(
    start_date = date[starts[gap_id]],
    end_date   = date[ends[gap_id]] + 1,
    n_days     = r$lengths[gap_id],
    row.names  = NULL
  )
}



# Function to interpolate in between gaps
interpolate_series <- function(x) {
  idx <- which(!is.na(x))
  
  if (length(idx) == 0) return(rep(NA, length(x)))
  if (length(idx) == 1) return(rep(x[idx], length(x)))
  
  approx(
    x = idx,
    y = x[idx],
    xout = seq_along(x),
    rule = 2
  )$y
}



# Function to plot HS series
plot_hs_year <- function(df_hs, station_name, ele, year, max_hs = max_hs, x_breaks = "1 month", x_labels = "%b") {
  
  stopifnot(all(c("date", "hs_hydro") %in% names(df_hs)))
  df_hs <- df_hs[order(df_hs$date), ]
  df_hs$hs_interp <- interpolate_series(df_hs$hs_hydro)
  max_date <- df_hs$date[which.max(df_hs$hs_hydro)]
  
  gap_periods <- get_gap_periods(df_hs$date, df_hs$hs_hydro)
  p1 <- ggplot(df_hs, aes(x = date)) +
    geom_area(aes(y = hs_interp), fill = "grey40", alpha = 0.2, na.rm = TRUE) +
    geom_line(aes(y = hs_interp), color = "grey40", linewidth = 0.7, na.rm = TRUE) +
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
    {if (nrow(gap_periods) > 0)
      geom_rect(
        data = gap_periods,
        aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
        fill = "#C1FFC1", alpha = 1, color = NA, inherit.aes = FALSE
      )
    } +
    coord_cartesian(ylim = c(0, max_hs * 1.1)) +
    scale_x_date(date_breaks = x_breaks, date_labels = x_labels) +
    labs(
      title = paste0(station_name, " (", ele, " m a.s.l.) — ", year - 1, " to ", year),
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
  
  return(p1)
}









# Importing station names and metadatas
df_hydro <- read.table(fname_hydro)
station_names <- df_hydro$V1

df_ana <- read.table(fname_ana)
mask <- df_ana$V1 %in% station_names
df_ana <- df_ana[mask, ]

# Cycle over stations
for(name in station_names){
  
  # Selecting elevation
  mask <- df_ana$V1 == name
  ele <- as.numeric(df_ana$V4)[mask]
  if(length(ele) != 1) stop(paste0("Elevation is not of expected lenght for ", name))
  
  # Importing hs series for a given station
  df <- read.table(paste0("Dataset/hydro/", name))
  years <- as.numeric(df$V1)
  hs <- as.numeric(df$V2)
  max_hs <- max(hs, na.rm = TRUE)
  
  # Cycle over station years 
  for(y in unique(years)){
    
    # Datas to plot
    mask <- years == y
    hs_hydro <- hs[mask]
    dates <- seq(as.Date(paste0(y-1, "-09-01")), as.Date(paste0(y, "-08-31")), by = "day")
    
    if(length(hs_hydro) != length(dates)) stop(paste0("Dates and hs series not compatible in length for ", name, " during ", y))
    df_hs <- data.frame(date = dates, hs_hydro = hs_hydro)
    
    p <- plot_hs_year(df_hs = df_hs, station_name = name, year = y, ele = ele, max_hs = max_hs)
    ggsave(paste0("Images/", name, "_", y-1,"_to_", y, ".png"), plot = p, width = 12, height = 10, dpi = 150)
    print(paste0("Made plot for ", name, " (", ele, " m)  -  ", y-1," to ", y))
  }
}