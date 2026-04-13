# The main goal of this script is to inspect DeltaSnow capabilities in order to 
# understand the SWE conversion I saw earlier
rm(list = ls())
gc

library(ggplot2)
library(nixmass)
library(patchwork)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/QC_series/")

# Function to create comparison plot between HS, SWE from DeltaSnow and SWE from model
plot_swe_comparison <- function(hsdata, swe_sta) {
  
  dates <- hsdata$date
  
  # Build dataframes
  df_hs <- data.frame(date = dates, value = hsdata$hs*100)
  df_swe_hs <- data.frame(date = dates, value = swe_sta)
  
  # Common y-range for SWE plots
  y_range <- range(c(swe_sta), na.rm = TRUE)
  y_range[1] <- 0
  
  # 🔴 Find date of maximum HS
  max_idx <- which.max(hsdata$hs)
  max_date <- dates[max_idx]
  
  # Panel 1: HS
  p1 <- ggplot(df_hs, aes(x = date, y = value)) +
    geom_line(color = "grey40") +
    geom_area(alpha = 0.2, fill = "grey40") +
    geom_vline(xintercept = max_date, linetype = "dashed", color = "red") +
    labs(
      title = "Comparison between methods",
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
  p2 <- ggplot(df_swe_hs, aes(x = date, y = value)) +
    geom_line(color = "#2171b5") +
    geom_area(alpha = 0.2, fill = "#2171b5") +
    coord_cartesian(ylim = y_range) +
    labs(
      x = NULL,
      y = "SWE DeltaSnow [mm w.e.]"
    ) +
    theme_minimal() +
    theme(
      axis.title.y = element_text(size = 14),
      axis.text.y  = element_text(size = 12),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  # Combine plots
  p1 / p2
}


# Importing station names and years to be taken care of
hs_hydro <- c(0, 100, 60, 160, 102)
dates <- seq(as.Date(paste0("2011-09-01")), as.Date(paste0("2011-09-05")), by = "day")
hsdata <- data.frame(date = dates, hs = hs_hydro/100)


# SWE series
swe1 <- swe.delta.snow(hsdata, dyn_rho_max = FALSE)
print(swe1)
p <- plot_swe_comparison(hsdata = hsdata, swe_sta = swe1)
print(p)


result <- swe.delta.snow(hsdata, dyn_rho_max = FALSE, layers = TRUE)
print(result$h[1, ])
print(result$h[2, ])