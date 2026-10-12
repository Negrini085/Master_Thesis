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

fname_ana <- "Original/STATION_check/ANAGRAFICA"
fname_max_vals <- "HS_correction/QC_series/Results/max_hs_swe_values.dat"
fname_with_gaps <- "HS_correction/Dataset/hs_series/with_gaps/with_gaps.dat"
fname_complete <- "HS_correction/Dataset/hs_series/all_complete/all_complete.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/")


# Function to split a dataframe into pre-season, season and post-season series for an incomplete hydro series
split_series_gap <- function(df, appo_gaps){
  n <- nrow(df)
  
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


# Function to create comparison plot between HS and SWE from model for an incomplete hydrological year
plot_hs_issues <- function(hs_first, station_names, years, max_hs_vals) {
  
  # Plotting first HS series
  dates <- seq(as.Date(paste0(years[1]-1, "-09-01")), as.Date(paste0(years[1], "-08-31")), by = "day")
  df_hs <- data.frame(date = dates, value = hs_first)
  df_hs <- split_series_gap(df_hs, as.numeric(df_hs$value))

  p1 <- ggplot(df_hs, aes(x = date)) +
    geom_area(aes(y = value), fill = "grey40", alpha = 0.2) +
    geom_line(aes(y = value), color = "grey40", linewidth = 0.7) +
    geom_rect(
      data = df_hs$gap_periods[[1]], 
      aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
      fill = "#C1FFC1", alpha = 1, color = NA, inherit.aes = FALSE
    ) +
    coord_cartesian(ylim = c(0, 380)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(title = paste0(station_names[1], "  -  ", years[1] - 1, " to ", years[1]), x = NULL, y = "HS [cm]") +
    theme_minimal() +
    theme(
      plot.title   = element_text(face = "bold"),
      axis.title.y = element_text(size = 20),
      axis.text.y  = element_text(size = 15),
      axis.text.x = element_text(size = 15)
    )
  
  p1
}





# Importing complete and with gaps metadatas
df_complete <- read.table(fname_complete)
df_complete <- data.frame(name = df_complete$V1, year = as.numeric(df_complete$V2), status = rep("com", length(df_complete$V1)))

df_with_gaps <- read.table(fname_with_gaps)
df_with_gaps <- data.frame(name = df_with_gaps$V1, year = as.numeric(df_with_gaps$V2), status = rep("gap", length(df_with_gaps$V1)))

df_max_vals <- read.table(fname_max_vals, header = TRUE)



# Selecting HS series
mask <-  df_with_gaps$name == "HSD_TAA_TN_VERMIGLIO_CAPANNA_PRESENA_MAN"
station_names <- unique(df_with_gaps$name[mask])
years <- 2008



hs_to_print <- list()
max_hs_vals <- numeric(0)
for(i in seq_along(station_names)){
  
  # Importing hs series for a given year
  fname <- paste0("Original/STATION_series/Dataset/station_series/", station_names[i])
  df_hs <- read.table(fname)
  
  # Selecting given year
  mask <- as.numeric(df_hs$V1) == years[i]
  hs_to_print[[i]] <- as.numeric(df_hs$V2[mask])

  # Selecting max HS value
  mask <- df_max_vals$name == station_names[i]
  max_hs_vals <- c(max_hs_vals, df_max_vals$max_hs[mask])
}


plot_hs_issues(hs_to_print[[1]], station_names, years, max_hs_vals)