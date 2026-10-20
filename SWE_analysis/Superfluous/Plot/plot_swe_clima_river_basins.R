# The main goal of this script is to plot SWE volume climatologies basin-wise
rm(list = ls())
gc()

library(ggplot2)
library(patchwork)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")


# Function to read and order SWE climatologies
read_clima <- function(river_name, dir = "Results/SWE") {
  
  fname <- file.path(dir, paste0("clima_", river_name, "_basin.dat"))
  df <- read.table(fname, header = TRUE, check.names = FALSE)
  
  hydro_months <- month.abb[c(10:12, 1:9)]
  df <- df[hydro_months, , drop = FALSE]
  periods <- sub("_", "-", names(df))                 # "1952_1981" -> "1952-1981"
  
  data.frame(month  = factor(rep(hydro_months, times = ncol(df)), levels = hydro_months),
             period = factor(rep(periods, each = nrow(df)), levels = periods),
             swe    = unlist(df, use.names = FALSE))
}


# Function to plot SWE data
plot_swe_clima <- function(
    data, title = NULL, show_x = TRUE, show_y_title = TRUE, show_y_text = TRUE, ylim = NULL,
    size_title = 20, size_axis_title = 20, size_ticks = 15) {
  
  p <- ggplot(data, aes(x = month, y = swe, colour = period, group = period)) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 3.0) +
    scale_colour_viridis_d(option = "plasma", end = 0.92, name = NULL) +
    scale_y_continuous(limits = ylim, expand = expansion(mult = c(0.02, 0.05))) +
    labs(title = title, x = NULL,
         y = if (show_y_title) expression("SWE [Gm"^3*"]") else NULL) +
    theme_minimal(base_size = 15) +
    theme(plot.title          = element_text(face = "plain", size = size_title),
          axis.title.y        = element_text(size = size_axis_title),
          panel.grid.minor    = element_blank(),
          panel.grid.major    = element_line(colour = "grey90", linewidth = 0.4),
          axis.line.x         = element_line(colour = "black", linewidth = 0.4),
          axis.ticks.x        = element_line(colour = "black", linewidth = 0.4),
          axis.ticks.length.x = unit(4, "pt"),
          axis.text.x         = if (show_x) element_text(size = size_ticks, angle = 45,
                                                         hjust = 1, vjust = 1)
          else element_blank(),
          axis.text.y         = if (show_y_text) element_text(size = size_ticks)
          else element_blank(),
          legend.position     = "bottom",
          legend.text         = element_text(size = 15))
  
  if (is.null(ylim)) p <- p + expand_limits(y = 0)
  p
}




# Selecting basin to plot (and same y or not)
basins <- c(po = "Po", adige  = "Adige", brenta = "Brenta", piave_tagliamento  = "Piave & Tagliamento")
same_y   <- FALSE  # TRUE -> same y scale on all subplots


# Reading data
clima_list <- lapply(names(basins), read_clima)
ylim_common <- NULL
if(same_y) ylim_common <- c(0, max(sapply(clima_list, function(d) max(d$swe, na.rm = TRUE))))


seasons <- years[-1]
swe_season <- array(NA_real_, dim = c(365, n_bands, length(seasons)), dimnames = list(day_hydro = 1:365, band = band_labels, season = seasons))
for (k in seq_along(seasons)) {
  swe_season[1:92,   , k] <- swe_band_mean[274:365, , k]
  swe_season[93:365, , k] <- swe_band_mean[1:273,   , k + 1]
}




# Building the 4 subplots
plots <- lapply(seq_along(basins), function(k) {
  plot_swe_clima(clima_list[[k]],
                 title        = basins[[k]],
                 show_x       = k > 2,
                 show_y_title = k %% 2 == 1,
                 show_y_text  = !same_y || k %% 2 == 1,
                 ylim         = ylim_common)
})

p <- wrap_plots(plots, nrow = 2, ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(p)