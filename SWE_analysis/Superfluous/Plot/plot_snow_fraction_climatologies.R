# The main goal of this script is to compute snow cover climatologies, in order to 
# show how much the snow line rose during the 1951-2023 period.
rm(list = ls())
gc()

library(ggplot2)
library(patchwork)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")


# Function to convert elevation band labels ("<=50", "50-100", ...) into band midpoints [m]
band_midpoint <- function(labels) {
  
  upper <- as.numeric(sub("^.*(<=|-)", "", labels))
  lower <- rep(0, length(labels))
  is_range <- !startsWith(labels, "<=")
  lower[is_range] <- as.numeric(sub("-.*$", "", labels[is_range]))
  
  (lower + upper) / 2
}


# Function to read snow fraction climatologies and reshape them in long format
read_snow_fraction <- function(date_tag, dir = "Results/SCD/") {
  
  fname <- file.path(dir, paste0("snow_fraction_climatologies_", date_tag, ".dat"))
  df <- read.table(fname, header = TRUE, sep = "\t", row.names = 1, check.names = FALSE)
  
  periods <- rownames(df)
  out <- data.frame(elevation = rep(band_midpoint(colnames(df)), each = nrow(df)),
                    period    = factor(rep(periods, times = ncol(df)), levels = periods),
                    snow_frac = 100 * unlist(df, use.names = FALSE))
  
  out[!is.na(out$snow_frac), ]
}


# Function to plot snow fraction data (y axis always 0-100 %, y labels shown only if show_y)
plot_snow_fraction <- function(
    data, title = NULL, show_x = TRUE, show_y = TRUE, xlim = NULL,
    size_title = 20, size_axis_title = 20, size_ticks = 15) {
  
  ggplot(data, aes(x = elevation, y = snow_frac, colour = period, group = period)) +
    geom_line(linewidth = 1.2) +
    scale_colour_viridis_d(option = "plasma", end = 0.92, name = NULL) +
    scale_x_continuous(breaks = seq(0, 4000, 500), expand = expansion(mult = c(0.01, 0.02))) +
    scale_y_continuous(breaks = seq(0, 100, 20), expand = expansion(mult = c(0.02, 0.05))) +
    coord_cartesian(xlim = xlim, ylim = c(0, 100)) + 
    labs(title = title,
         x = if (show_x) "Elevation [m a.s.l.]" else NULL,
         y = if (show_y) "Snow cover fraction [%]" else NULL) +
    theme_minimal(base_size = 15) +
    theme(plot.title          = element_text(face = "plain", size = size_title),
          axis.title.x        = element_text(size = size_axis_title),
          axis.title.y        = element_text(size = size_axis_title),
          panel.grid.minor    = element_blank(),
          panel.grid.major    = element_line(colour = "grey90", linewidth = 0.4),
          axis.line.x         = element_line(colour = "black", linewidth = 0.4),
          axis.ticks.x        = element_line(colour = "black", linewidth = 0.4),
          axis.ticks.length.x = unit(4, "pt"),
          axis.text.x         = if (show_x) element_text(size = size_ticks) else element_blank(),
          axis.text.y         = if (show_y) element_text(size = size_ticks) else element_blank(),
          legend.position     = "bottom",
          legend.text         = element_text(size = 15))
}




# Selecting dates to plot (file tag = panel title)
dates <- c("dec" = "December", "jan" = "January", "feb" = "February", "mar" = "March")


# Reading data (same DEM for all dates -> common elevation range)
snow_list   <- lapply(names(dates), read_snow_fraction)
xlim_common <- c(0, max(sapply(snow_list, function(d) max(d$elevation))))


# Building the 4 subplots (x labels on bottom row, y labels on first column only)
plots <- lapply(seq_along(dates), function(k) {
  plot_snow_fraction(snow_list[[k]],
                     title  = dates[[k]],
                     show_x = k > 2,
                     show_y = k %% 2 == 1,
                     xlim   = xlim_common)
})

p <- wrap_plots(plots, nrow = 2, ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(p)

# Optional: saving the figure
# ggsave("Figures/snow_fraction_elevation.pdf", p, width = 14, height = 10)