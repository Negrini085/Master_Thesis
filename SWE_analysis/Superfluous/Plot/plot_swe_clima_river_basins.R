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


# Building the 4 subplots
plots <- lapply(seq_along(basins), function(k) {
  plot_swe_clima(clima_list[[k]],
                 title        = basins[[k]],
                 show_x       = k < 2,
                 show_y_title = k %% 2 == 1,
                 show_y_text  = !same_y || k %% 2 == 1,
                 ylim         = ylim_common)
})

p <- wrap_plots(plots, nrow = 2, ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(p)









# Legenda "a mano": 3 voci sopra, 2 sotto centrate
periods <- levels(clima_list[[1]]$period)
n  <- length(periods)
n1 <- ceiling(n / 2)          # voci nella prima riga
n2 <- n - n1                  # voci nella seconda riga

dx <- 1.3   # distanza fra voci adiacenti  -> aumenta per allargarle
dy <- 1.5   # distanza fra le due righe    -> aumenta per staccarle

leg_df <- data.frame(
  period = factor(periods, levels = periods),
  x = c(seq_len(n1), seq_len(n2) + (n1 - n2) / 2) * dx,
  y = c(rep(dy, n1), rep(0, n2))
)
xc <- mean(range(leg_df$x)) + 0.3   # centro della legenda

legend_plot <- ggplot(leg_df, aes(colour = period)) +
  geom_segment(aes(x = x - 0.25, xend = x - 0.05, y = y, yend = y), linewidth = 1.5) +
  geom_point(aes(x = x - 0.15, y = y), size = 3) +
  geom_text(aes(x = x, y = y, label = period), hjust = 0, size = 15 / .pt, colour = "black") +
  scale_colour_viridis_d(option = "plasma", end = 0.92, guide = "none") +
  coord_cartesian(xlim = xc + c(-2, 2), ylim = c(-0.6, 2.1), clip = "off") +
  theme_void()

# Singolo plot
p1 <- (plots[[1]] + theme(legend.position = "none")) / legend_plot +
  plot_layout(heights = c(1, 0.15))
print(p1)