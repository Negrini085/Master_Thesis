# The main goal of this script is to plot the evolution of performance metrics 
# during the simulated annealing procedure.
rm(list = ls())
gc()

library(ggplot2)
library(tidyr)
library(dplyr)

fnames <- sprintf("Runs/ddf_param/loss_evo_%d.dat", 1:5)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_calibration/")


# Importing dataframes (one per run) and stacking them
read_run <- function(i) {
  df <- read.table(fnames[i], header = FALSE)
  data.frame(
    AcceptedMove = seq_len(nrow(df)),
    csi          = 1 - as.numeric(df$V1),
    freq_bias    = as.numeric(df$V2),
    hit_rate     = as.numeric(df$V3),
    accuracy     = as.numeric(df$V4),
    Run          = paste("Run", i)
  )
}

plot_df <- do.call(rbind, lapply(seq_along(fnames), read_run))

plot_df_long <- plot_df %>%
  tidyr::pivot_longer(
    cols      = c(csi, freq_bias, hit_rate, accuracy),
    names_to  = "Performance",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Performance = factor(
      Performance,
      levels = c("csi", "freq_bias", "hit_rate", "accuracy"),
      labels = c("CSI", "Frequency Bias", "Recall", "Accuracy")
    ),
    Run = factor(Run, levels = paste("Run", seq_along(fnames)))
  )


pal <- c(
  "Run 1" = "#004488", 
  "Run 2" = "#DDAA33", 
  "Run 3" = "#BB5566", 
  "Run 4" = "#44AA99", 
  "Run 5" = "#AA4499"
  )


# Plotting procedure
p_loss <- ggplot(plot_df_long, aes(x = AcceptedMove, y = Value, colour = Run)) +
  geom_line(
    linewidth = 0.9,
    lineend   = "round"
  ) +
  facet_wrap(
    ~ Performance,
    ncol   = 1,
    scales = "free_y"
  ) +
  scale_colour_manual(values = pal) +
  scale_x_continuous(
    expand = expansion(mult = c(0.005, 0.01))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.04, 0.06))
  ) +
  labs(
    x      = "Accepted Move",
    y      = "Performance metric",
    colour = NULL
  ) +
  guides(
    colour = guide_legend(nrow = 1, override.aes = list(linewidth = 2))
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(
      family = "sans",
      colour = "black"
    ),
    axis.title = element_text(
      size = 20,
      face = "plain"
    ),
    axis.title.x = element_text(
      margin = margin(t = 12)
    ),
    axis.title.y = element_text(
      margin = margin(r = 12)
    ),
    axis.text = element_text(
      size   = 15,
      colour = "black"
    ),
    axis.line = element_line(
      linewidth = 0.6,
      colour    = "black"
    ),
    axis.ticks = element_line(
      linewidth = 0.5,
      colour    = "black"
    ),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major.x = element_line(
      colour    = "grey80",
      linewidth = 0.35
    ),
    panel.grid.major.y = element_line(
      colour    = "grey80",
      linewidth = 0.35
    ),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.4, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(
      size   = 17,
      face   = "bold",
      hjust  = 0,
      margin = margin(b = 6)
    ),
    legend.position   = "top",
    legend.direction  = "horizontal",
    legend.text       = element_text(size = 16),
    legend.key.width  = unit(1.6, "cm"),
    legend.margin     = margin(b = 8),
    plot.margin = margin(
      t = 5,
      r = 10,
      b = 5,
      l = 5
    )
  )

print(p_loss)