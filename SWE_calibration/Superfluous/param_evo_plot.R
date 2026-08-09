# The main goal of this script is to plot the evolution of model parameters 
# during the simulated annealing procedure.
rm(list = ls())
gc()

library(ggplot2)
library(tidyr)

fname <- "Runs/First/explored_params.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Calibration/")

df <- read.table(fname, header = FALSE)

plot_df <- data.frame(
  AcceptedMove = seq_len(nrow(df)),
  t_th = as.numeric(df$V1),
  ddfm = as.numeric(df$V2),
  ddfM = as.numeric(df$V3)
)

plot_df_long <- pivot_longer(
  plot_df,
  cols = c(t_th, ddfm, ddfM),
  names_to = "Parameter",
  values_to = "Value"
)

ggplot(plot_df_long, aes(x = AcceptedMove, y = Value)) +
  geom_line(linewidth = 0.9, color = "#9E27F4") +
  # geom_point(size = 1.2, alpha = 0.7, color = "#1F77B4") +
  facet_wrap(
    ~Parameter, ncol = 1, scales = "free_y",
    labeller = as_labeller(c(
      t_th = "Threshold Temperature",
      ddfm = "DDF min",
      ddfM = "DDF max"
    ))
  ) +
  labs(
    title = "Evolution of Model Parameters",
    subtitle = "Simulated Annealing Accepted Moves",
    x = "Accepted Move",
    y = "Parameter Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
