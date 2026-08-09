# The main goal of this script is to plot the loss function value as a function 
# of the accepted move.
rm(list = ls())
gc()

library(ggplot2)

fname <- "Runs/First/loss_values.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Calibration/")

df <- read.table(fname, header = FALSE)
value <- df$V1

plot_df <- data.frame(
  AcceptedMove = seq_along(value),
  LossValue = value
)

ggplot(plot_df, aes(x = AcceptedMove, y = LossValue)) +
  geom_line(linewidth = 0.9, color = "#9E27F4") +
  # geom_point(size = 1.2, color = "#1F77B4", alpha = 0.7) +
  labs(
    title = "Evolution of the Loss Function",
    subtitle = "Simulated Annealing Accepted Moves",
    x = "Accepted Move",
    y = "Loss Function Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )