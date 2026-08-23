# The main goal of this script is to plot the loss function value as a function 
# of the accepted move.
rm(list = ls())
gc()

library(ggplot2)
library(tidyr) 

fname_one <- "Runs/loss_evo_1.dat"
fname_two <- "Runs/loss_evo_2.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_calibration/")



# Importing dataframe
df_one <- read.table(fname_one, header = FALSE)
val_one <- as.numeric(df_one$V1)

df_two <- read.table(fname_two, header = FALSE)
val_two <- as.numeric(df_two$V1)



# Plotting procedure
plot_df <- data.frame(
  step = seq_along(val_one),
  val_one = val_one,
  val_two = val_two
)

plot_df_long <- pivot_longer(
  plot_df,
  cols = c(val_one, val_two),
  names_to = "Run",
  values_to = "LossValue"
)

plot_df_long$Run <- factor(
  plot_df_long$Run,
  levels = c("val_one", "val_two"),
  labels = c("Run 1", "Run 2")
)

ggplot(plot_df_long, aes(x = step, y = LossValue, color = Run)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c("Run 1" = "#9E27F4", "Run 2" = "#1F77B4")) +
  labs(
    title = "Evolution of the Loss Function",
    subtitle = "Simulated Annealing Accepted Moves",
    x = "Accepted Move",
    y = "Loss Function Value",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )