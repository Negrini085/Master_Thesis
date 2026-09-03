# The main goal of this script is to plot the evolution of model parameters 
# during the simulated annealing procedure.
rm(list = ls())
gc()

library(ggplot2)
library(tidyr)

fname_one <- "Runs/Open/param_evo_1.dat"
# fname_two <- "Runs/Restricted/param_evo_2.dat"
# fname_tre <- "Runs/Restricted/param_evo_3.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_calibration/")



# Importing dataframes
df_one <- read.table(fname_one, header = FALSE)
# df_two <- read.table(fname_two, header = FALSE)
# df_tre <- read.table(fname_tre, header = FALSE)

# df_one <- df_one[200:nrow(df_one), ]
# df_two <- df_two[200:nrow(df_two), ]



# Plotting procedure
plot_df_one <- data.frame(
  AcceptedMove = seq_len(nrow(df_one)),
  t_th = as.numeric(df_one$V1),
  ddfm = as.numeric(df_one$V2),
  ddfM = as.numeric(df_one$V3),
  expfact = as.numeric(df_one$V4),
  Run = "Run 1"
)

# plot_df_two <- data.frame(
#   AcceptedMove = seq_len(nrow(df_two)),
#   t_th = as.numeric(df_two$V1),
#   ddfm = as.numeric(df_two$V2),
#   ddfM = as.numeric(df_two$V3),
#   expfact = as.numeric(df_two$V4),
#   Run = "Run 2"
# )
# 
# plot_df_tre <- data.frame(
#   AcceptedMove = seq_len(nrow(df_tre)),
#   t_th = as.numeric(df_tre$V1),
#   ddfm = as.numeric(df_tre$V2),
#   ddfM = as.numeric(df_tre$V3),
#   expfact = as.numeric(df_tre$V4),
#   Run = "Run 3"
# )

plot_df <- rbind(plot_df_one)#, plot_df_two, plot_df_tre)

plot_df_long <- pivot_longer(
  plot_df,
  cols = c(t_th, ddfm, ddfM, expfact),
  names_to = "Parameter",
  values_to = "Value"
)


ggplot(plot_df_long, aes(x = AcceptedMove, y = Value, color = Run)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c("Run 1" = "#004488")) +#, "Run 2" = "#DDAA33", "Run 3" = "#BB5566")) +
  facet_wrap(
    ~Parameter, ncol = 1, scales = "free_y",
    labeller = as_labeller(c(
      t_th = "Threshold Temperature",
      ddfm = "DDF min",
      ddfM = "DDF max", 
      expfact = "Expfact"
    ))
  ) +
  labs(
    title = "Evolution of Model Parameters",
    subtitle = "Simulated Annealing Accepted Moves",
    x = "Accepted Move",
    y = "Parameter Value",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )