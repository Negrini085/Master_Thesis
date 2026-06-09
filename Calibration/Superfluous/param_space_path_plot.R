# The main goal of this script is to plot the path on the parameter space
rm(list = ls())
gc()

library(ggplot2)
library(plotly)
library(tidyr)

fname_par <- "moves.dat"
fname_mod <- "SimAnnealing.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Calibration/")


# Importing parameter values and loss function value
df_par <- read.table(fname_par, header = FALSE)
df_mod <- read.table(fname_mod, header = FALSE)

t_th <- as.numeric(df_par$V1)
ddfm <- as.numeric(df_par$V2)
ddfM <- as.numeric(df_par$V3)
loss <- as.numeric(df_mod$V1)


# Plotting procedure
df_plot <- data.frame(
  t_th = t_th,
  ddfm = ddfm,
  ddfM = ddfM,
  loss = loss,
  step = 1:length(loss)
)

plot_ly(df_plot, 
        x = ~ddfm, 
        y = ~ddfM, 
        z = ~t_th, 
        color = ~loss, 
        colors = viridis::viridis(100),
        type = 'scatter3d', 
        mode = 'lines+markers',
        line = list(width = 3),
        marker = list(size = 4)) %>%
  layout(
    title = "Parameter space path (Simulated Annealing)",
    scene = list(
      xaxis = list(title = 'DDF min'),
      yaxis = list(title = 'DDF max'),
      zaxis = list(title = 'T th')
    )
  )