# The main goal of this script is to plot the seasonal cycle of the degree-day
# factor (DDF), described by a sinusoid with a period of 366 days:
#   DDF_t = DDF_ave + DDF_amp * sin(2*pi*(t - 81)/366)
# The maximum falls at t = 81 + 366/4 (~172). The labels on the y axis are left
# symbolic (ave, ave +- amp), so the values of the parameters are just examples.
# DDF_ave and DDF_amp are marked on the plot with double-headed segments: the
# first goes from 0 to the mean level, the second from the mean level to the peak.
rm(list = ls())
gc()

library(ggplot2)



# Plot parameters
size_title        <- 20
size_subtitle     <- 12
size_legend_title <- 15
size_legend_text  <- 15
size_axis_title   <- 20
size_axis_text    <- 15
size_annotation   <- 20

theme_fonts <- theme(
  plot.title    = element_text(size = size_title, face = "bold"),
  plot.subtitle = element_text(size = size_subtitle),
  legend.title  = element_text(size = size_legend_title),
  legend.text   = element_text(size = size_legend_text),
  axis.title    = element_text(size = size_axis_title),
  axis.title.x  = element_text(margin = margin(t = 10)),
  axis.title.y  = element_text(margin = margin(r = 10)),
  axis.text     = element_text(size = size_axis_text)
)



# Sinusoid parameters
ddf_ave <- 2.2
ddf_amp <- 1.7
t_max   <- 81 + 366 / 4

# Axes limits (the x axis is at DDF = 0, so that DDF_ave can be measured from it)
x_lim <- c(0, 380)
y_lim <- c(0, ddf_ave + 1.3 * ddf_amp)

# Position of the segments
t_ave      <- 81                              # day in which DDF = DDF_ave (rising)
arrow_segm <- arrow(length = unit(0.2, "cm"), ends = "both", type = "open")



# Building the dataset
df_ddf <- data.frame(t = seq(0, 366, by = 0.5))
df_ddf$ddf <- ddf_ave + ddf_amp * sin(2 * pi * (df_ddf$t - 81) / 366)


# Plotting procedure
p <- ggplot() +
  annotate(
    "segment",
    x = t_max, xend = t_max, y = y_lim[1], yend = ddf_ave + ddf_amp,
    linetype = "dotted", color = "grey60"
  ) +
  annotate(
    "segment",
    x = 0, xend = 366, y = ddf_ave, yend = ddf_ave,
    linetype = "dashed", color = "grey60"
  ) +
  geom_line(
    data = df_ddf,
    aes(x = t, y = ddf),
    color = "darkorange3", linewidth = 1
  ) +
  # DDF_ave segment
  annotate(
    "segment",
    x = t_ave, xend = t_ave, y = 0, yend = ddf_ave,
    arrow = arrow_segm, color = "black", linewidth = 0.6
  ) +
  annotate(
    "text",
    x = t_ave + 5, y = ddf_ave / 2, label = "DDF[ave]", parse = TRUE,
    hjust = 0, size = size_annotation / .pt
  ) +
  # DDF_amp segment
  annotate(
    "segment",
    x = t_max, xend = t_max, y = ddf_ave, yend = ddf_ave + ddf_amp,
    arrow = arrow_segm, color = "black", linewidth = 0.6
  ) +
  annotate(
    "text",
    x = t_max + 5, y = ddf_ave + ddf_amp / 2, label = "DDF[amp]", parse = TRUE,
    hjust = 0, size = size_annotation / .pt
  ) +
  scale_x_continuous(
    breaks = c(81, 172, 264),
    limits = x_lim, expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = NULL,
    labels = expression(0, bolditalic(ave - amp), bolditalic(ave), bolditalic(ave + amp)),
    limits = y_lim, expand = c(0, 0)
  ) +
  theme_minimal() +
  labs(title = NULL, x = "Giorno giuliano", y = "DDF") +
  theme(
    panel.grid = element_blank(),
    axis.line  = element_line(color = "black", arrow = arrow(length = unit(0.25, "cm"), type = "open")),
    axis.ticks = element_line(color = "black"),
    axis.text  = element_text(color = "black")
  ) +
  theme_fonts

print(p)