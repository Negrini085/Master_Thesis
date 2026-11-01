# The main goal of this script is to make a comparison plot between snow covered 
# area as predicted by our model and by MODIS.
rm(list = ls())
gc()

library(ggplot2)

fname_model <- "Results/snow_covered_area_model.dat"
fname_modis <- "Results/snow_covered_area_modis.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_comparison/with_MODIS/")


# Importing both series
df_model <- read.table(fname_model, header = FALSE)
area_model <- as.numeric(df_model$V1)

df_modis <- read.table(fname_modis, header = FALSE)
area_modis <- as.numeric(df_modis$V1)

stopifnot(length(area_model) == 365, length(area_modis) == 365)

df_area <- data.frame(
  model = area_model,
  modis = area_modis
)


# ---- Hydrological-year time axis ---------------------------------------------
# Day 1 = 1st October. The year is only a placeholder: Oct 2022 - Sep 2023 is a
# non-leap hydrological year, i.e. exactly 365 days. Only months are shown.
dates <- seq(as.Date("2022-10-01"), by = "day", length.out = nrow(df_area))

month_breaks <- seq(as.Date("2022-10-01"), as.Date("2023-09-01"), by = "month")
month_labels <- month.abb[c(10:12, 1:9)]   # Oct, Nov, ..., Sep (locale-independent)
# For Italian labels instead:
# month_labels <- c("Ott", "Nov", "Dic", "Gen", "Feb", "Mar",
#                   "Apr", "Mag", "Giu", "Lug", "Ago", "Set")


# ---- Long format for ggplot ---------------------------------------------------
df_plot <- data.frame(
  date   = rep(dates, 2),
  area   = c(df_area$modis, df_area$model),
  source = factor(rep(c("MODIS", "Model"), each = nrow(df_area)),
                  levels = c("MODIS", "Model"))
)


# ---- Plot -------------------------------------------------------------------
col_series <- c("MODIS" = "#0072B2", "Model" = "#D55E00")

size_axis_title <- 20
size_axis_text <- 15
size_legend <- 20
size_title <- 24

p <- ggplot(df_plot, aes(x = date, y = area, colour = source)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = col_series, name = NULL) +
  scale_x_date(breaks = month_breaks,
               labels = month_labels,
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(labels = scales::label_number(big.mark = ","),
                     limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL,
       y = expression(plain("Snow covered area [km"^plain("2")*"]")),
       title = NULL) +
  theme_bw(base_size = 13) +
  theme(
    panel.border       = element_blank(),
    panel.grid.major   = element_line(colour = "grey85", linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    axis.line.x        = element_line(colour = "black", linewidth = 0.6),
    axis.ticks.y       = element_blank(),
    axis.text          = element_text(colour = "black", size = size_axis_text),
    axis.text.x        = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.title         = element_text(face = "plain", size = size_axis_title),
    axis.title.x       = element_text(margin = margin(t = 8)),
    axis.title.y       = element_text(margin = margin(r = 8)),
    plot.title         = element_text(face = "plain", size = size_title, hjust = 0.5),
    legend.text        = element_text(size = size_legend),
    legend.position    = "top",
    legend.justification = "center",
    legend.key.width   = unit(1.5, "lines"),
    legend.margin      = margin(0, 0, -5, 0)
  )

print(p)