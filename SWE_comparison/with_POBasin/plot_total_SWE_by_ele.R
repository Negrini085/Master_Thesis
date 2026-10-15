# The main goal of this script is to make SWE evolution plots comparing ITSNOW 
# and Po District in order to show which one is best
rm(list = ls())
gc()

library(ggplot2)
library(tidyr)
library(dplyr)

fname_PO <- "Results/po_basin_swe_series.dat"
fname_model <- "Results/model_swe_series.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_comparison/with_POBasin/")


# Importing both SWE evolution files in order to make a comparison
po <- read.table(fname_PO, header = TRUE)
model <- read.table(fname_model, header = TRUE)
mask <- (as.Date(model$dates) < as.Date("1991-10-01")) | (as.Date(model$dates) > as.Date("2021-09-30"))
model <- model[!mask, ]

po$dates <- as.Date(po$dates)
model$dates <- as.Date(model$dates)

cmp <- merge(po, model, by = "dates", all = TRUE, suffixes = c("_po", "_model"))
cmp <- cmp[order(cmp$dates), ]
rownames(cmp) <- NULL


value_cols <- c("higher_po", "higher_model",
                "medium_po", "medium_model",
                "lower_po",  "lower_model")

cmp_long <- cmp %>%
  dplyr::select(dates, dplyr::all_of(value_cols)) %>%
  tidyr::pivot_longer(
    cols          = dplyr::all_of(value_cols),
    names_to      = c("band", "dataset"),
    names_pattern = "(higher|medium|lower)_(po|model)",
    values_to     = "swe"
  ) %>%
  dplyr::mutate(
    band = factor(
      band,
      levels = c("higher", "medium", "lower"),
      labels = c("> 2000 m a.s.l.", "1000 - 2000 m a.s.l.", "< 1000 m a.s.l.")
    ),
    dataset = factor(
      dataset,
      levels = c("po", "model"),
      labels = c("PO District", "Model")
    )
  )

pal <- c(
  "PO District" = "#0072B2",
  "Model"      = "#D55E00"
)

p_cmp <- ggplot(cmp_long, aes(x = dates, y = swe, colour = dataset)) +
  geom_line(
    linewidth = 1.0,
    lineend   = "round"
  ) +
  facet_wrap(
    ~ band,
    ncol   = 1,
    scales = "free_y"
  ) +
  scale_colour_manual(values = pal) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand      = expansion(mult = c(0.005, 0.005))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.06))
  ) +
  labs(
    x      = "Date",
    y      = expression(bold("SWE [Gm"^3*"]")),
    colour = NULL
  ) +
  guides(
    colour = guide_legend(override.aes = list(linewidth = 2))
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(
      family = "sans",
      colour = "black"
    ),
    axis.title = element_text(
      size = 20,
      face = "bold"
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
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
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

print(p_cmp)