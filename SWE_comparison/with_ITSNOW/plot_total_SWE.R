# The main goal of this script is to plot both total volume time series in order
# to address eventual differences
rm(list = ls())
gc()

library(ggplot2)
library(dplyr)
library(tidyr)

fname_model  <- "Results/model_swe_series.dat"
fname_itsnow <- "Results/itsnow_swe_series.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_comparison/with_ITSNOW/")

# Optional analysis window
date_start <- NULL
date_end   <- NULL


# Importing both SWE series ----------------------------------------------------
# NB: the two files do NOT cover the same period (model: 2010-01-01 -> 2023-12-31,
# ITSNOW: 2010-09-01 -> 2023-08-31), so they must be aligned by date and not by
# row index: data.frame(model = swe_model, itsnow = swe_itsnow) would fail.
df_model  <- read.table(fname_model,  header = TRUE)
df_itsnow <- read.table(fname_itsnow, header = TRUE)

df_model$dates  <- as.Date(df_model$dates)
df_itsnow$dates <- as.Date(df_itsnow$dates)

df_swe <- dplyr::inner_join(
  df_model  %>% dplyr::select(dates, model  = swe),
  df_itsnow %>% dplyr::select(dates, itsnow = swe),
  by = "dates"
) %>%
  dplyr::arrange(dates)

if (!is.null(date_start)) df_swe <- dplyr::filter(df_swe, dates >= date_start)
if (!is.null(date_end))   df_swe <- dplyr::filter(df_swe, dates <= date_end)

df_long <- df_swe %>%
  tidyr::pivot_longer(
    cols      = c(itsnow, model),
    names_to  = "dataset",
    values_to = "swe"
  ) %>%
  dplyr::mutate(
    # factor order = drawing order: ITSNOW first, Model on top (as in the bands figure)
    dataset = factor(
      dataset,
      levels = c("itsnow", "model"),
      labels = c("ITSNOW", "Model")
    )
  )


# Plotting procedure -----------------------------------------------------------
pal <- c(
  "ITSNOW" = "#0072B2",
  "Model"  = "#D55E00"
)

plot_swe_single <- function(data, pal, panel_title = NULL) {
  ggplot(data, aes(x = dates, y = swe, colour = dataset)) +
    geom_line(
      linewidth = 1.0,
      lineend   = "round"
    ) +
    scale_colour_manual(values = pal) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand      = expansion(mult = c(0.005, 0.005))
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.06))
    ) +
    labs(
      title  = NULL,
      x      = "Date",
      y      = expression(plain("SWE [Gm"^3*"]")),
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
      # the title plays the role of the facet strip in the multi-band figure
      plot.title = element_text(
        size   = 17,
        face   = "bold",
        hjust  = 0,
        margin = margin(b = 6)
      ),
      plot.title.position = "panel",
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
      panel.grid.minor  = element_blank(),
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
}

p_swe <- plot_swe_single(df_long, pal, panel_title = "Total volume")
print(p_swe)