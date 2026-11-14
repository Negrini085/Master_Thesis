# The main goal of this script is to make SWE evolution plots over the common
# region for all three hydrologically oriented snow products
rm(list = ls())
gc()

library(ggplot2)
library(tidyr)
library(dplyr)

fname_po     <- "Results/po_swe_all3.dat"
fname_model  <- "Results/model_swe_all3.dat"
fname_itsnow <- "Results/itsnow_swe_all3.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_comparison/with_ITSNOW/")


# ---- Import ------------------------------------------------------------------
itsnow <- read.table(fname_itsnow, header = TRUE)
model  <- read.table(fname_model,  header = TRUE)
po     <- read.table(fname_po,     header = TRUE)

itsnow$dates <- as.Date(itsnow$dates)
model$dates  <- as.Date(model$dates)
po$dates     <- as.Date(po$dates)

# ---- Complete daily time axis (ITSNOW period) --------------------------------
# Building the full daily sequence makes every day missing from the Po file
# appear as a row with NA, so geom_line breaks the Po line there
start_date <- min(itsnow$dates)
end_date   <- max(itsnow$dates)
all_dates  <- data.frame(dates = seq(start_date, end_date, by = "day"))

cmp <- all_dates %>%
  left_join(itsnow %>% rename_with(~ paste0(.x, "_itsnow"), -dates), by = "dates") %>%
  left_join(model  %>% rename_with(~ paste0(.x, "_model"),  -dates), by = "dates") %>%
  left_join(po     %>% rename_with(~ paste0(.x, "_po"),     -dates), by = "dates") %>%
  # ITSNOW: NA -> 0 (ITSNOW columns ONLY, Po keeps its NA)
  mutate(across(ends_with("_itsnow"), ~ tidyr::replace_na(.x, 0))) %>%
  arrange(dates)


# ---- Long format -------------------------------------------------------------
value_cols <- c(
  "higher_itsnow", "higher_model", "higher_po",
  "medium_itsnow", "medium_model", "medium_po",
  "lower_itsnow",  "lower_model",  "lower_po"
)

cmp_long <- cmp %>%
  dplyr::select(dates, dplyr::all_of(value_cols)) %>%
  tidyr::pivot_longer(
    cols          = dplyr::all_of(value_cols),
    names_to      = c("band", "dataset"),
    names_pattern = "(higher|medium|lower)_(itsnow|model|po)",
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
      levels = c("itsnow", "model", "po"),
      labels = c("ITSNOW", "Model", "Po")
    )
  )

pal <- c(
  "ITSNOW" = "#004488",  # blu scuro
  "Model"  = "#BB5566",  # rosso
  "Po"     = "#DDAA33"   # giallo ocra
)


# ---- Plot --------------------------------------------------------------------
p_cmp <- ggplot(cmp_long, aes(x = dates, y = swe, colour = dataset)) +
  geom_line(
    linewidth = 1.0,
    lineend   = "round",
    na.rm     = TRUE    # only suppresses the warning: the line still breaks at NA
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
    text = element_text(family = "sans", colour = "black"),
    axis.title   = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    axis.text    = element_text(size = 15, colour = "black"),
    axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.line    = element_line(linewidth = 0.6, colour = "black"),
    axis.ticks   = element_line(linewidth = 0.5, colour = "black"),
    axis.ticks.length  = unit(0.15, "cm"),
    panel.grid.major.x = element_line(colour = "grey80", linewidth = 0.35),
    panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.35),
    panel.grid.minor   = element_blank(),
    panel.spacing      = unit(1.4, "lines"),
    strip.background   = element_blank(),
    strip.text = element_text(size = 17, face = "bold", hjust = 0,
                              margin = margin(b = 6)),
    legend.position   = "top",
    legend.direction  = "horizontal",
    legend.text       = element_text(size = 16),
    legend.key.width  = unit(1.6, "cm"),
    legend.margin     = margin(b = 8),
    plot.margin       = margin(t = 5, r = 10, b = 5, l = 5)
  )

print(p_cmp)