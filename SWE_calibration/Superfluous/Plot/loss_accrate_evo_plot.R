# The main goal of this script is to plot the loss evolution throughout the
# simulated annealing procedure, together with the acceptance rate and the
# neighbourhood half-width delta.
#
# - Loss: one value per ACCEPTED MOVE  -> x-axis = Accepted Move
# - Acceptance rate and delta: one value per TEMPERATURE STEP -> x-axis = T
#
# The cooling schedule is geometric (T_k = T_0 * alpha^k) and every step
# accepts the same number of moves, so log(T) is linear in the accepted move.
# The temperature axis is therefore logarithmic and decreasing from left to
# right, and its limits are chosen so that each temperature sits exactly
# below the accepted moves performed at that temperature: the three panels
# can be read vertically. The vertical grid lines (temperature decades) are
# shared by all panels; the loss panel also shows them as a top axis.
rm(list = ls())
gc()

library(ggplot2)
library(dplyr)
library(patchwork)   # install.packages("patchwork") if missing

n_runs      <- 5
run_levels  <- paste("Run", seq_len(n_runs))
fnames_loss <- sprintf("Runs/ddf_param/loss_evo_%d.dat",    seq_len(n_runs))
fnames_acc  <- sprintf("Runs/ddf_param/accrate_evo_%d.dat", seq_len(n_runs))
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_calibration/")


# ---------------------------------------------------------------------------
# Importing data
# ---------------------------------------------------------------------------
# Loss: one row per accepted move. V1 = loss, V5 = temperature.
read_loss <- function(i) {
  df <- read.table(fnames_loss[i], header = FALSE)
  data.frame(
    AcceptedMove = seq_len(nrow(df)),
    Loss         = as.numeric(df$V1),
    Temperature  = as.numeric(df$V5),
    Run          = run_levels[i]
  )
}

# Acceptance rate / delta: one row per temperature step.
# V1 = acceptance rate (%), V2 = 2 * delta, V3 = temperature.
read_accrate <- function(i) {
  df <- read.table(fnames_acc[i], header = FALSE)
  data.frame(
    TempStep    = seq_len(nrow(df)),
    AccRate     = as.numeric(df$V1),
    Delta       = as.numeric(df$V2) / 2,
    Temperature = as.numeric(df$V3),
    Run         = run_levels[i]
  )
}

loss_df <- dplyr::bind_rows(lapply(seq_len(n_runs), read_loss)) %>%
  dplyr::mutate(Run = factor(Run, levels = run_levels))

acc_df <- dplyr::bind_rows(lapply(seq_len(n_runs), read_accrate)) %>%
  dplyr::mutate(Run = factor(Run, levels = run_levels))

# Sanity check: both files must describe the same cooling schedule
stopifnot(isTRUE(all.equal(
  sort(unique(loss_df$Temperature)),
  sort(unique(acc_df$Temperature)),
  tolerance = 1e-4
)))


# ---------------------------------------------------------------------------
# Acceptance rate: centred moving average
# ---------------------------------------------------------------------------
# The raw acceptance rate is very noisy (it can only take values 5/n), so a
# centred moving average over a few temperature steps is overlaid.
k_smooth <- 9   # window width, in temperature steps
roll_mean <- function(x, k) {
  h <- (k - 1) %/% 2
  n <- length(x)
  vapply(seq_len(n), function(j) mean(x[max(1, j - h):min(n, j + h)]), numeric(1))
}

# CoolAxis = -log10(T): increases as the system cools down, so it can be
# drawn on a standard (left-to-right) axis and labelled as 10^-CoolAxis.
acc_df <- acc_df %>%
  dplyr::group_by(Run) %>%
  dplyr::arrange(TempStep, .by_group = TRUE) %>%
  dplyr::mutate(
    AccSmooth = roll_mean(AccRate, k_smooth),
    CoolAxis  = -log10(Temperature)
  ) %>%
  dplyr::ungroup()


# ---------------------------------------------------------------------------
# Aligning the temperature axis with the accepted-move axis
# ---------------------------------------------------------------------------
# Centre (in accepted moves) of the block of moves performed at each T.
# -log10(T) is linear in it, so a straight-line fit gives the exact mapping.
step_mid <- loss_df %>%
  dplyr::group_by(Temperature) %>%
  dplyr::summarise(MoveMid = mean(AcceptedMove), .groups = "drop")

cool_fit <- lm(I(-log10(Temperature)) ~ MoveMid, data = step_mid)
c0 <- unname(coef(cool_fit)[1])
c1 <- unname(coef(cool_fit)[2])
move_to_cool <- function(m) c0 + c1 * m
cool_to_move <- function(z) (z - c0) / c1

# Common horizontal extent (1 % padding on each side, no extra expansion)
x_lim_move <- c(0.5, max(loss_df$AcceptedMove) + 0.5)
x_lim_move <- x_lim_move + c(-1, 1) * 0.01 * diff(x_lim_move)
x_lim_cool <- move_to_cool(x_lim_move)

x_breaks_move <- seq(0, max(loss_df$AcceptedMove), by = 250)

# Temperature decades: breaks on the T axis, grid lines in every panel
cool_breaks   <- seq(ceiling(min(x_lim_cool)), floor(max(x_lim_cool)), by = 1)
cool_labels   <- parse(text = paste0("10^", -cool_breaks))
cool_grid_mov <- cool_to_move(cool_breaks)


# ---------------------------------------------------------------------------
# Common style
# ---------------------------------------------------------------------------
pal <- c(
  "Run 1" = "#004488",
  "Run 2" = "#DDAA33",
  "Run 3" = "#BB5566",
  "Run 4" = "#44AA99",
  "Run 5" = "#AA4499"
)

theme_sa <- theme_classic(base_size = 12) +
  theme(
    text = element_text(
      family = "sans",
      colour = "black"
    ),
    axis.title = element_text(
      size = 20,
      face = "plain"
    ),
    axis.title.x = element_text(
      margin = margin(t = 12)
    ),
    axis.title.x.top = element_text(
      size   = 17,
      margin = margin(b = 8)
    ),
    axis.title.y = element_text(
      face   = "plain",
      margin = margin(r = 12)
    ),
    axis.text = element_text(
      size   = 15,
      colour = "black"
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

# Common temperature x-scale for the two lower panels
scale_x_temperature <- function(...) {
  scale_x_continuous(
    limits = x_lim_cool,
    breaks = cool_breaks,
    labels = cool_labels,
    expand = expansion(0),
    ...
  )
}


# ---------------------------------------------------------------------------
# Panel 1: loss vs accepted move (main panel)
# ---------------------------------------------------------------------------
p_loss <- ggplot(loss_df, aes(x = AcceptedMove, y = Loss, colour = Run)) +
  # vertical grid at temperature decades, shared with the lower panels
  geom_vline(
    xintercept = cool_grid_mov,
    colour     = "grey80",
    linewidth  = 0.35
  ) +
  geom_line(
    linewidth = 0.9,
    lineend   = "round"
  ) +
  scale_colour_manual(values = pal) +
  # scale_x_continuous(
  #   limits   = x_lim_move,
  #   breaks   = x_breaks_move,
  #   expand   = expansion(0),
  #   sec.axis = sec_axis(
  #     ~ .,
  #     name   = "Temperature",
  #     breaks = cool_grid_mov,
  #     labels = cool_labels
  #   )
  # ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.04, 0.06))
  ) +
  labs(
    x      = "Accepted Move",
    y      = "Loss",
    colour = NULL
  ) +
  guides(
    colour = guide_legend(nrow = 1, override.aes = list(linewidth = 2))
  ) +
  theme_sa +
  theme(panel.grid.major.x = element_blank())


# ---------------------------------------------------------------------------
# Panel 2: acceptance rate vs temperature (raw + moving average)
# ---------------------------------------------------------------------------
p_acc <- ggplot(acc_df, aes(x = CoolAxis, colour = Run)) +
  geom_line(
    aes(y = AccRate),
    linewidth = 0.3,
    alpha     = 0.35
  ) +
  geom_line(
    aes(y = AccSmooth),
    linewidth = 0.9,
    lineend   = "round"
  ) +
  scale_colour_manual(values = pal, guide = "none") +
  scale_x_temperature() +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    expand = expansion(mult = c(0.04, 0.06))
  ) +
  labs(
    x = NULL,
    y = "AR (%)"
  ) +
  theme_sa +
  theme(axis.text.x = element_blank())


# ---------------------------------------------------------------------------
# Panel 3: delta vs temperature
# ---------------------------------------------------------------------------
p_delta <- ggplot(acc_df, aes(x = CoolAxis, y = Delta, colour = Run)) +
  geom_line(
    linewidth = 0.9,
    lineend   = "round"
  ) +
  scale_colour_manual(values = pal, guide = "none") +
  scale_x_temperature() +
  scale_y_continuous(
    expand = expansion(mult = c(0.04, 0.06))
  ) +
  labs(
    x = "Temperature",
    y = expression(delta)
  ) +
  theme_sa


# ---------------------------------------------------------------------------
# Assembling: the loss panel gets most of the vertical space
# ---------------------------------------------------------------------------
p_sa <- p_loss / p_acc / p_delta +
  plot_layout(heights = c(2.4, 1, 1), guides = "collect") &
  theme(legend.position = "top")

print(p_sa)

# ggsave("Figures/loss_evolution.pdf", p_sa, width = 11, height = 11)