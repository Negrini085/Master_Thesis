# The main goal of this script is to show the effect of smoothing.
library(ggplot2)

T_melt  <- 0
m_vals  <- c(0.5)
T_range <- c(-3, 3)

T_eff <- function(T_mean, T_melt, m_m) {
  x <- T_mean - T_melt
  pmax(x, 0) + m_m * log1p(exp(-abs(x) / m_m))
}

T_mean <- seq(T_range[1], T_range[2], length.out = 500)

curve <- do.call(rbind, lapply(m_vals, function(m) {
  data.frame(T_mean = T_mean,
             y      = T_eff(T_mean, T_melt, m),
             serie  = paste0("m[m] == ", m))
}))
curve$serie <- factor(curve$serie, levels = paste0("m[m] == ", m_vals))

soglia <- data.frame(T_mean = T_mean, y = pmax(T_mean - T_melt, 0))

colori <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
            "#e87ba4", "#008300", "#6250d6", "#e34948")[seq_along(m_vals)]

p <- ggplot() +
  geom_line(data = soglia,
            aes(T_mean, y, linetype = '"max" * group("(", list(T[mean] - T[melt], 0), ")")'),
            colour = "grey45", linewidth = 0.6) +
  geom_line(data = curve, aes(T_mean, y, colour = serie), linewidth = 0.9) +
  geom_vline(xintercept = T_melt, colour = "grey70", linewidth = 0.3) +
  scale_colour_manual(values = colori,
                      labels = function(l) parse(text = l), name = NULL) +
  scale_linetype_manual(values = "dashed",
                        labels = function(l) parse(text = l), name = NULL) +
  labs(
    title    = NULL,
    x = expression(T[mean] ~ "[°C]"),
    y = "Degree day") +
  guides(colour = guide_legend(order = 1), linetype = guide_legend(order = 2)) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor     = element_blank(),
    panel.grid.major     = element_line(colour = "grey90", linewidth = 0.3),
    axis.title           = element_text(size = 20),
    axis.text            = element_text(size = 15),
    legend.justification = c(0, 1),
    legend.background    = element_rect(fill = "white", colour = NA),
    legend.key.width     = unit(1.6, "lines"),
    legend.text          = element_text(hjust = 0, size = 20),
    legend.spacing.y     = unit(0, "pt"),
    legend.margin        = margin(2, 6, 2, 6),
    plot.title           = element_text(face = "bold")
  )


if (packageVersion("ggplot2") >= "3.5.0") {
  p <- p + theme(legend.position = "inside", legend.position.inside = c(0.02, 0.98))
} else {
  p <- p + theme(legend.position = c(0.02, 0.98))
}

print(p)