# The main goal of this script is to compute the mean precipitation anomaly over the IH-GAR between 1951 and 2023.
rm(list = ls())
gc()

library(ggplot2)

appo_yr <- 1991:2020
fname <- "Results/mean_annual_prec.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_inputs/PCPD/")


# Importing mean precipitation values over IH-GAR
df <- read.table(fname, header = TRUE)
years <- as.numeric(df$year)
prec <- as.numeric(df$mean_prec)


# Computing 1991-2020 climatology
mask <- years %in% appo_yr
clima_val <- mean(prec[mask], na.rm = TRUE)
prec_anomaly <- prec/clima_val



k    <- 21
half <- (k - 1) / 2
n    <- length(prec_anomaly)

run_mean <- sapply(seq_len(n), function(i) {
  idx <- max(1, i - half):min(n, i + half)
  mean(prec_anomaly[idx], na.rm = TRUE)
})
full_window <- seq_len(n) > half & seq_len(n) <= n - half



delta <- mean(prec[years %in% 1991:2020], na.rm = TRUE) -
  mean(prec[years %in% 1961:1990], na.rm = TRUE)

label_1 <- "Precipitazione media annua sull'IH-GAR:"
label_2 <- sprintf('bold("%+.0f mm")~"(1991-2020 vs. 1961-1990)"', delta)


plot_df <- data.frame(
  year     = years,
  anom     = prec_anomaly,
  sign     = ifelse(prec_anomaly >= 0, "pos", "neg"),
  run_mean = run_mean,
  full     = full_window
)

y_step <- 10^floor(log10(max(abs(plot_df$anom), na.rm = TRUE)))
y_lim  <- ceiling(max(abs(plot_df$anom), na.rm = TRUE) / y_step) * y_step
y_lim  <- y_lim * 1.15

x_guide <- if (packageVersion("ggplot2") >= "3.5.0") {
  guide_axis(minor.ticks = TRUE)
} else {
  "axis"
}


p <- ggplot(plot_df, aes(x = year)) +
  geom_col(aes(y = anom, fill = sign), width = 1, colour = NA) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 1) +
  geom_line(aes(y = run_mean), linewidth = 1.2, linetype = "22") +
  geom_line(data = subset(plot_df, full), aes(y = run_mean), linewidth = 1.2) +
  scale_fill_manual(values = c(pos = "#6495ED", neg = "#B22222"), guide = "none") +
  scale_x_continuous(breaks = seq(1950, 2030, by = 10),
                     minor_breaks = seq(min(years), max(years), by = 1),
                     expand = expansion(add = 1),
                     guide = x_guide) +
  scale_y_continuous(limits = c(-y_lim, y_lim),
                     expand = c(0, 0),
                     sec.axis = dup_axis(name = NULL)) +
  labs(x = "Years", y = "Precipitation anomaly [mm]") +
  theme_bw(base_size = 25) +
  theme(
    panel.grid        = element_blank(),
    panel.border      = element_rect(colour = "black", linewidth = 0.8),
    axis.ticks        = element_line(colour = "black"),
    axis.text         = element_text(colour = "black"),
    axis.ticks.length = unit(0.15, "cm")
  )

print(p)