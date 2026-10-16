# The main goal of this script is to compare SWE threshold to make SCD maps, in 
# order to assess whether it changes much or not.
rm(list = ls())
gc()

library(ncdf4)
library(ggplot2)

fname_10 <- "Results/SCD/scd_maps_hydro_1952_to_2023_lim_10.nc"
fname_15 <- "Results/SCD/scd_maps_hydro_1952_to_2023_lim_15.nc"
fname_20 <- "Results/SCD/scd_maps_hydro_1952_to_2023_lim_20.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")


# Importing various SCD maps
nc <- nc_open(fname_10)
scd_10 <- ncvar_get(nc, "scd")
scd_10 <- rowMeans(scd_10, dims = 2, na.rm = TRUE)
nc_close(nc)

nc <- nc_open(fname_15)
scd_15 <- ncvar_get(nc, "scd")
scd_15 <- rowMeans(scd_15, dims = 2, na.rm = TRUE)
nc_close(nc)

nc <- nc_open(fname_20)
scd_20 <- ncvar_get(nc, "scd")
scd_20 <- rowMeans(scd_20, dims = 2, na.rm = TRUE)
nc_close(nc)


# Making difference maps, keeping 15 mm w.e. as reference point (and selecting valid pixels)
diff_15_10 <- scd_15 - scd_10
diff_15_20 <- scd_15 - scd_20
mask <- !is.na(scd_10) & !is.na(scd_15) & !is.na(scd_20) & (scd_10 > 0 | scd_15 > 0 | scd_20 > 0)

lab_10 <- "SCD(15) - SCD(10)"
lab_20 <- "SCD(15) - SCD(20)"







# Plotting procedure
df_diff <- data.frame(
  diff = c(diff_15_10[mask], diff_15_20[mask]),
  comparison = factor(rep(c(lab_10, lab_20), each = sum(mask)), levels = c(lab_10, lab_20))
)

bw <- 1
lims <- range(df_diff$diff)
breaks <- seq(floor(lims[1]) - bw / 2, ceiling(lims[2]) + bw, by = bw)

p_diff <- ggplot(df_diff, aes(x = diff, fill = comparison)) +
  geom_histogram(aes(y = after_stat(density * width * 100)),
                 breaks = breaks, position = "identity", alpha = 0.6,
                 color = "white", linewidth = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey30", linewidth = 0.4) +
  scale_fill_manual(values = setNames(c("#3a7abf", "#d9622b"),
                                      c(lab_10, lab_20)),
                    name = NULL) +
  labs(
    title = NULL,
    x = "SCD difference (days)",
    y = "Frequency (%)"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  # coord_cartesian(xlim = c(-30, 30)) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 15),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.minor.y = element_line(color = "grey92", linewidth = 0.2),
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.90),
    legend.text = element_text(size = 15)
  )

p_diff

aggregate(diff ~ comparison, data = df_diff, FUN = function(x)
  round(c(mean = mean(x), median = median(x), sd = sd(x),
          pct_unchanged = mean(x == 0) * 100), 2))