# The main goal of this script is to make a plot to show trends as a function of both elevation and date.
rm(list = ls())
gc()

library(trend)
library(ggplot2)

fname_fig <- "Results/trend_day_elevation"
fname_tab <- "Results/trend_day_elevation.rds"
fname_in  <- "Results/swe_band_mean_hydrological.rds"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")

# Function to make Mann-Kendall estimates
mk_sen <- function(x) {
  if (any(is.na(x))) return(c(NA_real_, NA_real_))
  res <- tryCatch(sens.slope(x), error = function(e) NULL)
  if (is.null(res)) return(c(NA_real_, NA_real_))
  c(unname(res$estimates), res$p.value)
}


# Plot settings and labels
min_clima_swe <- 5
trend_thresh <- 0.05
trend_units <- "percent"    # "percent"
color_lim <- 10

month_label <- c("Ott", "Nov", "Dic", "Gen", "Feb", "Mar", "Apr", "Mag", "Giu", "Lug", "Ago", "Set")
legend_unit <- if (trend_units == "percent") "%/decade" else "mm/decade"
y_title     <- "Elevation (m a.s.l.)"



# Importing SWE estimates elevation and daily wise over the investigated period.
# Elevation band limits are computed, in order to create the following plot.
swe <- readRDS(fname_in)
n_days <- dim(swe)[1]
n_bands <- dim(swe)[2]
years <- as.numeric(dimnames(swe)$season)
band_labels <- dimnames(swe)$band

upper <- as.numeric(sub("^<=", "", sub(".*-", "", band_labels)))
lower  <- c(0, head(upper, -1))
band_elev <- (lower + upper)/2



# Creating result dataframe, in order to later compute trends elevation wise. 
clim <- apply(swe[, , 1:30], c(1, 2), mean, na.rm = TRUE)
final_clim <- apply(swe[, , 43:72], c(1, 2), mean, na.rm = TRUE)

res <- expand.grid(day = seq_len(n_days), band = seq_len(n_bands))
res$final_clim <- as.vector(final_clim)
res$label <- band_labels[res$band]
res$elev  <- band_elev[res$band]
res$clim  <- as.vector(clim)
res$slope <- NA_real_
res$pval <- NA_real_



# Selecting which pixel include in the trend analysis (with a threshold on SWE).
# Computing trend analyses, in order to check which regions experienced the most.
#todo <- which(!is.na(res$clim) & res$clim >= min_clima_swe)
todo <- which(!is.na(res$final_clim) & res$final_clim >= min_clima_swe)
for (k in seq_along(todo)) {
  idx <- todo[k]
  x <- swe[res$day[idx], res$band[idx], ]
  out <- mk_sen(x)
  
  res$slope[idx] <- out[1]
  res$pval[idx] <- out[2]
  if (k %% 5000 == 0) cat(k, "cells done\n")
}



# Computing the trend slope over a decade and selecting which pixel to put points on
res$slope <- if (trend_units == "percent") 1000 * res$slope / res$clim else 10 * res$slope
dots <- res[res$pval < trend_thresh & res$day %% 5 == 0 & res$band %% 4 == 0, ]


# Month labels in the middle of each month (hydrological year, 365 days)
month_start <- c(1, 32, 62, 93, 124, 152, 183, 213, 244, 274, 305, 336, 366)
month_mid   <- (head(month_start, -1) + month_start[-1]) / 2

caption_text <- paste0(
  "Grey: climatological SWE < ", min_clima_swe, " mm.  ",
  "Point: significant trend (Mann-Kendall, p < ", trend_thresh, ")"
)



# Final plotting
p <- ggplot(res, aes(x = day, y = elev, fill = slope)) +
  geom_raster() +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0,
                       limits = c(-color_lim, color_lim), oob = scales::squish, na.value = "grey85",
                       name = paste0("Trend SWE\n(", legend_unit, ")")) +
  geom_point(data = dots, aes(x = day, y = elev), inherit.aes = FALSE,
             size = 0.15, colour = "grey15") +
  scale_x_continuous(breaks = month_mid, labels = month_label, expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 3000, 500), expand = c(0, 0)) +
  labs(x = NULL, y = y_title, caption = caption_text) +
  guides(fill = guide_colourbar(barheight = unit(5, "cm"), barwidth = unit(0.4, "cm"))) +
  theme_bw(base_size = 10) +
  theme(panel.grid        = element_blank(),
        axis.ticks.x      = element_blank(),
        axis.title.y      = element_text(size = 20),
        axis.text.x       = element_text(size = 15),
        axis.text.y       = element_text(size = 15),
        axis.ticks.y      = element_line(linewidth = 0.5),
        axis.ticks.length = unit(0.2, "cm"),
        plot.caption      = element_text(hjust = 0, size = 12),
        legend.title      = element_text(size = 20),
        legend.text       = element_text(size = 15))
print(p)