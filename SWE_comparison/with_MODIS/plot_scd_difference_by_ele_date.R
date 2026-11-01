# The main goal of this script is to evaluate differences betwee model and modis estimates 
# for snow cover fraction
# The main goal of this script is to compare the snow cover fraction (SCF) of the model with the MODIS one,
# as a function of both elevation and date, through their relative difference (model - MODIS) / model.
rm(list = ls())
gc()

library(ggplot2)

fname_fig   <- "Results/scf_rel_diff_day_elevation.png"
fname_tab   <- "Results/scf_rel_diff_day_elevation.rds"
fname_model <- "Results/scf_band_model.rds"
fname_modis <- "Results/scf_band_MODIS.rds"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_comparison/with_MODIS/")



# Plot settings and labels
min_scf_model <- 0.05      # cells are coloured only where the mean model SCF is > 1%
color_lim     <- 0.15       # colour scale saturates at +/- color_lim (%)

month_label  <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
y_title      <- "Elevation (m a.s.l.)"
legend_title <- "Rel. diff. SCF\n(%)"



# Importing SCF estimates (day x band x year). Days are calendar days of year, with 29 Feb removed,
# so that row 274 is always 1 October.
scf_model <- readRDS(fname_model)
scf_modis <- readRDS(fname_modis)
if (!identical(dimnames(scf_model), dimnames(scf_modis))) stop("Model and MODIS arrays have different dimensions")

n_days      <- dim(scf_model)[1]
n_bands     <- dim(scf_model)[2]
years       <- as.numeric(dimnames(scf_model)$year)
band_labels <- dimnames(scf_model)$band



# Fixing the time offset of MODIS 2000.
# The 2000 MODIS files (day_221 ... day_312) cover 1 Oct - 31 Dec 2000, since their numbering starts on
# 24 Feb 2000, but compute_scd_ele_date_wise_MODIS.R stored them in rows 220-311 (7 Aug - 6 Nov) instead
# of rows 274-365. If this is the case they are moved to the right rows; once the MODIS script is fixed
# and re-run, this block does nothing.
if ("2000" %in% dimnames(scf_modis)$year) {
  rows_2000 <- which(apply(!is.na(scf_modis[, , "2000"]), 1, any))
  if (identical(range(rows_2000), c(220L, 311L))) {
    scf_modis[274:365, , "2000"] <- scf_modis[220:311, , "2000"]
    scf_modis[220:273, , "2000"] <- NA
    cat("MODIS 2000 moved from rows 220-311 to rows 274-365 (1 Oct - 31 Dec)\n")
  }
}



# Keeping only the day-band-year cells available in both datasets, so that both means refer to the same
# sample of years. Then averaging over years, obtaining two (n_days x n_bands) matrices.
both <- !is.na(scf_model) & !is.na(scf_modis)
cat("Cells discarded because available in one dataset only:", sum(!both & (!is.na(scf_model) | !is.na(scf_modis))), "\n")
scf_model[!both] <- NA
scf_modis[!both] <- NA

mean_model <- apply(scf_model, c(1, 2), mean, na.rm = TRUE)
mean_modis <- apply(scf_modis, c(1, 2), mean, na.rm = TRUE)



# Reordering days on the hydrological year (1 Oct - 30 Sep), as in the SWE trend plot
hydro_order <- c(274:365, 1:273)
mean_model  <- mean_model[hydro_order, ]
mean_modis  <- mean_modis[hydro_order, ]



# Computing the absolute and the relative difference (%). The latter is kept only where the mean
# model SCF is above the threshold, otherwise the cell is left grey.
diff_scf <- mean_model - mean_modis
rel_diff <- 100 * diff_scf / mean_model
rel_diff[is.na(mean_model) | mean_model <= min_scf_model] <- NA



# Elevation of each band (middle point), computed from the band labels ("<=50", "50-100", ...)
upper     <- as.numeric(sub("^<=", "", sub(".*-", "", band_labels)))
lower     <- c(0, head(upper, -1))
band_elev <- (lower + upper) / 2



# Creating the result dataframe
res <- expand.grid(day = seq_len(n_days), band = seq_len(n_bands))
res$label      <- band_labels[res$band]
res$elev       <- band_elev[res$band]
res$mean_model <- as.vector(mean_model)
res$mean_modis <- as.vector(mean_modis)
res$diff       <- as.vector(diff_scf)
res$rel_diff   <- as.vector(rel_diff)
saveRDS(res, fname_tab)

cat("Coloured cells:", sum(!is.na(res$rel_diff)), "of", nrow(res), "\n")
cat("Relative difference range (%):", round(range(res$rel_diff, na.rm = TRUE), 1), "\n")
cat("Coloured cells saturated by the colour scale:", sum(abs(res$rel_diff) > color_lim, na.rm = TRUE), "\n")



# Month labels in the middle of each month (hydrological year, 365 days)
month_start <- c(1, 32, 62, 93, 124, 152, 183, 213, 244, 274, 305, 336, 366)
month_mid   <- (head(month_start, -1) + month_start[-1]) / 2

hydro_years  <- paste0(min(years), "/", substr(min(years) + 1, 3, 4), "-", max(years) - 1, "/", substr(max(years), 3, 4))
caption_text <- paste0(
  "Relative difference = (model - MODIS) / model, mean SCF over ", hydro_years, ".\n",
  "Grey: mean model SCF <= ", 100 * min_scf_model, "%.  ",
  "Colour scale saturated at +/-", color_lim, "%."
)



# Final plotting
p <- ggplot(res, aes(x = day, y = elev, fill = diff)) +
  geom_raster() +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0,
                       limits = c(-color_lim, color_lim), oob = scales::squish, na.value = "grey85",
                       name = legend_title) +
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
        plot.margin       = margin(12, 5.5, 5.5, 5.5),
        legend.title      = element_text(size = 20),
        legend.text       = element_text(size = 15))
print(p)