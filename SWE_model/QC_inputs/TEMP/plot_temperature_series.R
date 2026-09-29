# The main goal of this script is to plot the daily temperature series in order to show whether some trends are present or not
rm(list = ls())
gc()

fname <- "Results/mean_temp_series.dat"
out_file <- "Results/temp_anomaly_trend"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_inputs/TEMP/")


# Running mean function
run_mean <- function(x, t, k, min_frac = 0.75) {
  n   <- length(x)
  out <- rep(NA_real_, n)
  h1  <- floor((k - 1) / 2)
  h2  <- k - 1 - h1
  for (i in (h1 + 1):(n - h2)) {
    w <- x[(i - h1):(i + h2)]
    if (mean(!is.na(w)) >= min_frac) out[i] <- mean(w, na.rm = TRUE)
  }
  data.frame(t = t + (h2 - h1) / 2 / 12, value = out)
}


# Plotting procedure
make_plot <- function() {
  par(mar = c(3, 5, 1, 1), family = "serif", las = 1,
      tcl = 0.4, mgp = c(3.2, 0.6, 0), cex.axis = 1.2, cex.lab = 1.4)
  plot(NA, xlim = xlim, ylim = ylim, xaxs = "i",
       xlab = "", ylab = "Temperature Anomaly (\u00B0C)")
  axis(3, labels = FALSE)
  axis(4, labels = FALSE)
  
  lines(rm_short$t, rm_short$value, col = col_short, lwd = 2.5)
  lines(rm_long$t,  rm_long$value,  col = col_long,  lwd = 5)
  points(annual$t, annual$anom, pch = 15, cex = 0.7)
  lines(t_fit, y_fit, col = col_fit, lty = 3, lwd = 3.5)
  
  legend("topleft", inset = 0.01, bg = "white", cex = 1.4,
         legend = c(sprintf("%d-month Running Mean", k_short),
                    sprintf("%d-month Running Mean", k_long),
                    "January-December Mean",
                    sprintf("Best Linear Fit (%d-%d): %+.2f \u00B0C/decade",
                            fit_period[1], fit_period[2], slope)),
         col = c(col_short, col_long, "black", col_fit),
         lty = c(1, 1, NA, 3), lwd = c(1.3, 5, NA, 3.5), pch = c(NA, NA, 15, NA),
         seg.len = 2.5)
}










# Running mean & fit parameters
k_short     <- 4
k_long      <- 40
fit_period  <- c(2000, 2020)
base_period <- c(1951, 1980)
min_days    <- 20
min_frac_rm <- 0.75


# Importing data
df    <- read.table(fname, header = TRUE)
temp  <- as.numeric(df$temp)
dates <- as.Date(df$date)

ok    <- !is.na(dates)
temp  <- temp[ok]
dates <- dates[ok]

first_month <- as.Date(format(min(dates), "%Y-%m-01"))
last_month  <- as.Date(format(max(dates), "%Y-%m-01"))
all_months  <- format(seq(first_month, last_month, by = "month"), "%Y-%m")

ym     <- format(dates, "%Y-%m")
n_days <- tapply(!is.na(temp), ym, sum)
m_mean <- tapply(temp, ym, mean, na.rm = TRUE)
m_mean[n_days < min_days] <- NA

monthly <- data.frame(
  ym    = all_months,
  year  = as.integer(substr(all_months, 1, 4)),
  month = as.integer(substr(all_months, 6, 7))
)
monthly$temp <- as.numeric(m_mean[monthly$ym])
monthly$t    <- monthly$year + (monthly$month - 0.5) / 12

in_base <- monthly$year >= base_period[1] & monthly$year <= base_period[2]
if (sum(!is.na(monthly$temp[in_base])) < 12 * 10) {
  warning("Less than 10 years of data in the base period: using the whole record.")
  in_base <- rep(TRUE, nrow(monthly))
}
clim <- tapply(monthly$temp[in_base], monthly$month[in_base], mean, na.rm = TRUE)
monthly$anom <- monthly$temp - clim[as.character(monthly$month)]



rm_short <- run_mean(monthly$anom, monthly$t, k_short, min_frac_rm)
rm_long  <- run_mean(monthly$anom, monthly$t, k_long,  min_frac_rm)


n_valid <- tapply(!is.na(monthly$anom), monthly$year, sum)
ann     <- tapply(monthly$anom, monthly$year, mean, na.rm = TRUE)
ann[n_valid < 12] <- NA
annual  <- data.frame(t = as.integer(names(ann)) + 0.5, anom = as.numeric(ann))


in_fit <- monthly$year >= fit_period[1] & monthly$year <= fit_period[2]
fit    <- lm(anom ~ t, data = monthly[in_fit, ])
slope  <- coef(fit)[["t"]] * 10
se     <- summary(fit)$coefficients["t", "Std. Error"] * 10
cat(sprintf("Trend %d-%d: %+.2f +/- %.2f degC/decade\n",
            fit_period[1], fit_period[2], slope, se))


xlim   <- c(floor(min(monthly$t) / 10) * 10, ceiling(max(monthly$t) / 5) * 5)
t_fit  <- seq(fit_period[1], xlim[2], length.out = 200)
y_fit  <- predict(fit, newdata = data.frame(t = t_fit))
ylim   <- range(c(rm_short$value, annual$anom, y_fit), na.rm = TRUE)
ylim   <- ylim + c(-0.03, 0.35) * diff(ylim)

col_short <- "#3b82f6"
col_long  <- "#e8141c"
col_fit   <- "#22c55e"

make_plot()