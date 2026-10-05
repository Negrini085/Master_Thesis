# The main goal of this script is to plot the daily temperature series in order to 
# show whether some trends are present or not
rm(list = ls())
gc()

library(ggplot2)

fname    <- "Results/mean_temp_series.dat"
out_file <- "Results/temp_anomaly_trend"

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_inputs/TEMP/")

run_mean <- function(x, t, k, min_frac = 0.75) {
  
  n   <- length(x)
  out <- rep(NA_real_, n)
  
  h1 <- floor((k - 1) / 2)
  h2 <- k - 1 - h1
  
  for (i in (h1 + 1):(n - h2)) {
    
    w <- x[(i - h1):(i + h2)]
    
    if (mean(!is.na(w)) >= min_frac) {
      out[i] <- mean(w, na.rm = TRUE)
    }
  }
  
  data.frame(
    t     = t + (h2 - h1) / 2 / 12,
    value = out
  )
}

k_short     <- 4
k_long      <- 40
fit_period  <- c(1991, 2020)
base_period <- c(1951, 1980)

min_days    <- 20
min_frac_rm <- 0.75

df <- read.table(fname, header = TRUE)

temp  <- as.numeric(df$temp)
dates <- as.Date(df$date)

ok <- !is.na(dates)

temp  <- temp[ok]
dates <- dates[ok]

first_month <- as.Date(format(min(dates), "%Y-%m-01"))
last_month  <- as.Date(format(max(dates), "%Y-%m-01"))

all_months <- format(
  seq(first_month, last_month, by = "month"),
  "%Y-%m"
)

ym <- format(dates, "%Y-%m")

n_days <- tapply(
  !is.na(temp),
  ym,
  sum
)

m_mean <- tapply(
  temp,
  ym,
  mean,
  na.rm = TRUE
)

m_mean[n_days < min_days] <- NA


monthly <- data.frame(
  ym    = all_months,
  year  = as.integer(substr(all_months, 1, 4)),
  month = as.integer(substr(all_months, 6, 7))
)

monthly$temp <- as.numeric(m_mean[monthly$ym])
monthly$t <- monthly$year + (monthly$month - 0.5) / 12


in_base <- monthly$year >= base_period[1] &
  monthly$year <= base_period[2]

if (sum(!is.na(monthly$temp[in_base])) < 12 * 10) {
  
  warning(
    "Less than 10 years of data in the base period: ",
    "using the whole record."
  )
  
  in_base <- rep(TRUE, nrow(monthly))
}


clim <- tapply(
  monthly$temp[in_base],
  monthly$month[in_base],
  mean,
  na.rm = TRUE
)

monthly$anom <- monthly$temp -
  clim[as.character(monthly$month)]

rm_short <- run_mean(
  monthly$anom,
  monthly$t,
  k_short,
  min_frac_rm
)

rm_long <- run_mean(
  monthly$anom,
  monthly$t,
  k_long,
  min_frac_rm
)

n_valid <- tapply(
  !is.na(monthly$anom),
  monthly$year,
  sum
)

ann <- tapply(
  monthly$anom,
  monthly$year,
  mean,
  na.rm = TRUE
)

ann[n_valid < 12] <- NA


annual <- data.frame(
  t    = as.integer(names(ann)) + 0.5,
  anom = as.numeric(ann)
)

in_fit <- monthly$year >= fit_period[1] &
  monthly$year <= fit_period[2]

fit <- lm(
  anom ~ t,
  data = monthly[in_fit, ]
)


# Trend in °C / decade
slope <- coef(fit)[["t"]] * 10

se <- summary(fit)$coefficients[
  "t",
  "Std. Error"
] * 10


cat(
  sprintf(
    "Trend %d-%d: %+.2f +/- %.2f degC/decade\n",
    fit_period[1],
    fit_period[2],
    slope,
    se
  )
)

xlim <- c(
  floor(min(monthly$t) / 10) * 10,
  ceiling(max(monthly$t) / 5) * 5
)

t_fit <- seq(
  fit_period[1],
  xlim[2],
  length.out = 200
)

fit_line <- data.frame(
  t = t_fit
)

fit_line$anom <- predict(
  fit,
  newdata = fit_line
)


ylim <- range(
  c(
    rm_short$value,
    annual$anom,
    fit_line$anom
  ),
  na.rm = TRUE
)

ylim <- ylim + c(-0.03, 0.35) * diff(ylim)

col_short <- "#3b82f6"
col_long  <- "#e8141c"
col_fit   <- "#22c55e"

lab_short <- sprintf(
  "%d-month Running Mean",
  k_short
)

lab_long <- sprintf(
  "%d-month Running Mean",
  k_long
)

lab_annual <- "January-December Mean"

lab_fit <- sprintf(
  "Linear Fit (%d-%d): %+.2f °C/decade",
  fit_period[1],
  fit_period[2],
  slope
)


legend_order <- c(
  lab_short,
  lab_long,
  lab_annual,
  lab_fit
)


p <- ggplot() +
  geom_line(
    data = rm_short,
    aes(
      x = t,
      y = value,
      colour = lab_short
    ),
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  geom_line(
    data = rm_long,
    aes(
      x = t,
      y = value,
      colour = lab_long
    ),
    linewidth = 1.6,
    na.rm = TRUE
  ) +
  geom_point(
    data = annual,
    aes(
      x = t,
      y = anom,
      colour = lab_annual
    ),
    shape = 15,
    size = 2.0,
    na.rm = TRUE
  ) +
  geom_line(
    data = fit_line,
    aes(
      x = t,
      y = anom,
      colour = lab_fit
    ),
    linewidth = 1.1,
    linetype = "dotted"
  ) +
scale_colour_manual(
  name = NULL,
  breaks = legend_order,
  values = c(
    setNames(col_short, lab_short),
    setNames(col_long,  lab_long),
    setNames("black",   lab_annual),
    setNames(col_fit,   lab_fit)
  )
) +
scale_x_continuous(
  limits = xlim,
  expand = c(0, 0),
  
  breaks = pretty(
    xlim,
    n = 8
  ),
  
  sec.axis = dup_axis(
    name   = NULL,
    labels = NULL
  )
) +
scale_y_continuous(
  limits = c(-2.5, 4),
  expand = c(0, 0),
  
  sec.axis = dup_axis(
    name   = NULL,
    labels = NULL
  )
) +
  
  # ----------------------------------------------------------
# Labels
# ----------------------------------------------------------

labs(
  x = NULL,
  y = "Temperature Anomaly [°C]"
) +
  
# ----------------------------------------------------------
# Theme
# ----------------------------------------------------------

theme_classic(
  base_family = "serif"
) +
  
  theme(
    
    # Font sizes
    axis.text = element_text(
      size = 20,
      colour = "black"
    ),
    
    axis.title.y = element_text(
      size = 25,
      colour = "black",
      margin = margin(r = 12)
    ),
    
    # Black axes
    axis.line = element_line(
      colour = "black",
      linewidth = 0.5
    ),
    
    axis.ticks = element_line(
      colour = "black",
      linewidth = 0.5
    ),
    
    # Ticks pointing towards the plotting region
    axis.ticks.length = grid::unit(
      -0.10,
      "cm"
    ),
    
    axis.text.x = element_text(
      margin = margin(t = 7)
    ),
    
    axis.text.y = element_text(
      margin = margin(r = 7)
    ),

    axis.text.x.top = element_blank(),
    axis.text.y.right = element_blank(),

    legend.position = c(0.015, 0.985),
    
    legend.justification = c(
      0,
      1
    ),
    
    legend.background = element_rect(
      fill = "white",
      colour = "black",
      linewidth = 0.3
    ),
    
    legend.key = element_rect(
      fill = "white",
      colour = NA
    ),
    
    legend.text = element_text(
      family = "serif",
      size = 20
    ),
    
    legend.spacing.y = grid::unit(
      0.15,
      "cm"
    ),
    plot.margin = margin(
      t = 5,
      r = 8,
      b = 8,
      l = 10
    )
  ) +

guides(
  colour = guide_legend(
    override.aes = list(
      linewidth = c(
        0.8,
        1.6,
        0,
        1.1
      ),
      
      linetype = c(
        "solid",
        "solid",
        "blank",
        "dotted"
      ),
      
      shape = c(
        NA,
        NA,
        15,
        NA
      ),
      
      size = c(
        NA,
        NA,
        2,
        NA
      )
    )
  )
)


print(p)