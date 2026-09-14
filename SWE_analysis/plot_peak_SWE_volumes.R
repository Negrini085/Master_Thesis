# The main goal of this script is to make plots of peak and first April total SWE 
# volume, in order to assess whether some trends are found or not.
rm(list = ls())
gc()

library(patchwork)
library(ggplot2)
library(terra)


# Mann-Kendall e Sen-Tail trend assessment
trend_stats <- function(x, y, label) {
  
  ok <- complete.cases(x, y)
  x  <- x[ok]; y <- y[ok]
  
  cat("\n=========================", label, "=========================\n")
  print(summary(lm(y ~ x)))
  
  if (requireNamespace("trend", quietly = TRUE)) {
    print(trend::mk.test(y))
    print(trend::sens.slope(y))
  } else {
    message("Need to install package 'trend' -> install.packages(\"trend\")")
  }
  
  invisible(NULL)
}


# Function to make plots
make_plot <- function(data, yvar, title) {
  
  d <- data[complete.cases(data[c("years", yvar)]), ]
  
  ggplot(d, aes(x = years, y = .data[[yvar]])) +
    geom_point(colour = col_pts, size = 0.9) +
    geom_smooth(method = "lm", formula = y ~ x,
                colour = col_trend, fill = col_trend,
                alpha = 0.15, linewidth = 0.8, se = TRUE) +
    annotate("text", x = -Inf, y = Inf,
             label      = trend_label(d$years, d[[yvar]]),
             hjust      = -0.05,
             vjust      = 1.15,
             size       = 3.1,
             lineheight = 1.15) +
    labs(title = title,
         x     = "Year",
         y     = expression("SWE  ["*Gm^3*"]")) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          plot.title       = element_text(face = "bold"))
}

col_pts <- "#2C7FB8"
col_trend <- "#B2182B"

outfig <- "Images/swe_volume_trends.png"
fname <- "Results/SWE/peak_and_first_april_swe_volume.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")





# Importing dataframe and checking if data are NA or not
df <- read.table(fname, header = TRUE)
df[] <- lapply(df, as.numeric)

vars <- c("years", "max_swe", "fap_swe")
stopifnot(all(vars %in% names(df)))


# Trend assessment
trend_label <- function(x, y) {
  fit <- lm(y ~ x)
  sm  <- summary(fit)
  a   <- coef(fit)[1]
  b   <- coef(fit)[2]
  p   <- coef(sm)[2, 4]
  
  sprintf("y = %.4f x %s %.2f\nR\u00b2 = %.2f   p = %.3g\ntrend = %+.3f Gm\u00b3/decade",
          b, if (a >= 0) "+" else "-", abs(a),
          sm$r.squared, p, 10 * b)
}

df <- df[order(df$years), ]

p1 <- make_plot(df, "max_swe", "Peak SWE")
p2 <- make_plot(df, "fap_swe", "1st April SWE")

p <- (p1 + p2) +
  plot_annotation(theme = theme(plot.title = element_text(face = "bold", size = 14)))
print(p)

ggsave(outfig, p, width = 10, height = 4.5, dpi = 300)

trend_stats(df$years, df$max_swe, "Peak SWE")
trend_stats(df$years, df$fap_swe, "1st April SWE")