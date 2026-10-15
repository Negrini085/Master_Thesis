# The main goal of this script is to make plots of peak and first April total SWE 
# volume, in order to assess whether some trends are found or not.
rm(list = ls())
gc()

library(trend)
library(ggplot2)
library(patchwork)

fname <- "Results/SWE/peak_and_first_april_swe_volume.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")



# Function to make trend estimates of peak swe volumes
make_trend_plot <- function(df, lab_points, lab_y){
  
  
  # Making trend estimates (regression line is computed using the Conver formalism)
  mk <- mk.test(df$swe)
  ss <- sens.slope(df$swe)
  
  b <- unname(ss$estimates)
  a <- median(df$swe - b * df$years)
  df$trend <- a + b * df$years
  
  
  # Labels
  p_txt <- if (mk$p.value < 0.001) "p < 0.001" else sprintf("p = %.3f", mk$p.value)
  lab_points <- lab_points
  lab_trend <- sprintf("Trend (%d-%d): %+.3f Gm³/decade, %s",
                       min(df_fap$years), max(df_fap$years), 10 * b, p_txt)
  

  # Plotting procedure  
  p <- ggplot(df, aes(x = years)) +
    geom_line(aes(y = trend, colour = lab_trend), linewidth = 1.1) +
    geom_point(aes(y = swe, colour = lab_points), size = 2.2) +
    scale_colour_manual(
      values = setNames(c("#2C7FB8", "#B2182B"), c(lab_points, lab_trend)),
      breaks = c(lab_points, lab_trend),
      guide  = guide_legend(override.aes = list(
        shape     = c(16, NA),
        linetype  = c(0, 1),
        linewidth = c(0, 1.1)
      ))
    ) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 8), expand = expansion(mult = 0.02)) +
    scale_y_continuous(expand = expansion(mult = c(0.0, 0.22))) +
    coord_cartesian(ylim = c(0, 16)) +
    labs(x = NULL, y = lab_y, colour = NULL) +
    theme_classic(base_size = 14, base_family = "serif") +
    theme(
      panel.border      = element_rect(colour = "black", fill = NA, linewidth = 0.6),
      axis.line         = element_blank(),
      axis.ticks.length = unit(0.15, "cm"),
      axis.text.x       = element_text(size = 15, colour = "black", margin = margin(t = 6)),
      axis.text.y       = element_text(size = 15, colour = "black", margin = margin(r = 6)),
      axis.title.y      = element_text(size = 20),
      legend.justification = c(0, 1),
      legend.background = element_rect(colour = "black", fill = "white", linewidth = 0.4),
      legend.margin     = margin(3, 6, 3, 4),
      legend.key.width  = unit(1.2, "cm"),
      legend.text       = element_text(size = 15)
    ) +
    theme(legend.position = "inside", legend.position.inside = c(0.02, 0.98))
 
  return(p) 
}



# Importing peak SWE volumes
df <- read.table(fname, header = TRUE)
df_fap <- data.frame(years = as.numeric(df$years), swe = as.numeric(df$fap_swe))
df_peak <- data.frame(years = as.numeric(df$years), swe = as.numeric(df$max_swe))


# Making first plot
lab_y <- "SWE [Gm³]"
lab_points <- "Peak annual SWE"
p1 <- make_trend_plot(df = df_peak, lab_points = lab_points, lab_y = lab_y)


# Making second plot
#lab_y <- "1 April SWE [Gm³]"
lab_y <- NULL
lab_points <- "1 April SWE"
p2 <- make_trend_plot(df = df_fap, lab_points = lab_points, lab_y = lab_y)


p1 + p2