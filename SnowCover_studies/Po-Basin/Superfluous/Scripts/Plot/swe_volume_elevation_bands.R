# The main goal of this script is to plot SWE evolution by elevation band. We want
# to have just three plots per image, in order to create just 3 plots
rm(list = ls()); gc()

library(grid)
library(ggplot2)
library(gridExtra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")

swe <- read.table("Datas/swe_evolution_elevation.dat", header = FALSE)

df <- data.frame(
  day  = seq_len(nrow(swe)),
  swe1 = swe$V1,
  swe2 = swe$V2,
  swe3 = swe$V3,
  swe4 = swe$V4,
  swe5 = swe$V5,
  swe6 = swe$V6,
)

# Selecting bands to keep
keep_bands <- c("swe1", "swe2", "swe3")

data0 <- as.Date("1991-10-03")
dates <- as.Date(paste0(seq(1991, 2021, by = 3), "-10-03"))
dates_ind <- as.numeric(dates - data0) + 1

band_labels <- c(
  swe4 = "0 - 500 m",
  swe5 = "500 - 1000 m",
  swe6 = "1000 - 1500 m"
)

make_plot <- function(band_name){
  ggplot(df, aes(x = day, y = .data[[band_name]])) +
    geom_line(linewidth = 0.9, color = "blue") +
    scale_x_continuous(
      breaks = dates_ind,
      labels = format(dates, "%d %b %Y"),
      expand = c(0, 0)
    ) +
    labs(
      title = band_labels[[band_name]],
      x = NULL,
      y = "SWE (Gm³)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold")
    )
}

p1 <- make_plot(keep_bands[1])
p2 <- make_plot(keep_bands[2])
p3 <- make_plot(keep_bands[3])

grid.arrange(
  grobs = list(p1, p2, p3),
  ncol = 1,
  top = textGrob("SWE evolution: from 1500 to 3000 meters", gp = gpar(fontface = "bold", fontsize = 14))
)
