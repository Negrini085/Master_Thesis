# The main goal of this script is to plot SWE evolution by elevation band. We want
# to have just three plots per image, in order to create just 3 plots
rm(list = ls())
gc()

library(grid)
library(ggplot2)
library(gridExtra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")

appo <- read.table("Datas/swe_evolution_elevation.dat", header = FALSE)
appo <- as.matrix(appo)

# We need to create NULL periods lasting for 93 days, because to plot SWE volume
# series we aim for the whole year to be considered
dur <- 272
conta1 <- 0
conta2 <- 0
years <- 1992:2021
swe <- array(0, dim = c(10958, 6))

for(y in years){
  
  if(y%%4 == 0){
    swe[(conta1+1):(conta1+dur+1), ] <- appo[(conta2+1):(conta2+dur+1), ]
    swe[(conta1+dur+2):(conta1+dur+94), ] <- array(NA_real_, dim = c(93, 6))
    
    conta1 <- conta1 + dur + 94
    conta2 <- conta2 + dur + 1
  }
  else{
    swe[(conta1+1):(conta1+dur), ] <- appo[(conta2+1):(conta2+dur), ]
    swe[(conta1+dur+1):(conta1+dur+93), ] <- array(NA_real_, dim = c(93, 6))
    
    conta1 <- conta1 + dur + 93
    conta2 <- conta2 + dur
  }
}


df <- data.frame(
  day  = seq_len(nrow(swe)+1),
  swe1 = c(swe[, 1], NA),
  swe2 = c(swe[, 2], NA),
  swe3 = c(swe[, 3], NA),
  swe4 = c(swe[, 4], NA),
  swe5 = c(swe[, 5], NA),
  swe6 = c(swe[, 6], NA)
)



# Selecting bands to keep
keep_bands <- c("swe4", "swe5", "swe6")

data0 <- as.Date("1991-10-03")
dates <- as.Date(paste0(seq(1991, 2021, by = 3), "-10-03"))
dates_ind <- as.numeric(dates - data0) + 1

band_labels <- c(
  swe4 = "1500 - 2000 m",
  swe5 = "2000 - 2500 m",
  swe6 = "Over 2500 m"
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
  top = textGrob("SWE evolution: above 1500 meters", gp = gpar(fontface = "bold", fontsize = 14))
)
