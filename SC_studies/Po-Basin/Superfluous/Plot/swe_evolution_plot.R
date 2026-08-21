# The main goal of this script is to plot SWE total volume across the whole
# investigated period, in order to show its seasonal variability.
rm(list=ls())
gc()

library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/Po-Basin")


# We first read the table. In order to have a correct plot, we need to take into
# account that there are 93 days which are not considered in the SWE GeoTif 
# for every year, considering that the study period for a single year starts on
# the third of October and ends on the first on July.
appo <- read.table("Datas/swe_evolution.dat")
appo <- appo$V1

years <- 1992:2021
dur <- 272
com <- 93

# Creating a whole new time series with NAs for those 93 days.
conta <- 0
swe_evolution <- NULL
for(y in years){
  if(y == 1992){
    swe_evolution <- appo[1:(dur+1)]
    conta <- conta + dur + 1
  }
  else if(y%%4 == 0){
    swe_evolution <- c(swe_evolution, appo[(conta+1):(conta+dur+1)])
    conta <- conta + dur + 1
  }
  else{
    swe_evolution <- c(swe_evolution, appo[(conta+1):(conta+dur)])
    conta <- conta + dur
  }
  
  fill <- rep(NA_real_, 93)
  swe_evolution <- c(swe_evolution, fill)
  
}


# Creating dataframe and plotting
df <- data.frame(
  day = 1:length(swe_evolution), 
  swe = swe_evolution
)

data0 <- as.Date("1991-10-03")
dates <- as.Date(paste0(seq(1992, 2021, by = 4), "-01-01"))
dates_ind <- as.numeric(dates - data0) + 1


p <- ggplot(df, aes(x = day, y = swe)) +
  geom_line(
    linewidth = 1.2,
    colour = "#1F4E79",
    lineend = "round"
  ) +
  scale_x_continuous(
    breaks = dates_ind,
    labels = format(dates, "%Y"),
    expand = expansion(mult = c(0.005, 0.005))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.04))
  ) +
  labs(
    x = "Date",
    y = expression(bold("SWE [Gm"^3*"]"))
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(
      family = "sans",
      colour = "black"
    ),
    axis.title = element_text(
      size = 22,
      face = "bold"
    ),
    axis.title.x = element_text(
      margin = margin(t = 15)
    ),
    axis.title.y = element_text(
      margin = margin(r = 15)
    ),
    axis.text = element_text(
      size = 18,
      colour = "black"
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    axis.line = element_line(
      linewidth = 0.6,
      colour = "black"
    ),
    axis.ticks = element_line(
      linewidth = 0.5,
      colour = "black"
    ),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major.x = element_line(
      colour = "grey80",
      linewidth = 0.35
    ),
    panel.grid.major.y = element_line(
      colour = "grey80",
      linewidth = 0.35
    ),
    panel.grid.minor = element_blank(),
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5
    )
  )

print(p)

ggsave(
 "Images/swe_evolution.png",
 plot = p,
 width = 14,
 height = 6,
 units = "in",
 dpi = 600
)