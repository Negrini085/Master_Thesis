# The main goal of this script is to plot how zero-snow condition is handled 
# differently for different stations or even different hydrological years.
rm(list = ls())
gc()

library(ggplot2)
library(dplyr)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/FindBug/")


# Importing MALGA TERLAGO datas and selecting zeros
fname <- "../STATION_series/Datas/station_series/HSD_TAA_TN_PAGANELLA_MALGA_TERLAGO"
df <- read.table(fname, header = FALSE)
hs_malga_terlago <- df$V2[669:730]


# Importing GRIGNO BARRICATA datas and selecting zeros
fname <- "../STATION_series/Datas/station_series/HSD_TAA_TN_GRIGNO_BARRICATA"
df <- read.table(fname, header = FALSE)
hs_grigno <- df$V2[304:365]


# Importing PASSO BROCON datas and selecting zeros
fname <- "../STATION_series/Datas/station_series/HSD_TAA_TN_PASSO_BROCON"
df <- read.table(fname, header = FALSE)
hs_brocon <- df$V2[1765:1826]


# Importing DOS DEL SABION datas and selecting zeros
fname <- "../STATION_series/Datas/station_series/HSD_TAA_TN_DOS_DEL_SABION_MONTE_GRUAL"
df <- read.table(fname, header = FALSE)
hs_sabion <- df$V2[304:365]


# Importing PASSO VALLES datas and selecting zeros
fname <- "../STATION_series/Datas/station_series/HSD_TAA_TN_PASSO_VALLES"
df <- read.table(fname, header = FALSE)
hs_valles <- df$V2[670:731]


# Importing MALGA DAONE datas and selecting zeros
fname <- "../STATION_series/Datas/station_series/HSD_TAA_TN_DAONE_MALGA_BISSINA"
df <- read.table(fname, header = FALSE)
hs_daone <- df$V2[305:366]





# Plotting procedure
series_list <- list(hs_malga_terlago, hs_grigno, hs_brocon,
                    hs_sabion, hs_valles, hs_daone)

titles <- c("Malga Terlago 2018", "Grigno Barricata 2013", "Passo Brocon 2018", 
            "Dos del Sabion 2013", "Passo Valles 2013", "Malga Daone 2012")

df_plot <- bind_rows(
  lapply(seq_along(series_list), function(i) {
    data.frame(
      day    = seq_along(series_list[[i]]),
      hs     = series_list[[i]],
      station = titles[i]
    )
  })
)

df_plot$station <- factor(df_plot$station, levels = titles)

x_ticks  <- c(1, 16, 32, 47, 62)
x_labels <- c("1 Jul", "16 Jul", "1 Aug", "16 Aug", "31 Aug")

ggplot(df_plot, aes(x = day, y = hs)) +
  geom_line(color = "steelblue", linewidth = 0.7) +
  facet_wrap(~ station, ncol = 2) +
  scale_x_continuous(breaks = x_ticks, labels = x_labels) +
  labs(x = NULL, y = "HS [cm]") +
  theme_bw() +
  theme(
    strip.text       = element_text(face = "bold", size = 18),
    strip.background = element_blank(),
    axis.text.x      = element_text(angle = 30, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.border     = element_blank()
  )