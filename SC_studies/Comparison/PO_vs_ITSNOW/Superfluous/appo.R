# The main goal of this script is to make SWE evolution plots comparing ITSNOW 
# and Po District in order to show which one is best
rm(list = ls())
gc()

library(ggplot2)
library(tidyr)
library(dplyr)

fname_PO <- "Dataset/total_swe_PO.dat"
fname_ITSNOW <- "Dataset/total_swe_ITSNOW.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/Comparison/PO_vs_ITSNOW/")


# Importing both SWE evolution files in order to make a comparison
po <- read.table(fname_PO, header = TRUE)
itsnow <- read.table(fname_ITSNOW, header = TRUE)
itsnow[is.na(itsnow$lower), 3] <- 0
itsnow[is.na(itsnow$medium), 4] <- 0
itsnow[is.na(itsnow$higher), 5] <- 0

po$dates     <- as.Date(po$dates)
itsnow$dates <- as.Date(itsnow$dates)

cmp <- merge(po, itsnow, by = "dates", all = TRUE, suffixes = c("_po", "_itsnow"))
cmp <- cmp[order(cmp$dates), ]
rownames(cmp) <- NULL



cmp_long <- cmp %>%
  select(dates, swe_po, swe_itsnow) %>%
  pivot_longer(-dates, names_to = "source", values_to = "swe") %>%
  mutate(source = recode(source,
                         swe_po     = "Po District",
                         swe_itsnow = "ITSNOW"))

ggplot(cmp_long, aes(dates, swe, colour = source)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c("Po District" = "#1f78b4",
                                 "ITSNOW"      = "#e31a1c")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(x = NULL, y = "SWE (mm w.e.)", colour = NULL,
       title = "SWE evolution: Po District vs ITSNOW") +
  theme_bw() +
  theme(legend.position = "top")
