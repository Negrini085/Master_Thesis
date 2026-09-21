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

mask <- !is.na(cmp$swe_po)
cmp <- cmp[mask, ]


d <- cmp$lower_po - cmp$lower_itsnow

MD     <- mean(d, na.rm = TRUE)
MAE    <- mean(abs(d), na.rm = TRUE)
RMSD   <- sqrt(mean(d^2, na.rm = TRUE))
R      <- cor(cmp$lower_po, cmp$lower_itsnow, use = "complete.obs")
R2     <- R^2

print(MD)
print(MAE)
print(RMSD)
print(R2)

write.table(data.frame(appo = cmp$lower_itsnow), "appo.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)