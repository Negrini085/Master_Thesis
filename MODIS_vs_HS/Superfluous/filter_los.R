# The main goal of this script is to filter los values in order to later plot
# hs series for those stations which seems highly biased.
rm(list = ls())
gc()

fname_los <- "Results/compare_los.dat"
fname <- "Results/only_over_five_HS_MODIS_diff.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/MODIS_vs_HS/")


# Importing bias values (and filtering for values higher than 0.5)
df <- read.table(fname, header = TRUE)
mask <- abs(as.numeric(df$diff)) > 0.5
df <- df[mask, ]
names <- df$name
rm(df)
gc()


# Importing los values
df <- read.table(fname_los, header = TRUE)
mask <- df$name %in% names
df <- df[mask, ]

write.table(df, "Results/filtered_los.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)