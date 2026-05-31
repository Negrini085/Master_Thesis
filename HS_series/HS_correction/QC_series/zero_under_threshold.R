# The main goalo this script is to put to zero every HS data which is smaller than
# a given threshold (it may vary depending on the station taken into account).
rm(list = ls())
gc()

thresh <- 5
name <- "HSD_TAA_TN_VERMIGLIO_CAPANNA_PRESENA"
fname <- paste0("Dataset/Tmp/", name)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/HS_correction/QC_series/")


# Importing HS series
df <- read.table(fname)
appo_y <- as.numeric(df$V1)
appo_hs <- as.numeric(df$V2)


# Setting to zero under a given HS threshold
mask <- appo_hs <= thresh
appo_hs[mask] <- 0



# Saving new series to file
df_print <- data.frame(
  years = appo_y, 
  hs_series = appo_hs
)

write.table(df_print, paste0("Dataset/Tmp/", name), row.names = FALSE, col.names = FALSE, quote = FALSE)