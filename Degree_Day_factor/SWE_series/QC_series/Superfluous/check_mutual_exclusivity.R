# The main goal here is to check whether complete years dataset and with gap dataset
# are mutually exclusive as they should be. If that's not the case there must be some
# problems with our procedure.
rm(list = ls())
gc()

fname_with_gaps <- "../Dataset/hs_series/with_gaps/with_gaps.dat"
fname_complete <- "../Dataset/hs_series/all_complete/all_complete.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/QC_series/")


# Importing both dataframe
df_complete <- read.table(fname_complete)
df_with_gaps <- read.table(fname_with_gaps)


# Checking if there are some common rows
common_rows <- intersect(df_complete, df_with_gaps)
print(paste0("Number of complete hydrological years: ", nrow(df_complete)))
print(paste0("Number of hydrological years with gaps: ", nrow(df_with_gaps)))
print(paste0("Number of common  hydrological years: ", length(common_rows)))