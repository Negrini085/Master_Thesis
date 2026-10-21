# The main goal of this script is to compare SWE time series
rm(list = ls())
gc()

fname_model <- "with_ITSNOW/Results/model_swe_series.dat"
fname_to_compare <- "with_ITSNOW/Results/itsnow_swe_series.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_comparison/")



# Importing both time series
df_model <- read.table(fname_model, header = TRUE) 
df_to_compare <- read.table(fname_to_compare, header = TRUE)

common_dates  <- intersect(df_model$dates, df_to_compare$dates)
df_model <- df_model[match(common_dates, df_model$dates), ]
df_to_compare <- df_to_compare[match(common_dates, df_to_compare$dates), ]



# Extracting time series
swe_model <- as.numeric(df_model$higher)
swe_to_compare <- as.numeric(df_to_compare$higher)



# Making statistical analysis
ok  <- complete.cases(swe_to_compare, swe_model)
swe_to_compare <- swe_to_compare[ok]; swe_model <- swe_model[ok]
err <- swe_model - swe_to_compare

n <- length(swe_to_compare)
bias <- mean(err)
mae <- mean(abs(err))
rmse <- sqrt(mean(err^2))
r <- cor(swe_model, swe_to_compare)
r2 <- r^2


cat("Bias: ", bias, "\n")
cat("Mean absolute error: ", mae, "\n")
cat("Root mean squared error: ", rmse, "\n")
cat("R2: ", r2, "\n")