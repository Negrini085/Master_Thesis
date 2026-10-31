# The main goal of this script is to evaluate the agreement between SWE series.
rm(list = ls())
gc()

fname_po <- "Results/po_basin_swe_series.dat"
fname_model <- "Results/model_swe_series.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_comparison/with_POBasin/")


# Importing series to compare
df_po <- read.table(fname_po, header = TRUE)
swe_po <- as.numeric(df_po$higher)

df_model <- read.table(fname_model, header = TRUE)
swe_model <- as.numeric(df_model$higher)





# Checking length and removing couples in which at least one is not assigned
mask <- df_model$dates %in% df_po$dates
swe_model <- swe_model[mask]

stopifnot(length(swe_po) == length(swe_model))
ok  <- complete.cases(swe_po, swe_model)

ref <- swe_model[ok]
est <- swe_po[ok]
err <- est - ref





# Computing metrics
n <- length(err)
bias <- mean(err)
mae    <- mean(abs(err))
rmse   <- sqrt(mean(err^2))
r2_cor <- cor(est, ref)^2
r2_det <- 1 - sum(err^2) / sum((ref - mean(ref))^2)   # R² rispetto al riferimento (= NSE)

metrics <- data.frame(n, bias, mae, rmse, r2_cor, r2_det)
print(round(metrics, 3))