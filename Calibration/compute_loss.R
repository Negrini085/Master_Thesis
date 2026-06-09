# The main goal of this script is to compute loss function values for SA optimization.
rm(list = ls())
invisible(gc())

library(parallel)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Calibration/")

process_station <- function(name) {
  TP <- 0L; TN <- 0L; FP <- 0L; FN <- 0L
  
  # Importing series
  fname_MINE <- paste0("Results/hydro/", sub("HSD", "DV_SDH", name))
  df_MINE    <- read.table(fname_MINE, header = FALSE)
  
  fname_HS   <- paste0("../HS_series/Correct/Dataset/", name)
  df_HS      <- read.table(fname_HS, header = FALSE)
  years      <- unique(as.numeric(df_HS$V1))
  
  for (y in years) {
    if (y > 2023) next
    
    appo_MINE <- as.numeric(df_MINE$V2)[as.numeric(df_MINE$V1) == y]
    appo_HS   <- as.numeric(df_HS$V2)[as.numeric(df_HS$V1)   == y]
    
    if (length(appo_MINE) != length(appo_HS))
      stop(paste0("Incompatible lengths at ", name, ", year ", y))
    
    TP <- TP + sum(appo_MINE >  0 & appo_HS >  0, na.rm = TRUE)
    FP <- FP + sum(appo_MINE >  0 & appo_HS == 0, na.rm = TRUE)
    FN <- FN + sum(appo_MINE == 0 & appo_HS >  0, na.rm = TRUE)
    TN <- TN + sum(appo_MINE == 0 & appo_HS == 0, na.rm = TRUE)
  }
  
  c(TP = TP, TN = TN, FP = FP, FN = FN)
}


# Importing station names
files      <- list.files(path = "Results/hydro", full.names = TRUE)
names_swe  <- sub("Results/hydro/DV_SDH", "HSD", files)

fname      <- "../HS_series/Correct/STATION_check/Dataset/ANAGRAFICA"
df         <- read.table(fname, header = TRUE)
names_hs   <- df$name

station_names <- intersect(names_swe, names_hs)


# Parallel execution
n_cores <- 8
results <- mclapply(station_names, process_station, mc.cores = n_cores)

failed <- sapply(results, inherits, "try-error")
if (any(failed))
  stop("Error in stations: ", paste(station_names[failed], collapse = ", "))


# Evaluating loss
totals <- Reduce("+", results)
loss <- 1 - totals["TP"] / (totals["TP"] + totals["FP"] + totals["FN"])
write.table(data.frame(loss = loss), "appo_loss.dat",row.names = FALSE, col.names = FALSE, quote = FALSE)