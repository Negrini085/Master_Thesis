# The main goal of this script is to find the best parameters from SA. This will 
# enable the user to work in the best enviromental conditions possible.
rm(list = ls())
gc()

fname_loss <- "Runs/ddf_param/loss_evo_"
fname_param <- "Runs/ddf_param/param_evo_"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_calibration/")


# Importing loss values and finding the minimum value
df <- read.table(paste0(fname_loss, 1, ".dat"), header = FALSE)
appo_loss <- array(NA_real_, dim = c(5, nrow(df)))
rm(df); invisible(gc())

for(i in 1:5){
  df <- read.table(paste0(fname_loss, i, ".dat"), header = FALSE)
  appo_loss[i, ] <- as.numeric(df$V1)
}

if(any(is.na(appo_loss))) stop("Some NA are still in loss container")
min_loss <- min(appo_loss, na.rm = TRUE)
min_idx <- which(appo_loss == min_loss, arr.ind = TRUE)

print(min_idx[, 1])

# Importing parameter files
for(i in seq_along(min_idx[, 1])){
  df <- read.table(paste0(fname_param, unname(min_idx[i, 1]), ".dat"), header = FALSE)
  print(df[unname(min_idx[i, 2]), ])
}