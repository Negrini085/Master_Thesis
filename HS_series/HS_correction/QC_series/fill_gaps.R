# The main goal of this script is to fill gaps via linear interpolation (only when
# those missing datas are within both non-zero values or both zeros). 
rm(list = ls())
gc()

name <- "HSD_CH_SLFAL1"
fname <- paste0("Dataset/ToFix/", name)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/HS_correction/QC_series/")



# Function to fill gaps
fill_gaps <- function(x) {
  n <- length(x)
  
  # Finding NA positions
  na_pos <- which(is.na(x))
  if (length(na_pos) == 0) return(x)
  
  # Finding gaps (which can be made of one or more contiguous NAs)
  gaps <- list()
  i <- 1
  while (i <= length(na_pos)) {
    gap_start <- na_pos[i]
    gap_end   <- na_pos[i]
    
    # Expanding gap untill a number is found
    while (i + 1 <= length(na_pos) && na_pos[i + 1] == na_pos[i] + 1) {
      i <- i + 1
      gap_end <- na_pos[i]
    }
    gaps[[length(gaps) + 1]] <- c(gap_start, gap_end)
    i <- i + 1
  }
  
  # Filling every gap
  for (g in gaps) {
    
    # Known value index
    left_idx  <- g[1] - 1
    right_idx <- g[2] + 1
    
    # Not filling gaps on border of the year
    if (left_idx < 1 || right_idx > n) next
    
    left_val  <- x[left_idx]
    right_val <- x[right_idx]
    if (is.na(left_val) || is.na(right_val)) next
    
    gap_len <- g[2] - g[1] + 1
    
    # Gap-filling only if both limits are of the same class
    if (left_val == 0 && right_val == 0) x[g[1]:g[2]] <- 0
    else if (left_val != 0 && right_val != 0) {
      x[g[1]:g[2]] <- approx(
        x   = c(left_idx, right_idx),
        y   = c(left_val, right_val),
        xout = g[1]:g[2]
      )$y
    }
  }
  
  return(x)
}



# Importing HS series
df <- read.table(fname)
appo_y <- as.numeric(df$V1)
appo_hs <- as.numeric(df$V2)


# Cycle in order to asses if some years are only made of NAs or zeros
years <- numeric(0)
hs_series <- numeric(0)
for(y in unique(appo_y)){
  
  if(y == 2024) next
  
  # Selecting datas for a given hydrological year
  mask <- appo_y == y
  appo <- appo_hs[mask]
  
  # Checking if it's only made of zeros or NAs
  mask <- is.na(appo) | appo == 0
  if(all(mask)) next
  else{
    appo <- fill_gaps(appo)
    years <- c(years, rep(y, length(appo)))
    hs_series <- c(hs_series, appo)
  }
}


# Saving series to tmp directory
df_print <- data.frame(
  years = years, 
  hs_series = hs_series
)

if(any(is.na(hs_series))){
  print("Still some gaps!")
  write.table(df_print, paste0("Dataset/Gap/", name), row.names = FALSE, col.names = FALSE, quote = FALSE)
} else { 
  print("Recovered whole series!")
  write.table(df_print, paste0("Dataset/Tmp/", name), row.names = FALSE, col.names = FALSE, quote = FALSE)
}