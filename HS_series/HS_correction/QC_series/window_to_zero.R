# The main goal of this script is to set to zero the summer window, in order to 
# get rid of some noise which makes our HS series worst.
rm(list = ls())
gc()

name <- "HSD_TAA_TN_VERMIGLIO_CAPANNA_PRESENA"
end_idx <- 15
start_idx <- 318
fname <- paste0("Dataset/Filtered/", name)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/HS_correction/QC_series/")



# Importing HS series
df <- read.table(fname)
appo_years <- as.numeric(df$V1)



# Cycle over years
years <- numeric(0)
hs_series <- numeric(0)
for(y in unique(appo_years)){
  
  if(y == 2024) next
  
  # Selecting given year
  mask <- as.numeric(appo_years) == y
  appo_hs <- as.numeric(df$V2)[mask]
  if(all(is.na(appo_hs) | appo_hs == 0)) next
  
  # Setting to zero hs datas inside summer window
  if(start_idx > end_idx){
    appo_hs[1:end_idx] <- 0
    appo_hs[start_idx:length(appo_hs)] <- 0
  }
  else appo_hs[start_idx:end_idx] <- 0
  
  # Saving correct hs series
  hs_series <- c(hs_series, appo_hs)
  years <- c(years, rep(y, length(appo_hs)))
}

if(length(hs_series) != length(years)) stop("Non compatible length for ", name)



# Saving data-frame to file
df_print <- data.frame(
  years = years, 
  hs_series = hs_series
)


if(any(is.na(hs_series))){
  print("Still some steps to do!")
  fout <- paste0("Dataset/ToFix/", name)
} else { 
  print("Everything should be good!")
  fout <- paste0("Dataset/Tmp/", name)
}

if(nrow(df_print) == 0) stop(paste0("No rows left for ", name))
write.table(df_print, fout, row.names = FALSE, col.names = FALSE, quote = FALSE)