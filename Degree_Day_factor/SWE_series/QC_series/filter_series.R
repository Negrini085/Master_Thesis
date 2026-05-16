# The main goal of this script is to filter series for a given station, in order 
# to omit those hydrological years which we wanted to delete.
rm(list = ls())
gc()


sta <- "HSD_AT_104174"
appo_omit_years <- c(1993, 2004)
fname_gap <- paste0("../Dataset/hs_series/with_gaps/", sta)
fname_com <- paste0("../Dataset/hs_series/all_complete/", sta)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/QC_series/")

# Function to import series
import_series <- function(fname_gap, fname_com, sta){
  
  if(file.exists(fname_com) & file.exists(fname_gap)){
    df_com <- read.table(fname_com)
    df_gap <- read.table(fname_gap)
    
    df <- NULL 
    df_appo <- rbind(df_com, df_gap)
    for(y in sort(unique(as.numeric(df_appo$V1)))){
      mask <- as.numeric(df_appo$V1) == y
      df <- rbind(df, df_appo[mask, ])
    }
    
  }
  else if(file.exists(fname_com)) df <- read.table(fname_com)
  else if(file.exists(fname_gap)) df <- read.table(fname_gap)
  
  if(!file.exists(fname_com) & !file.exists(fname_gap)) stop(paste0("No hs series file for ", sta))
  return(df)
}



# Importing df
df <- import_series(fname_gap = fname_gap, fname_com = fname_com, sta)
if(!is.null(appo_omit_years)){
  mask <- !(as.numeric(df$V1) %in% appo_omit_years)
  df <- df[mask, ]
}

if(nrow(df) == 0) stop(paste0("No rows left for ", sta))
write.table(df, paste0("Dataset/Filtered/", sta), row.names = FALSE, col.names = FALSE, quote = FALSE)