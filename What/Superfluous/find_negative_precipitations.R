# The main goal of this script is to find negative precipitation datas, in order 
# to later signal those to Michele et al.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/What/")


# Variables
files <- list.files(path = "Dataset/PCPD", full.names = TRUE)
appo_names <- character(0)
appo_value <- numeric(0)
appo_month <- numeric(0)
appo_year <- numeric(0)
appo_day <- numeric(0)


# Cycle over precipitation dataset
for(name in files){
  
  # Importing precipitation series and looking for negative precipitations
  df <- read.table(name)
  day <- as.numeric(df$V3)
  year <- as.numeric(df$V1)
  month <- as.numeric(df$V2)
  value <- as.numeric(df$V5)
  mask <- value < 0
  
  if(any(mask)){
    cat(sub("Dataset/PCPD/DV_", "", name), "\n")
    for(i in which(mask)){
      appo_names <- c(appo_names, sub("Dataset/PCPD/DV_", "", name))
      appo_value <- c(appo_value, value[i])
      appo_month <- c(appo_month, month[i])
      appo_year <- c(appo_year, year[i])
      appo_day <- c(appo_day, day[i])
      
      cat(year[i], month[i], day[i], "\n")
    }
    cat("\n")
  }
}


df_print <- data.frame(
  name = appo_names, 
  year = appo_year,
  month = appo_month,
  day = appo_day,
  value = appo_value
)
write.table(df_print, "Results/negative_prec.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)