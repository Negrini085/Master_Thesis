# The main goal of this script is to make sure that my filtering procedure worked 
# well and no series has some NAs in itself
rm(list = ls())
gc()

fname_list <- "../Dataset/model_runs/raw/PCPD/lista"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/DDF_computation/")


# Importing list and creating correct filenames
df <- read.table(fname_list)
file_names <- sub("^D_", "DV_", df$V1)


# Cycle to take care of different repositories
repo_list <- c("PCPD", "TMND", "TMXD")
for(repo in repo_list){
  
  # Cycle across stations
  fname_appo <- paste0("../Dataset/model_runs/hydro/", repo, "/")
  for(name in file_names){
    
    # Checkin if file is void or not
    fname <- paste0(fname_appo, name)
    info <- file.info(fname)
    if(is.na(info$size) || info$size == 0) next
    
    # Checking if there are some missing datas or no
    df_series <- read.table(fname)
    mask <- is.na(as.numeric(df_series$V2)) | as.numeric(df_series$V2) == -90 | is.nan(as.numeric(df_series$V2))
    if(sum(mask, na.rm = TRUE) != 0) print(paste0("Some problems for ", name))
  }
}