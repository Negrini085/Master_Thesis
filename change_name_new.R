rm(list = ls())
gc()

fname_old <- "Dataset/NEW"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/What/")

df <- read.table(fname_old, header = FALSE)
names <- df$V1
repo <- "SNW/"

for(name in names){
  fname_old <- paste0("Dataset/", repo, sub("HSD", "DV_SNW", name))
  fname_new <- paste0("Dataset/", repo, sub("HSD", "DV_SNW", name), "_MAN")
  
  if(file.exists(fname_old)){
    file.rename(fname_old, fname_new)
  } else {
    warning("Missing file: ", fname_old)
  }
}