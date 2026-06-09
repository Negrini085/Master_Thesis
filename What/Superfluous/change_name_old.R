rm(list = ls())
gc()

fname_old <- "Dataset/OLD"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/What/")

df <- read.table(fname_old, header = FALSE)
names <- df$V1
repo <- "SNW/"

for(name in names){
  fname <- paste0("Dataset/", repo, sub("HSD", "DV_SNW", name))
  df <- read.table(fname, header = FALSE)
  
  write.table(df, paste0("Dataset/", repo, sub("HSD", "DV_SNW", name), "_MAN"), row.names = FALSE, col.names = FALSE, quote = FALSE)
}