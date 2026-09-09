# The main goal of this script is to crop temperature dataset to the extent of the precipitation ones.
rm(list = ls())
gc()

library(terra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/")

outdir <- "Input/TEMP"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

years <- 1951:2023
vars  <- c("tmxd", "tmd", "tmnd")

for (y in years) {
  fin <- sprintf("../../Backup/TEMP/temperatures_%d.nc", y)
  if (!file.exists(fin)) { warning("Not existing! ", fin); next }
  
  prec <- rast(sprintf("Input/PCPD/%d.nc", y), subds = "total_precipitation")
  
  lst <- lapply(vars, function(v) {
    r <- rast(fin, subds = v)
    r <- crop(r, ext(prec))
    stopifnot(compareGeom(r, prec, stopOnError = FALSE))
    stopifnot(nlyr(r) == nlyr(prec))
    r <- mask(r, prec)
    r
  })
  
  x <- sds(lst)
  names(x) <- vars
  
  writeCDF(x,
           filename = file.path(outdir, sprintf("%d.nc", y)),
           unit = "degree_Celsius",
           longname = c("Daily maximum air temperature at 2 metres",
                        "Daily mean air temperature at 2 metres",
                        "Daily minimum air temperature at 2 metres"),
           compression = 4,
           overwrite   = TRUE)
  
  cat("Done ", y, "\n")
  
  rm(x, lst, prec)
  gc()
}

