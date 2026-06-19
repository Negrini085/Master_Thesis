# The main goal of this script is to develop a toy model to create SWE series. We 
# will first check for compatibility with the SWE series that were given to me by
# Michele, and then I will try to optimize the model input parameters via a simulated
# annealing procedure.
rm(list = ls())
invisible(gc())

library(parallel)

fname_in <- "input.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Calibration/")

n_cores <- 8
# cat("Using", n_cores, "cores\n\n")


# The following function will be used to compute solid precipitations, which is a 
# key component of snow accumulation. The idea is that if both minimum and maximum
# day temperature are below a certain threshold than all precipitation is treated 
# as solid. On the other hand if both are above that threshold according to the model
# is raining. At last, the in-between situations leads to a mixture of solid and 
# liquid precipitation.
solid_prec <- function(prec, tmax, tmin, t_th, name){
  
  # Checking container length
  if(length(prec) != length(tmax)) stop(paste0("Non compatible length for precipitation and temperature at ", name))
  if(length(tmin) != length(tmax)) stop(paste0("Non compatible length for min and max temperature at ", name))
  
  # Checking if tmin is always smaller than tmax 
  mask <- tmin < tmax
  if(!all(mask)) stop(paste0("Some Tmax are smaller than Tmin at ", name))
  
  # Checking if precipitations are always non-negative
  mask <- prec >= 0
  if(!all(mask)) stop(paste0("Some days have negative precipitations at ", name))
  
  # Creating solid precipitation array
  solid <- rep(0, length(prec))
  
  # Solid only
  mask <- tmax < t_th & !is.na(prec) & prec > 0
  solid[mask] <- prec[mask]
  
  # Mixture
  mask <- tmin <= t_th & tmax > t_th & !is.na(prec) & prec > 0
  solid[mask] <- prec[mask]*(t_th - tmin[mask])/(tmax[mask] - tmin[mask])
  
  return(solid)
}


# Function to compute DDF as a day-dependent value, with a period of one year
compute_ddf <- function(year, ddf_min, ddf_max){
  
  len <- 365
  if((year %% 4 == 0 & year %% 100 != 0) | year %% 400 == 0) len <- 366
  idx <- 1:len
  
  ddf <- (ddf_max + ddf_min)/2 + (ddf_max - ddf_min)/2 * sin(2 * pi * (idx - 81)/len)
  return(ddf)
}


# Function to compute degree day in order to model melt.
compute_degree_days <- function(tmax, tmin, name){
  
  # Checking dataset quality
  if(length(tmin) != length(tmax)) stop(paste0("Non compatible length for min and max temperature at ", name))
  if(!all(tmin < tmax, na.rm = TRUE)) stop(paste0("Some Tmax are smaller than Tmin at ", name))
  
  # Mean temperature 
  tmean <- (tmin + tmax)/2
  deg_day <-rep(0, length(tmean))
  
  # Degree day computation
  mask <- tmean > 0
  deg_day[mask] <- tmean[mask]
  
  return(deg_day)
}


# Function to compute actual swe series for a given station. We will start to 
# accumulate on the 1st of January in 1950.
swe_series <- function(name, t_th, ddf_min, ddf_max){
  
  # Importing temperature and precipitation series
  df_prec <- read.table(paste0("Dataset/PCPD/DV_", name), header = FALSE)
  month <- as.numeric(df_prec$V2)
  prec <- as.numeric(df_prec$V5)
  year <- as.numeric(df_prec$V1)
  day <- as.numeric(df_prec$V3)
  
  df_tmin <- read.table(paste0("Dataset/TMND/DV_", name), header = FALSE)
  mask <- as.numeric(df_tmin$V1) %in% unique(year)
  tmin <- as.numeric(df_tmin$V5)[mask]
  
  df_tmax <- read.table(paste0("Dataset/TMXD/DV_", name), header = FALSE)
  mask <- as.numeric(df_tmax$V1) %in% unique(year)
  tmax <- as.numeric(df_tmax$V5)[mask]
  
  
  # Checking if there are some NAs
  if(any(is.na(tmin))) stop(paste0("Some NAs in Tmin at ", name))
  if(any(is.na(tmax))) stop(paste0("Some NAs in Tmax at ", name))
  if(any(is.na(prec))) stop(paste0("Some NAs in precipitation at ", name))
  
  if(!all(tmin < tmax, na.rm = TRUE)){
    warning(paste0("Some Tmax are smaller than Tmin at ", name))
    return(0)
  }
  
  if(any(prec < 0, na.rm = TRUE)){
    warning(paste0("Some precipitation are negative for ", name))
    return(0)
  }
  
  if(length(tmin) != length(tmax)) stop(paste0("Incompatible length for temperature series at ", name))
  if(length(tmin) != length(prec)) stop(paste0("Incompatible length for temperature and precipitation series at ", name))
  
  
  # Computing solid precipitation and degree days
  prec_s <- solid_prec(prec = prec, tmax = tmax, tmin = tmin, t_th = t_th, name = name)
  df_precs <- data.frame(year = year, value = prec_s)
  
  degree_day <- compute_degree_days(tmax = tmax, tmin = tmin, name = name)
  df_dd <- data.frame(year = year, value = degree_day)
  
  
  # Cycle over years
  idx <- 1
  appo <- 0
  swe_appo <- array(NA_real_, dim = c(length(tmin)))
  for(y in sort(unique(year))){
    
    # Selecting values for a given year
    mask <- as.numeric(df_precs$year) == y
    precs_year <- as.numeric(df_precs$value)[mask]
    
    mask <- as.numeric(df_dd$year) == y
    dd_year <- as.numeric(df_dd$value)[mask]
    
    
    # DDF computation
    ddf <- compute_ddf(year = y, ddf_min = ddf_min, ddf_max = ddf_max)
    if(length(precs_year) != length(dd_year)) stop("Non compatible length for solid precipitation and dd at ", name, " during ", y)
    if(length(ddf) != length(dd_year)) stop("Non compatible length for dd and ddf at ", name, " during ", y)
    
    
    # Cycle over days of a given year
    for(i in 1:length(ddf)){
      appo <- appo + precs_year[i]
      if(appo > 0){
        melt <- ddf[i] * dd_year[i]
        appo <- appo - melt
        
        if(appo < 0) appo <- 0
      }
      swe_appo[idx] <- appo
      idx <- idx + 1
    }
  }
  
  if(any(is.na(swe_appo))) stop(paste0("Some NAs in SWE series at ", name))
  if(length(swe_appo) != length(tmin)) stop(paste0("Non compatible lenght for swe and other series at ", name))
  
  df_swe <- data.frame(year = year, month = month, day = day, swe = swe_appo)
  write.table(df_swe, paste0("Results/raw/", sub("HSD", "DV_SDH", name)), row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  rm(tmin, tmax, prec, prec_s, degree_day, swe_appo)
  gc()
  
  rm(df_swe, df_dd, df_precs, df_tmax, df_tmin, df_prec)
  gc()
  
  return(1)
}










# Importing input values and station names
df_in <- read.table(fname_in, header = TRUE)
ddf_min <- df_in$ddf_min
ddf_max <- df_in$ddf_max
t_th <- df_in$tlim

files <- list.files(path = "Dataset/PCPD", full.names = TRUE)
files <- sub("Dataset/PCPD/DV_", "", files)


# Actual swe computation
results <- mclapply(files, function(name) {
  appo <- swe_series(name  = name, t_th  = t_th, ddf_min = ddf_min, ddf_max = ddf_max)
  if (appo == 1) message("Made swe computations for ", name)
  return(appo)
}, mc.cores = n_cores)
