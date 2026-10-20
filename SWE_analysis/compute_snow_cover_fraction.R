# The main goal of this script is to compute snow cover changes as a function of 
# elevation over the investigated period.
rm(list = ls())
gc()

library(terra)

lim_inf <- seq(1952, 1992, by = 10)
lim_sup <- seq(1981, 2021, by = 10)
fname_dem <- "DEM/DEM_compatible.tif"
month_out <- c("dec", "jan", "feb", "mar")
required_dates <- c("-12-01", "-01-01", "-02-01", "-03-01")
lim_sup_dates <- c("-12-31", "-01-31", "-02-28", "-03-31")
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")


# Funtion to compute the snow covered pixel fraction at each elevation band
snow_fraction_by_band <- function(scd_maps, zones, n_bands){
  
  appo <- sum(scd_maps)
  
  v_snow <- values(appo)[, 1]
  v_band <- values(zones)[, 1]
  
  frac <- rep(NA, n_bands)
  for(b in 1:n_bands){
    
    sel <- which(v_band == b & !is.na(v_snow))
    if(length(sel) > 0){
      frac[b] <- mean(v_snow[sel])
    }
  }
  return(frac)
}



# Importing dem and classifying over elevation bands
dem <- rast(fname_dem)
breaks  <- c(-Inf, seq(50, 3500, 50))
n_bands <- length(breaks) - 1
band_labels <- c("<=50", paste0(breaks[2:n_bands], "-", breaks[3:(n_bands+1)]))

zones <- classify(dem, cbind(breaks[-(n_bands + 1)], breaks[-1], 1:n_bands), right = TRUE, others = NA)
names(zones) <- "band"



# Cycle over dates
for(date in required_dates){
  
  k <- which(required_dates == date)
  appo_fractions <- array(0, dim = c(length(lim_inf), n_bands))
  for(i in seq_along(lim_inf)){
    
    # Selecting time window
    min_year <- lim_inf[i]
    max_year <- lim_sup[i]
    
    # Cycle over investigated time-window
    for(y in min_year:max_year){
      
      # Selecting which julian year to consider
      appo_y <- y
      if(date == "-12-01") appo_y <- appo_y - 1
      
      # Selecting map index
      idx <- which(seq(as.Date(paste0(appo_y, "-01-01")), as.Date(paste0(appo_y, "-12-31")), by = "day") %in% seq(as.Date(paste0(appo_y, date)), as.Date(paste0(appo_y, lim_sup_dates[k])), by = "day"))
      fnames <- paste0("Dataset/SC/lim_15/SC_", appo_y ,".nc")
      scd_maps <- rast(fnames)[[idx]]
      
      appo_fractions[i, ] <- appo_fractions[i, ] + snow_fraction_by_band(scd_maps, zones, n_bands)/length(idx)
    }
    
    cat("Done climatology number ", i, " for date ", date, "\n")
  }
  
  appo_fractions <- appo_fractions/30
  rownames(appo_fractions) <- paste0(lim_inf, "-", lim_sup)
  colnames(appo_fractions) <- band_labels
  
  # Building output file name
  fname_out <- paste0("Results/SCD/snow_fraction_climatologies_", month_out[k], ".dat")
  write.table(appo_fractions, fname_out, sep = "\t", quote = FALSE, col.names = NA)
}