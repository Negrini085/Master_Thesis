# This is the implementation of a degree-day based model to evaluate snow water
# equivalent. Total precipitations, as well as minimum and maximum temperature on a 
# daily basis are required as inputs. The model describes separately the two processes 
# that govern the snowpack mass balance at the ground: melt and accumulation.
#
# Snowmelt is computed using a degree-day approach: daily melt is assumed proportional 
# to the excess of temperature above a melting threshold, multiplied by a degree-day factor (DDF). 
# Unlike the classical formulation, which treats the DDF as constant, here the factor is 
# allowed to vary seasonally following the approach proposed by Magnusson, which accounts for 
# seasonal changes in the radiative balance.
#
# Accumulation, i.e. the fraction of precipitation falling as solid precipitation (snow), is 
# instead estimated through a temperature threshold model. For each day i, the minimum (Tn) and 
# maximum (Tx) temperatures are compared a gainst a threshold temperature th: if both exceed the 
# threshold, all precipitation P is considered liquid and the solid component SP is zero; if both 
# are below or equal to the threshold, all precipitation is considered snow (SP = P); in the 
# intermediate case a mixed solid/liquid condition is assumed.
rm(list = ls())
gc()

library(ncdf4)

years <- 1945:2023
fname_in <- "input.dat"
fname_water_mask <- "Dataset/water_mask.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/")



# Function to check whether two netCDF variables are defined on the same spatial 
# grid, comparing directly the coordinate values stored in the files.
compare_nc_coords <- function(fname_1, var_1, fname_2, var_2, tol = 1e-6){
  
  fnames <- c(fname_1, fname_2)
  vars <- c(var_1, var_2)
  coords <- vector("list", 2)
  
  # Importing coordinates of the two variables
  for(i in 1:2){
    nc <- nc_open(fnames[i])
    v <- nc$var[[vars[i]]]
    if(is.null(v)){
      on.exit(nc_close(nc))
      stop("Variable '", vars[i], "' not found in ", fnames[i], ". You can choose between: ", 
           paste(names(nc$var), collapse = ", ")
      )
    }
    stopifnot(length(v$dim) >= 2)
    
    coords[[i]] <- list(x = v$dim[[1]]$vals, y = v$dim[[2]]$vals)
    nc_close(nc); rm(nc, v); invisible(gc())
  }
  
  
  # Checking if the number of grid points matches along both dimensions
  if(length(coords[[1]]$x) != length(coords[[2]]$x)) return(FALSE)
  if(length(coords[[1]]$y) != length(coords[[2]]$y)) return(FALSE)
  
  # Checking if coordinates match one by one.
  if(any(abs(coords[[1]]$x - coords[[2]]$x) > tol)) return(FALSE)
  if(any(abs(coords[[1]]$y - coords[[2]]$y) > tol)) return(FALSE)
  
  return(TRUE)
}


# Function to compute solid precipitations. The approach is the one described above, 
# with a single temperature threshold, which coupled with minimum and maximum temperatures 
# from the day decide which fraction of precipitations was solid
compute_solid_precipitations <- function(prec, tmnd, tmxd, t_th){
  
  precs <- prec 
  
  # Masking those pixels where at least the maximum temperature is above the threshold
  mask <- tmnd <= t_th & tmxd > t_th & !is.na(prec) & !is.na(tmnd) & !is.na(tmxd) & prec > 0
  precs[mask] <- prec[mask]*(t_th - tmnd[mask])/(tmxd[mask] - tmnd[mask])
  rm(mask); invisible(gc())
  
  mask <- tmnd > t_th & !is.na(prec) & !is.na(tmnd) & !is.na(tmxd) & prec > 0
  precs[mask] <- 0
  rm(mask); invisible(gc())
  
  
  # Deleting all the precipitation grid points which don't have corresponding temperatures
  mask <- !is.na(precs) & (is.na(tmnd) | is.na(tmxd))
  precs[mask] <- NA
  rm(mask); invisible(gc())
  
  
  # Returning solid precipitations
  return(precs)
}


# Function to compute DDF (which then will be corrected in order to obtain snow-melt). 
# Leap years and non-leap years are treated equally, but for a non-leap year the 
# DDF value of the 29^th of February will be deleted.
compute_ddf <- function(year, ddf_ave, ddf_ampl){
  len <- 366
  offset <- 81
  idx <- 1:len
  
  ddf <- ddf_ave + ddf_ampl * sin(2 * pi * (idx - offset)/len)
  
  if(!(year %% 4 == 0 && (year %% 100 != 0 || year %% 400 == 0))) ddf <- ddf[-60]
  return(ddf)
}


# Function to compute snow-melt based on Magnusson approach. The fusion, governed 
# by a periodically varying DDF, will be corrected by an exponential factor in order 
# to avoid non-differentiability at zero degrees.
compute_melt <- function(year, tmean, ddf_ave, ddf_ampl, expfact){
  
  # Computing DDF and degree day
  ddf <- compute_ddf(year = year, ddf_ave = ddf_ave, ddf_ampl = ddf_ampl)
  deg_day <- tmean + expfact * log(1 + exp(-tmean/expfact))
  
  # Checking if number of days in a given year and the thickness of tmean match
  # to finally compute melt
  stopifnot(dim(tmean)[3] == length(ddf))
  melt <- sweep(deg_day, 3, ddf, "*")
  
  # Looking for eventual overflows
  stopifnot(all(is.finite(melt) | is.na(melt)))

  return(melt)
}


# Function to create a container for the daily SWE maps. We need to do this step 
# as SWE keeps accumulating year after year in some locations.
create_swe_container <- function(fname){
  nc <- nc_open(fname)
  prec <- ncvar_get(nc, "total_precipitation", start = c(1, 1, 1), count = c(-1, -1, 1))
  nc_close(nc)
  
  appo <- array(0, dim = dim(prec))
  rm(prec); gc()
  
  return(appo)
}


# Save annual SWE maps into netCDF files, in order to later be able to make some trend 
# analysis and assess the amount of water stored within the snowpack. 
save_annual_swe <- function(total, fname_template, fname_out){
  
  nc <- nc_open(fname_template)
  v <- nc$var[["total_precipitation"]]
  if(is.null(v)){
    on.exit(nc_close(nc))
    stop("Variable 'total_precipitation' not found. You can choose between: ", 
         paste(names(nc$var), collapse = ", ")
         )
  }
  stopifnot(length(v$dim) == 3)
  
  dims <- vector("list", 3)
  for(i in seq_along(v$dim)){
    d <- v$dim[[i]]
    dims[[i]] <- ncdim_def(name = d$name, units = d$units, vals = d$vals,
                           unlim = d$unlim,
                           create_dimvar = isTRUE(d$create_dimvar),
                           calendar = if(is.null(d$calendar)) NA else d$calendar)
  }
  nc_close(nc); rm(nc, v); invisible(gc())
  
  
  # Checking if netCDF container and SWE container dimensions match
  lens <- vapply(dims, function(d) as.integer(d$len), integer(1))
  stopifnot(identical(lens, as.integer(dim(total))))
  
  swe <- ncvar_def(name = "swe", units = "mm", dim = dims, missval = -9999,
                   longname = "Snow water equivalent", prec = "float",
                   compression = 5)
  
  nc_out <- nc_create(fname_out, swe, force_v4 = TRUE)
  ncvar_put(nc_out, "swe", total, start = c(1, 1, 1), count = dim(total))
  nc_close(nc_out)
  
  rm(nc_out, swe, dims); invisible(gc())
}









# Importing model parameters
df_in <- read.table(fname_in, header = TRUE)
ddf_ave <- as.numeric(df_in$ddf_ave)
ddf_ampl <- as.numeric(df_in$ddf_ampl)
expfact <- as.numeric(df_in$expfact)
t_th <- as.numeric(df_in$tlim)

if((ddf_ave - ddf_ampl) < 0){ stop("DDF is negative on some days: stopping! ") }


# Importing water mask
nc <- nc_open(fname_water_mask)
water_mask <- ncvar_get(nc, "water")
nc_close(nc)


# Cycle over years
appo <- create_swe_container("Dataset/PCPD/1951.nc")
for(y in years){
  
  # File-names for precipitation and temperature dataset, to later use
  fname_prec <- paste0("Dataset/PCPD/", y, ".nc")
  fname_temp <- paste0("Dataset/TEMP/", y, ".nc")
  
  # Checking that netCDF maps are aligned
  if(!compare_nc_coords(fname_prec, "total_precipitation", fname_temp, "tmnd")){
    stop(paste0("Temperature and precipitation arrays are not aligned during ", y))
  }
  if(!compare_nc_coords(fname_prec, "total_precipitation", fname_water_mask, "water")){
    stop(paste0("Water mask and precipitation arrays are not aligned during ", y))
  }
  
  
  
  # Actually loading precipitation and temperature grids for a given year. Every layer corresponds to a day
  nc <- nc_open(fname_prec)
  prec <- ncvar_get(nc, "total_precipitation")
  prec[!is.na(prec) & prec < 0] <- 0
  nc_close(nc)
  
  nc <- nc_open(fname_temp)
  tmnd <- ncvar_get(nc, "tmnd")
  tmxd <- ncvar_get(nc, "tmxd")
  stopifnot(identical(dim(tmnd), dim(tmxd)))
  tmean <- (tmnd + tmxd)/2
  nc_close(nc)
  
  rm(nc); invisible(gc());
  stopifnot(identical(dim(tmean), dim(prec)))
  
  
  
  
  
  # Computing solid precipitations and then calling garbage cleaner in order to 
  # free up as many RAM as possible
  precs <- compute_solid_precipitations(prec = prec, tmnd = tmnd, tmxd = tmxd, t_th = t_th)
  rm(prec, tmnd, tmxd)
  invisible(gc())
  
  
  # Computing annual melt and then calling garbage cleaner in order to free up as 
  # many RAM as possible
  melt <- compute_melt(year = y, tmean = tmean, ddf_ave = ddf_ave, ddf_ampl = ddf_ampl, expfact = expfact)
  rm(tmean); invisible(gc())
  
  total <- array(NA_real_, dim = dim(melt))
  for(i in 1:dim(total)[3]){
    
    # Adding solid precipitations and melt to the SWE map and later looking for 
    # negative values, as they must be set to zero
    appo <- appo + precs[, , i] - melt[, , i]
    
    mask <- appo < 0 & !is.na(appo)
    appo[mask] <- 0
    
    total[, , i] <- round(appo, 1)
  }
  rm(mask); invisible(gc())
  
  
  # Masking swe maps with water mask
  stopifnot(identical(dim(water_mask), dim(total)[1:2]))
  mask <- as.vector(!is.na(water_mask) & water_mask == 1) & !is.na(total)
  total[mask] <- NA
  rm(mask); invisible(gc())
  

  # Saving SWE data to file
  total[is.na(total)] <- -9999
  fname_template <- paste0("Dataset/PCPD/", y, ".nc")
  fname_out <- paste0("Results/SWE_", y, ".nc")
  save_annual_swe(total = total, fname_template = fname_template, fname_out = fname_out)
  
  rm(precs, melt, total); invisible(gc())
  cat("Computed SWE maps for: ", y, "       Number of pixel NAs: ", sum(is.na(appo)),"\n")
}
