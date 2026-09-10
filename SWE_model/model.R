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

library(terra)
library(ncdf4)

years <- 1951:2023
fname_in <- "input.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/")



# Function to compute solid precipitations. The approach is the one described above, 
# with a single temperature threshold, which coupled with minimum and maximum temperatures 
# from the day decide which fraction of precipitations was solid
compute_solid_precipitations <- function(prec, tmnd, tmxd, t_th){
  
  precs <- prec 
  
  # Masking those pixels where at least the maximum temperature is above the threshold
  mask <- tmnd <= t_th & tmxd > t_th & !is.na(prec) & prec > 0
  precs[mask] <- prec[mask]*(t_th - tmnd[mask])/(tmxd[mask] - tmnd[mask])
  
  mask <- tmnd > t_th & !is.na(prec) & prec > 0
  precs[mask] <- 0
  
  # Deleting mask in order to free up ram space and then returning solid precipitations
  rm(mask); invisible(gc)
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
  
  if(year %% 4 != 0) ddf <- ddf[-60]
  return(ddf)
}


# Function to compute snow-melt based on Magnusson approach. The fusion, governed 
# by a periodically varying DDF, will be corrected by an exponential factor in order 
# to avoid non-differentiability at zero degrees.
compute_melt <- function(year, tmean, ddf_ave, ddf_ampl, expfact){
  
  ddf <- compute_ddf(year = year, ddf_ave = ddf_ave, ddf_ampl = ddf_ampl)
  deg_day <- tmean + expfact * log(1 + exp(-tmean/expfact))
  melt <- sweep(deg_day, 2, ddf, "*")   # DA CAMBIARE PER VERI RASTER

  return(melt)
}


# Function to create a container for the daily SWE maps. We need to do this step 
# as SWE keeps accumulating year after year in some locations.
create_swe_container <- function(fname){
  nc <- nc_open(fname)
  prec <- ncvar_get(nc, "prec", start = c(1, 1), count = c(-1, 1)) # DA CAMBIARE PER I VERI RASTER
  nc_close(nc)
  
  appo <- array(0, dim = dim(prec))
  rm(prec); gc()
  
  return(appo)
}


# Save annual SWE maps into netCDF files, in order to later be able to make some trend 
# analysis and assess the amount of water stored within the snowpack. 
save_annual_swe <- function(total, fname_template, fname_out){
  
  # Opening netCDF file to rob template to
  nc <- nc_open(fname_template)
  

  # Copying the dimensions of the precipitation variable, one by one
  dims <- list()
  for(i in seq_along(nc$var$prec$dim)){
    d <- nc$var$prec$dim[[i]]
    dims[[i]] <- ncdim_def(name = d$name, units = d$units, vals = d$vals, unlim = d$unlim)
  }
  nc_close(nc)
  rm(nc); invisible(gc)
  
  
  # Defining the new variable and writing it on disk
  swe <- ncvar_def(name = "swe", units = "mm", dim = dims, missval = -9999,
                   longname = "Snow water equivalent", prec = "float")
  
  nc_out <- nc_create(fname_out, swe)
  ncvar_put(nc_out, swe, total)
  nc_close(nc_out)
  
  rm(nc_out, swe, dims); invisible(gc)

}





df_in <- read.table(fname_in, header = TRUE)
ddf_ave <- as.numeric(df_in$ddf_ave)
ddf_ampl <- as.numeric(df_in$ddf_ampl)
expfact <- as.numeric(df_in$expfact)
t_th <- as.numeric(df_in$tlim)

appo <- create_swe_container("Dataset/PCPD/PCPD_1951.nc")
for(y in years){
  
  # File-names for precipitation and temperature dataset, to later use
  fname_prec <- paste0("Dataset/PCPD/PCPD_", y, ".nc")
  fname_temp <- paste0("Dataset/TEMP/T_", y, ".nc")
  
  # Importing temperature and precipitation grids in order to assess whether grid 
  # geometry is the same or not. In case of fail, we stop the script.
  prec <- rast(fname_prec)
  temp <- rast(fname_temp)
  
  if(!compareGeom(prec, temp)) stop(paste0("No compatible grids for temperature and precipitation during ", y))
  rm(prec, temp)
  invisible(gc)
  
  
  
  # Actually loading precipitation and temperature grids for a given year. Every layer corresponds to a day
  nc <- nc_open(fname_prec)
  prec <- ncvar_get(nc, "prec")
  nc_close(nc)
  
  nc <- nc_open(fname_temp)
  tmnd <- ncvar_get(nc, "tmnd")
  tmxd <- ncvar_get(nc, "tmxd")
  tmean <- ncvar_get(nc, "tmean")
  nc_close(nc)
  
  rm(nc); invisible(gc);
  
  
  
  # Computing solid precipitations and then calling garbage cleaner in order to 
  # free up as many RAM as possible
  precs <- compute_solid_precipitations(prec = prec, tmnd = tmnd, tmxd = tmxd, t_th = t_th)
  rm(prec, tmnd, tmxd)
  invisible(gc())
  
  
  # Computing annual melt and then calling garbage cleaner in order to free up as 
  # many RAM as possible
  melt <- compute_melt(year = y, tmean = tmean, ddf_ave = ddf_ave, ddf_ampl = ddf_ampl, expfact = expfact)
  rm(tmean); invisible(gc)
  
  total <- array(NA_real_, dim = dim(melt))
  for(i in 1:dim(total)[2]){ # DA CAMBIARE PER I VERI RASTER
    
    # Adding solid precipitations and melt to the SWE map and later looking for 
    # negative values, as they must be set to zero
    appo <- appo + precs[, i] - melt[, i]  # DA CAMBIARE PER I VERI RASTER
    
    mask <- appo < 0 & !is.na(appo)
    appo[mask] <- 0
    
    rm(mask); invisible(gc)
    total[, i] <- round(appo, 1)  # DA CAMBIARE PER I VERI RASTER
  }
  
  # Saving SWE data to file
  fname_template <- paste0("Dataset/PCPD/PCPD_", y, ".nc")
  fname_out <- paste0("QC_datas/Model/Dataset/Raster/SWE_", y, ".nc")
  save_annual_swe(total = total, fname_template = fname_template, fname_out = fname_out)
  
  rm(precs, melt, total); invisible(gc())
  cat("Done it for ", y, "\n")
}