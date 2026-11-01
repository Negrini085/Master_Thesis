# The main goal of this script is to make SCD estimates per elevation band and per day
rm(list = ls())
gc()

library(terra)

years <- 2000:2023
fname_template_model <- "../Dataset/SWE_1945.nc"
fname_dem <- "../../SWE_analysis/DEM/DEM_compatible.tif"
fname_template_modis <- "../../SC_studies/MODIS/Dataset/annual_maps/LOS/los_2023.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_comparison/with_MODIS/")
is_leap <- function(y) (y %% 4 == 0 & y %% 100 != 0) | y %% 400 == 0


# Creating overlap mask
template_model <- rast(fname_template_model)[[1]]
template_modis <- rast(fname_template_modis)

template_modis <- project(template_modis, template_model, method = "near")
compareGeom(template_modis, template_model, stopOnError = TRUE)
overlap_mask <- !is.na(template_model) & !is.na(template_modis)



# Importing dem and selecting elevation bands
dem <- rast(fname_dem)
breaks  <- c(-Inf, seq(50, 3000, 50))
n_bands <- length(breaks) - 1
band_labels <- c("<=50", paste0(breaks[2:n_bands], "-", breaks[3:(n_bands+1)]))

zones <- classify(dem, cbind(breaks[-(n_bands + 1)], breaks[-1], 1:n_bands), right = TRUE, others = NA)
names(zones) <- "band"

compareGeom(zones, template_model, stopOnError = TRUE)
n_tot <- zonal(overlap_mask, zones, fun = "sum")
n_tot_band <- setNames(rep(0, n_bands), band_labels)
n_tot_band[as.integer(n_tot$band)] <- n_tot[, 2]





# Cycle over years (to fill the matrix to make SCD comparison assessment on)
scf_band  <- array(NA_real_, dim = c(365, n_bands, length(years)), dimnames = list(day = 1:365, band = band_labels, year = years))
nval_band <- scf_band
for (k in seq_along(years)) {
  
  y <- years[k]
  start <- 1
  end   <- 365 + is_leap(y)
  if (y == 2000) { start <- 221; end <- 312 }
  if (y == 2023) end <- 273
  doy <- start:end
  
  fname <- paste0("../../SC_studies/MODIS/Dataset/daily/", y, "/day_", sprintf("%03d", doy), ".tif")
  year_maps <- rast(fname)
  if (nlyr(year_maps) != length(doy)) stop(y, ": ", nlyr(year_maps), " layer invece di ", length(doy))
  
  if (is_leap(y) && 60 %in% doy) {
    year_maps <- year_maps[[which(doy != 60)]]
    doy <- doy[doy != 60]
  }
  row_idx <- if (is_leap(y)) doy - (doy > 60) else doy
  
  year_maps <- subst(year_maps, 2, NA)
  year_maps <- project(year_maps, template_model, method = "average")
  year_maps <- mask(year_maps, overlap_mask, maskvalues = 0)
  
  zs <- zonal(year_maps, zones, fun = "sum", na.rm = TRUE)
  zn <- zonal(!is.na(year_maps), zones, fun = "sum", na.rm = TRUE)
  
  b    <- as.integer(zs$band)
  snow <- t(as.matrix(zs[, -1, drop = FALSE]))
  nval <- t(as.matrix(zn[, -1, drop = FALSE]))
  scf  <- snow / nval
  scf[nval == 0] <- NA
  
  scf_band[row_idx, b, k]  <- scf
  nval_band[row_idx, b, k] <- nval
  
  cat("Taken into account year", y, "!\n")
}

frac_obs <- sweep(nval_band, 2, n_tot_band, "/")
saveRDS(scf_band, "Results/scf_band_MODIS.rds")