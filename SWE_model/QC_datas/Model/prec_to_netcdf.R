# ---------------------------------------------------------------------------
# Convert daily precipitation series (PCPD) into one netCDF file per year,
# following the CF timeSeries discrete-sampling-geometry conventions.
#
# Fixed version: robust output-directory / target-file checks, safe handling of
# pre-existing files, informative errors, guaranteed nc_close on failure.
# ---------------------------------------------------------------------------

rm(list = ls())
gc()

library(ncdf4)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_datas/Model/")

fillval          <- -9999
years            <- 1951:2023
output_dir       <- "Dataset/Sequential"          # no trailing slash needed
input_dir        <- "../../../SWE_calibration/Dataset/PCPD/"
fname_anagrafica <- "../../../HS_series/Original/STATION_check/ANAGRAFICA"
file_pattern     <- "^DV_"                        # set to NULL to take every file
overwrite        <- TRUE                          # overwrite existing PCPD_<yr>.nc


# --- Output directory ------------------------------------------------------
# file.access(mode = 2) is NOT a reliable writability test: it only checks the
# write bit with the real UID, ignoring the directory search (x) bit, ACLs and
# read-only mounts. A directory can pass that test and still refuse file
# creation with EACCES. The only trustworthy check is to actually create a file.

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(output_dir)) {
  stop("Cannot create output directory: ",
       normalizePath(output_dir, mustWork = FALSE))
}

output_dir <- normalizePath(output_dir, mustWork = TRUE)

probe <- file.path(output_dir, sprintf(".write_probe_%d", Sys.getpid()))
if (!isTRUE(suppressWarnings(file.create(probe)))) {
  stop("Output directory is not writable (cannot create files in it): ", output_dir,
       "\n  Check with:  ls -ld '", output_dir, "'",
       "\n  A directory needs both write AND execute permission for its owner:",
       "\n  chmod u+wx '", output_dir, "'")
}
unlink(probe)

message("Writing netCDF files to: ", output_dir)


# --- Helper: make sure the target file can actually be created --------------

prepare_target <- function(path, overwrite) {
  if (file.exists(path)) {
    if (!overwrite) {
      stop("File already exists and overwrite = FALSE: ", path)
    }
    # nc_create clobbers, but it cannot clobber a file we are not allowed to
    # write; removing it first turns a cryptic "Permission denied" into a
    # clear message.
    unlink(path)
    if (file.exists(path)) {
      stop("Cannot overwrite existing file (check ownership/permissions): ", path,
           "\n  ls -l '", path, "'")
    }
  }
  invisible(path)
}


# --- Input files and station metadata --------------------------------------

files <- list.files(path = input_dir, pattern = file_pattern, full.names = TRUE)
files <- files[!dir.exists(files)]                # skip sub-directories
if (length(files) == 0) stop("No files in: ", normalizePath(input_dir, mustWork = FALSE))

ids  <- basename(files)
anag <- read.table(fname_anagrafica, header = TRUE, stringsAsFactors = FALSE)

needed <- c("name", "lon", "lat", "ele")
if (!all(needed %in% names(anag))) {
  stop("ANAGRAFICA is missing column(s): ",
       paste(setdiff(needed, names(anag)), collapse = ", "))
}

# match() (not %in%) so that the metadata rows follow the same order as `files`
key <- sub("^DV_", "", ids)
idx <- match(key, anag$name)
if (anyNA(idx)) stop("No metadata for: ", paste(ids[is.na(idx)], collapse = ", "))
anag <- anag[idx, ]


# --- Build the station x time matrix ---------------------------------------

dates <- seq(as.Date(sprintf("%d-01-01", min(years))),
             as.Date(sprintf("%d-12-31", max(years))), by = "day")
prec  <- matrix(NA_real_, nrow = length(dates), ncol = length(files))

for (i in seq_along(files)) {
  
  d <- tryCatch(
    read.table(files[i], col.names = c("y", "m", "d", "seq", "prec"),
               colClasses = c("integer", "integer", "integer", "NULL", "numeric")),
    error = function(e)
      stop("Cannot read ", ids[i], ": ", conditionMessage(e), call. = FALSE)
  )
  if (nrow(d) == 0) {
    warning("Empty series: ", ids[i])
    next
  }
  
  dt <- as.Date(sprintf("%04d-%02d-%02d", d$y, d$m, d$d))
  if (anyNA(dt)) stop("Invalid date(s) in ", ids[i], call. = FALSE)
  if (anyDuplicated(dt)) warning("Duplicated dates in ", ids[i], " - last value wins")
  
  # drop records outside the requested period instead of erroring on NA subscripts
  j  <- match(dt, dates)
  ok <- !is.na(j)
  prec[j[ok], i] <- d$prec[ok]
  
  if (i %% 100 == 0) cat("Taken into account ", i, "/", length(files), " series\n")
}


# --- One netCDF per year ---------------------------------------------------

yy      <- as.integer(format(dates, "%Y"))
maxchar <- max(nchar(ids))
nstat   <- length(files)

write_year <- function(yr) {
  
  sel    <- which(yy == yr)
  if (length(sel) == 0) {
    warning("No dates for year ", yr, " - skipped")
    return(invisible(NULL))
  }
  origin <- as.Date(paste0(yr, "-01-01"))
  fout   <- file.path(output_dir, sprintf("PCPD_%d.nc", yr))
  
  prepare_target(fout, overwrite)
  
  dim_stat <- ncdim_def("station",     "", seq_len(nstat), create_dimvar = FALSE)
  dim_char <- ncdim_def("name_strlen", "", seq_len(maxchar), create_dimvar = FALSE)
  dim_time <- ncdim_def(
    "time", paste0("days since ", yr, "-01-01 00:00:00"),
    as.numeric(dates[sel] - origin),
    unlim = TRUE, calendar = "standard"
  )
  
  v_name <- ncvar_def("station_name", "", list(dim_char, dim_stat), prec = "char")
  v_lon  <- ncvar_def("lon", "degrees_east",  dim_stat, fillval, prec = "double")
  v_lat  <- ncvar_def("lat", "degrees_north", dim_stat, fillval, prec = "double")
  v_alt  <- ncvar_def("alt", "m",             dim_stat, fillval, prec = "double")
  v_prec <- ncvar_def(
    "prec", "mm", list(dim_stat, dim_time), fillval,
    longname = "daily precipitation amount",
    prec = "double", compression = 5
  )
  
  nc <- tryCatch(
    nc_create(fout, list(v_name, v_lon, v_lat, v_alt, v_prec), force_v4 = TRUE),
    error = function(e)
      stop("nc_create failed for ", fout, "\n  ", conditionMessage(e),
           "\n  Most common causes: the directory lacks write+execute permission,",
           "\n  the file exists and belongs to another user, the filesystem is",
           "\n  mounted read-only, or the disk/quota is full.", call. = FALSE)
  )
  # close the handle even if a later ncvar_put/ncatt_put fails, otherwise the
  # file stays locked and the next run reports "Permission denied"
  on.exit(try(nc_close(nc), silent = TRUE), add = TRUE)
  
  ncvar_put(nc, v_name, ids)
  ncvar_put(nc, v_lon,  anag$lon)
  ncvar_put(nc, v_lat,  anag$lat)
  ncvar_put(nc, v_alt,  anag$ele)
  ncvar_put(nc, v_prec, t(prec[sel, , drop = FALSE]))
  
  ncatt_put(nc, "station_name", "cf_role", "timeseries_id")
  ncatt_put(nc, "lon", "standard_name", "longitude")
  ncatt_put(nc, "lat", "standard_name", "latitude")
  ncatt_put(nc, "alt", "standard_name", "height_above_mean_sea_level")
  ncatt_put(nc, "alt", "positive", "up")
  ncatt_put(nc, "prec", "standard_name", "lwe_thickness_of_precipitation_amount")
  ncatt_put(nc, "prec", "cell_methods", "time: sum")
  ncatt_put(nc, "prec", "coordinates", "lat lon alt station_name")
  ncatt_put(nc, 0, "featureType", "timeSeries")
  ncatt_put(nc, 0, "Conventions", "CF-1.8")
  ncatt_put(nc, 0, "title", paste("Daily observed precipitation -", yr))
  
  nc_close(nc)
  on.exit()                                        # closed cleanly, nothing to do
  cat("Created PCPD_", yr, ".nc\n", sep = "")
  invisible(fout)
}

for (yr in years) write_year(yr)

message("Done: ", length(years), " year(s) processed.")