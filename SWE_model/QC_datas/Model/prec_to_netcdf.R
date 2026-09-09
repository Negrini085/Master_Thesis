# ------------------------------------------------------------------
# Da serie giornaliere TMND / TMXD a file NetCDF annuali (station x time)
# Ogni file annuale contiene tmnd, tmxd e tmean piu' i metadati stazione.
# ------------------------------------------------------------------

rm(list = ls())
gc()

library(ncdf4)


## ---- Configurazione ----------------------------------------------------
# setwd va PRIMA di tutto: i path sotto sono relativi a questa directory
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_datas/Model/")

fillval          <- -9999
years            <- 1951:2023
output_dir       <- "Dataset/Sequential/"
input_dir_tmnd   <- "../../../SWE_calibration/Dataset/TMND"
input_dir_tmxd   <- "../../../SWE_calibration/Dataset/TMXD"
fname_anagrafica <- "../../../HS_series/Original/STATION_check/ANAGRAFICA"

# Nomi delle colonne nell'ANAGRAFICA: ADATTALI ai tuoi (controlla con names(anag))
col_name <- "name"
col_lon  <- "lon"
col_lat  <- "lat"
col_ele  <- "ele"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)


## ---- Lettura file e anagrafica -----------------------------------------
files_tmnd <- list.files(path = input_dir_tmnd, full.names = TRUE)
files_tmxd <- list.files(path = input_dir_tmxd, full.names = TRUE)

if (length(files_tmnd) == 0) stop("Nessun file in: ", input_dir_tmnd)
if (length(files_tmxd) == 0) stop("Nessun file in: ", input_dir_tmxd)

ids <- basename(files_tmnd)

# le due cartelle devono contenere le stesse stazioni, nello stesso ordine
if (!identical(ids, basename(files_tmxd)))
  stop("TMND e TMXD non contengono gli stessi file (o non nello stesso ordine)")

anag <- read.table(fname_anagrafica, header = TRUE, stringsAsFactors = FALSE)

need <- c(col_name, col_lon, col_lat, col_ele)
if (!all(need %in% names(anag)))
  stop("Colonne mancanti nell'anagrafica: ",
       paste(setdiff(need, names(anag)), collapse = ", "),
       "\nColonne disponibili: ", paste(names(anag), collapse = ", "))

key <- sub("DV_", "", ids)
idx <- match(key, anag[[col_name]])
if (anyNA(idx)) stop("Metadati assenti per: ", paste(ids[is.na(idx)], collapse = ", "))
anag <- anag[idx, ]


## ---- Costruzione delle matrici [time x station] ------------------------
dates <- seq(as.Date("1951-01-01"), as.Date("2023-12-31"), by = "day")
nfile <- length(files_tmnd)

tmnd <- matrix(NA_real_, nrow = length(dates), ncol = nfile)
tmxd <- matrix(NA_real_, nrow = length(dates), ncol = nfile)

for (i in seq_len(nfile)) {
  
  d_min <- read.table(files_tmnd[i], col.names = c("y", "m", "d", "seq_num", "tmnd"))
  mask <- d_min$y <= 2023
  d_min <- d_min[mask, ]
  
  d_max <- read.table(files_tmxd[i], col.names = c("y", "m", "d", "seq_num", "tmxd"))
  mask <- d_max$y <= 2023
  d_max <- d_max[mask, ]
  
  # indici separati: le due serie possono coprire periodi diversi
  dt_min <- as.Date(sprintf("%04d-%02d-%02d", d_min$y, d_min$m, d_min$d))
  dt_max <- as.Date(sprintf("%04d-%02d-%02d", d_max$y, d_max$m, d_max$d))
  
  tmnd[match(dt_min, dates), i] <- d_min$tmnd
  tmxd[match(dt_max, dates), i] <- d_max$tmxd
  
  if (i %% 100 == 0) cat("Lette", i, "/", nfile, "serie\n")
}

# i valori mancanti nei file di input sono gia' NA? se usano un codice tipo -999
# scommenta e adatta:
# tmnd[tmnd <= -900] <- NA
# tmxd[tmxd <= -900] <- NA

tmean <- (tmnd + tmxd) / 2


## ---- Scrittura NetCDF annuali ------------------------------------------
nstat  <- ncol(tmnd)
snames <- key
nchmax <- max(nchar(snames))
yrs    <- as.integer(format(dates, "%Y"))

# ncvar_put non converte gli NA in modo affidabile: li sostituiamo a mano
fill_na <- function(m) { m[is.na(m)] <- fillval; m }

for (yy in years) {
  
  sel <- which(yrs == yy)
  if (!length(sel)) next
  
  origin <- as.Date(sprintf("%d-01-01", yy))
  
  # ---- dimensioni ----
  dim_stat <- ncdim_def("station", "", seq_len(nstat), create_dimvar = FALSE)
  dim_time <- ncdim_def("time", paste0("days since ", origin),
                        as.numeric(dates[sel] - origin),
                        unlim = TRUE, calendar = "standard")
  dim_nch  <- ncdim_def("nchar", "", seq_len(nchmax), create_dimvar = FALSE)
  
  # ---- variabili ----
  v_lon <- ncvar_def("lon", "degrees_east",  dim_stat, fillval, "longitude", prec = "double")
  v_lat <- ncvar_def("lat", "degrees_north", dim_stat, fillval, "latitude",  prec = "double")
  v_ele <- ncvar_def("elevation", "m",       dim_stat, fillval, "station elevation", prec = "double")
  v_nam <- ncvar_def("station_name", "", list(dim_nch, dim_stat), prec = "char")
  
  dims <- list(dim_stat, dim_time)
  v_tn <- ncvar_def("tmnd",  "degC", dims, fillval, "daily minimum temperature", prec = "float")
  v_tx <- ncvar_def("tmxd",  "degC", dims, fillval, "daily maximum temperature", prec = "float")
  v_tm <- ncvar_def("tmean", "degC", dims, fillval, "daily mean temperature",    prec = "float")
  
  fout <- file.path(output_dir, sprintf("T_%d.nc", yy))
  nc <- nc_create(fout, list(v_lon, v_lat, v_ele, v_nam, v_tn, v_tx, v_tm), force_v4 = TRUE)
  
  ncvar_put(nc, v_lon, anag[[col_lon]])
  ncvar_put(nc, v_lat, anag[[col_lat]])
  ncvar_put(nc, v_ele, anag[[col_ele]])
  ncvar_put(nc, v_nam, snames)
  
  # t(): da [time, station] a [station, time], coerente con dims
  ncvar_put(nc, v_tn, fill_na(t(tmnd [sel, , drop = FALSE])))
  ncvar_put(nc, v_tx, fill_na(t(tmxd [sel, , drop = FALSE])))
  ncvar_put(nc, v_tm, fill_na(t(tmean[sel, , drop = FALSE])))
  
  ncatt_put(nc, "station_name", "cf_role", "timeseries_id")
  ncatt_put(nc, 0, "featureType", "timeSeries")
  ncatt_put(nc, 0, "Conventions", "CF-1.8")
  ncatt_put(nc, 0, "history", paste("created", Sys.time()))
  
  nc_close(nc)
  cat("Creato", fout, "\n")
}

cat("Fatto.\n")