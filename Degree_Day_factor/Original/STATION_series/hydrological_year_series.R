# The main goal of this script is to convert station series from conventional
# format (year starting on the 1st of January and ending on the 31th of December) 
# to hydrological years. I will consider as an hydrological year the period stretching
# from the 1st of September to the 31st of August in order to later focus my search 
# on single years. I will also check whether some years are missing, in order to 
# lighten the dataset.
rm(list = ls())
gc()

fname <- "Dataset/station_names.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Ours/STATION_series/")


# Function to switch from years to hydrological years
normal_to_hydro <- function(station_name){
  
  # Importing snow height series for a specific station. In order to get in the 
  # best situation possible, I already delete the first 8 rows and the last 4, 
  # because they are part of an hydrological year which is not fully contained in
  # the dataset.
  fname <- paste0("../Dataset/", station_name)
  ncols <- max(count.fields(fname, sep = ""))
  df <- read.table(fname, header = FALSE, sep = "", fill = TRUE, col.names = paste0("V", seq_len(ncols)))
  df <- df[9:(nrow(df) - 4), ]
  
  if(nrow(df) != 2){
    if(nrow(df) %% 12 != 0){
      print(paste0("Row number is not a multiple of 12! Station: ", station_name))
      return(0) 
    }
    
    
    # Deleting year and month columns, in order to check data quality
    raw <- as.matrix(df[, -(1:2), drop = FALSE])
    raw <- apply(raw, 2, as.numeric)
    
    years <- numeric(0)
    for(i in seq_len(nrow(raw))){
      years <- c(years, as.integer(df[1, 1]) + 1 + (i-1)%/%12)
    }
    
    
    # Looking for void years
    rows_to_remove <- c()
    n_iter <- nrow(raw)/12
    for(i in seq_len(n_iter)){
      
      # Selecting specific hydrological year
      ind <- (i-1)*12 + 1
      appo <- raw[ind:(ind+11), , drop = FALSE]
      
      # This year is actually void or not?
      mask <- is.na(appo) | appo == -90.0
      if(sum(mask) == (nrow(appo) * ncol(appo))){
        rows_to_remove <- c(rows_to_remove, ind:(ind+11))
      }
    }
    
    
    # Row removal
    if(length(rows_to_remove) > 0){
      raw <- raw[-rows_to_remove, , drop = FALSE]
      years <- years[-rows_to_remove]
    }
    
    
    # Check on raw content. I don't want to create empty files
    if(nrow(raw) == 0){
      print(paste0("No valid years for ", station_name))
      return(0)
    }
    
    
    # Finally unlisting
    hs_series <- unlist(lapply(seq_len(nrow(raw)), function(i) {
      x <- as.numeric(raw[i, , drop = FALSE])
      x <- x[!is.na(x)]
      x[x == -90] <- NA
      x
    }), use.names = FALSE)
    
    years_expanded <- unlist(lapply(seq_len(nrow(raw)), function(i) {
      x <- raw[i, , drop = FALSE]
      x <- x[!is.na(x)]
      rep(years[i], length(x))
    }), use.names = FALSE)
    
    # Setting to zero negative values
    mask <- hs_series < 0
    hs_series[mask] <- 0
    
    # Creating data-frame to later plot
    df <- data.frame(
      yrs = years_expanded, 
      hs = hs_series
    )
    
    # Saving to file
    write.table(df, paste0("Dataset/station_series/", station_name), row.names = FALSE, col.names = FALSE, quote = FALSE)
    return(1)
    
  } else {
    print(paste0("Not enough years for ", station_name))
    return(0)
  }
}


# Importing station names
df <- read.table(fname)
station_names <- df$V1
flags <- df$V2

# Cycle on stations
station_flag <- character(0)
usable_stations <- character(0)
for(i in seq_along(station_names)){
  appo <- normal_to_hydro(station_names[i])
  if(appo == 1){
    usable_stations <- c(usable_stations, station_names[i])
    station_flag <- c(station_flag, flags[i])
  }
}

df_print <- data.frame(
  station_names = usable_stations, 
  flags = station_flag
)

write.table(df_print, "Dataset/station_series/usable_stations.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)
