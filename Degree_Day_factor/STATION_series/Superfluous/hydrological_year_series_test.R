# The main goal of this script is to test normal_to_hydro function.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/STATION_series/")

# Function to switch from years to hydrological years
normal_to_hydro <- function(station_name){
  
  # Importing snow height series for a specific station. In order to get in the 
  # best situation possible, I already delete the first 8 rows and the last 4, 
  # because they are part of an hydrological year which is not fully contained in
  # the dataset.
  fname <- paste0("Superfluous/Files/", station_name)
  ncols <- max(count.fields(fname, sep = ""))
  df <- read.table(fname, header = FALSE, sep = "", fill = TRUE, col.names = paste0("V", seq_len(ncols)))
  df <- df[9:(nrow(df) - 4), ]
  
  if(nrow(df) != 2){
    if(nrow(df) %% 12 != 0){
      print(nrow(df))
      print("Row number is not a multiple of 12!")
    }
    
    
    # Deleting year and month columns, in order to check data quality
    raw <- as.matrix(df[, -(1:2), drop = FALSE])
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
      return(NULL)
    }
    
    
    # Finally unlisting
    hs_series <- unlist(lapply(seq_len(nrow(raw)), function(i) {
      x <- raw[i, , drop = FALSE]
      x <- x[!is.na(x)]
      x[x == -90] <- NA
      x
    }), use.names = FALSE)
    
    years_expanded <- unlist(lapply(seq_len(nrow(raw)), function(i) {
      x <- raw[i, , drop = FALSE]
      x <- x[!is.na(x)]
      rep(years[i], length(x))
    }), use.names = FALSE)
    
    
    # Creating data-frame to later plot
    df <- data.frame(
      yrs = years_expanded, 
      hs = hs_series
    )
    
    # Saving to file
    write.table(df, paste0("Datas/station_series/", station_name), row.names = FALSE, col.names = FALSE, quote = FALSE)
    
  } else {
    print(paste0("Not enough years for ", station_name))
  }
}


# First test
print("First check, look into Datas/station_series folder!")
normal_to_hydro("test_1.dat")


# Second test
print("Second check, look into Datas/station_series folder!")
normal_to_hydro("test_2.dat")


# Third test
print("Third check, look into Datas/station_series folder!")
normal_to_hydro("test_3.dat")


# Fourth test
print("Fourth check, look into Datas/station_series folder!")
normal_to_hydro("test_4.dat")