# The main goal of this script is to create a frequency plot referring to pixel altitude
rm(list = ls())
gc()

library(terra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")
demR <- rast("DEM/DEM_Italy.tif")

# Transforming dem as we should
dem <- as.matrix(demR, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)

# Setting band limits (will be updated at every iteration of the following cycle)
liminf <- 0
limsup <- 100
appo <- numeric(46)
# Considering different altitude bands
for(i in 1:46){
  appo[i] <- length(dem[dem > liminf & dem <= limsup])

  # Updating band limits
  liminf <- liminf + 100
  limsup <- limsup + 100
}

appo <- appo/sum(appo)



# Band limits and band centers
breaks <- seq(0, 4600, by = 100)
mid    <- breaks[-1] - 50

tick_x    <- seq(0, 4500, by = 500)
tick_pos  <- tick_x + 50                    

bp <- barplot(
  height = appo,
  names.arg = rep("", length(appo)),       
  xlab = "Elevation (m a.s.l)",
  ylab = "Frequency",
  main = "Italian territory",
  ylim = c(0, max(appo) * 1.1),
  border = NA
)

# barplot restituisce le posizioni delle barre; ci mettiamo i tick dove serve
axis(1,
     at = approx(x = mid, y = bp, xout = tick_pos)$y,
     labels = tick_x)

