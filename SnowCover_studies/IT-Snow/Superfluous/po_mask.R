# This script reads a SWE raster and a Po Basin mask, cleans the mask’s NoData 
# values, and converts it into a binary (1/NA) mask. It then reprojects and 
# resamples the mask so it perfectly matches the SWE raster’s CRS, resolution, 
# extent, and grid alignment. Finally, it applies the aligned mask to the SWE map 
# and saves both the regridded mask and the masked SWE raster for further analysis.
rm(list = ls())
gc()

library(terra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")



# First step consist in importing raster (the first is a snow-map from IT-Snow, 
# whilst the other one is a mask of Italian Po Basin)
swe  <- rast("Datas/SWE_map.tif")
mask_italian_po <- rast("../Po-Basin/Dataset/Italian_Po_Mask.tif")
mask_italian_po[mask_italian_po == -9999] <- NA



# Second step: we need to make sure that that our mask is binary in the form of
# 1 or NA values as a choice for every possible pixel. Here we are using ifel, 
# which is a tool that tests a logic condition cell by cell. In particular, three
# arguments are required, namely the logic condition, the value to assign if true, 
# and the value to assign when false. By doing so, if a cell is NA or 0, it will
# become NA after ifel() usage, and in every other case the value 1 will be assigned.
mask_italian_po <- ifel(is.na(mask_italian_po) | mask_italian_po == 0, NA, 1)



# Third step: projection and re-sampling. The first line projects the mask of Italian 
# Po basin in SWE CRS. In this situation project() takes the reference information
# from SWE. The second line resamples the projected mask on the same grid of SWE. This
# it's a necessary step, because raster in the same CRS can still be misaligned and/or
# can still have different resolution.
mask_proj <- project(mask_italian_po, swe, method = "near")
mask_res <- resample(mask_proj, swe, method = "near")



# Fourth step: actually making sure that the extent is the same. The following line
# actually deal with numerical errors that could occur during previous raster operations.
# The crop function clips the first raster on the area covered by the second one. The 
# following line is the opposite of the first one: after the application of both we are
# sure of raster alignment. The final line is just to make sure that the file is 
# still binary, with values unitary or not assigned
final_mask <- crop(final_mask, swe, snap = "out")
final_mask <- extend(final_mask, swe)   
final_mask[final_mask == 0] <- NA



# Fifth step: saving mask!
writeRaster(final_mask, "Italian_Po_Mask_on_SWEgrid.tif", overwrite = TRUE)