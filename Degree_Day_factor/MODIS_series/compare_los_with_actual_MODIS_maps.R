# The main goal of this script is to compare modis series los with modis actual 
# values. To be fair, this isn't a perfectly fine comparison because MODIS uses 
# 1st October --> 30th September as an hydrological year, whilst I worked with 
# 1st September --> 31th August. Considering that, I don't expect a perfect match, 
# but not even a big dispersion from the y = x diagonal if working with a scatter.
rm(list = ls())
gc()