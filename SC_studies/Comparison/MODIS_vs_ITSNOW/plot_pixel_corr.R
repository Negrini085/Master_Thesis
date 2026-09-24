# The main goal of this script is to compare SCD values from MODIS and ITSNOW scd 
# maps, in order to assess whether some bias is present or not.
rm(list = ls())
gc()

library(terra)
library(ggplot2)

fname_modis <- "Dataset/mean_SCD_MODIS.tif"
fname_itsnow <- "Dataset/mean_scd_ITSNOW.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/Comparison/MODIS_vs_ITSNOW/")


# Importing both maps and comparing geometries
scd_modis <- rast(fname_modis)
scd_itsnow <- rast(fname_itsnow)
compareGeom(scd_itsnow, scd_modis, stopOnError = FALSE)

scd_modis <- project(scd_modis, scd_itsnow, method = "near")
compareGeom(scd_itsnow, scd_modis, stopOnError = FALSE)


# Masking pixels in order to avoid errors on the borders
mask <- is.na(scd_modis) & !is.na(scd_itsnow)
scd_itsnow <- mask(scd_itsnow, mask, maskvalues = TRUE, updatevalue = NA)

mask <- !is.na(scd_modis) & is.na(scd_itsnow)
scd_modis <- mask(scd_modis, mask, maskvalues = TRUE, updatevalue = NA)

mask <- scd_itsnow < 1 & scd_modis > 50
scd_modis <- mask(scd_modis, mask, maskvalues = TRUE, updatevalue = NA)
scd_itsnow <- mask(scd_itsnow, mask, maskvalues = TRUE, updatevalue = NA)



# Linear regression and analyses
r_stack <- c(scd_modis, scd_itsnow)
names(r_stack) <- c("scd_MOD", "scd_ITS")
df_plot <- as.data.frame(r_stack, na.rm = TRUE)

fit  <- lm(scd_MOD ~ scd_ITS, data = df_plot)
r2   <- summary(fit)$r.squared
bias <- mean(df_plot$scd_MOD - df_plot$scd_ITS)
rmse <- sqrt(mean((df_plot$scd_MOD - df_plot$scd_ITS)^2))

lab <- sprintf("y = %.2f x %+.2f\nR² = %.3f\nBias = %.1f days\nRMSE = %.1f days\nn = %d",
               coef(fit)[2], coef(fit)[1], r2, bias, rmse, nrow(df_plot))

# Plotting procedure
ggplot(df_plot, aes(x = scd_ITS, y = scd_MOD)) +
  geom_point(alpha = 0.2, size = 0.8, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "black",
              linetype = "dashed", linewidth = 0.8) +
  geom_abline(slope = coef(fit)[2], intercept = coef(fit)[1],
              color = "tomato", linewidth = 1) +
  annotate("label", x = 5, y = 365, label = lab,
           hjust = 0, vjust = 1, size = 3.8, fill = "white") +
  labs(
    title    = NULL,
    x        = "SCD ITSNOW [days]",
    y        = "SCD MODIS [days]"
  ) +
  theme_minimal(base_size = 13) +
  coord_equal(xlim = c(0, 370), ylim = c(0, 370))