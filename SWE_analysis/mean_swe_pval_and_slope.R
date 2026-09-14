# The main goal of this script is to reproduce a trend and slope plot that can
# be seen in MODIS paper. 
rm(list = ls())
gc()

library(trend)
library(terra)
library(geodata)
library(ggplot2)
library(ggspatial)
library(tidyterra)
library(patchwork)

fname_glacier <- "Results/years_continuous_snow.nc" 
fname_mean_swe <- "Results/SWE/mean_swe_maps_hydro_1952_to_2023.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")


# Making Mann-Kendall and Sen-Slope tests in order to assess SWE trends.
# The tests are applied pixel-wise: pixels with too few valid years or with a
# constant series (e.g. masked glaciers) are simply set to NA.
trend_pixel <- function(swe_ts) {
  ok <- !is.na(swe_ts)
  if (sum(ok) < 30 || length(unique(swe_ts[ok])) == 1) {
    return(c(tau = NA_real_, pvalue = NA_real_, slope = NA_real_))
  }
  mk <- trend::mk.test(swe_ts[ok])
  ss <- trend::sens.slope(swe_ts[ok])
  c(tau    = unname(mk$estimates["tau"]),
    pvalue = mk$p.value,
    slope  = unname(ss$estimates))
}


# Function to reclassify a continuous layer into discrete classes. Compared to a
# hardcoded reclassification matrix this works with any number of classes, so
# the same helper serves both the slope and the significance panel.
discretize <- function(raster_lyr, breaks, labels) {
  rcl <- cbind(head(breaks, -1), tail(breaks, -1), seq_along(labels))
  out <- as.factor(classify(raster_lyr, rcl))
  levels(out) <- data.frame(ID = seq_along(labels), label = labels)
  out
}


# Function to create a clean environment for plot creation
theme_paper_clean <- function() {
  theme_void() + 
    theme(
      legend.position = "right",
      legend.direction = "horizontal", 
      legend.title = element_text(face = "bold", size = 25, vjust = 0.5),
      legend.text = element_text(size = 20),
      legend.spacing.x = unit(0.3, 'cm'), 
      legend.spacing.y = unit(0.2, 'cm'),
      plot.margin = margin(5, 5, 5, 5)
    )
}


# Function to actually make the plot. drop = FALSE keeps every class in the
# legend even when no pixel falls into it, so that the two panels stay readable
# and comparable with the published figure.
make_trend_plot <- function(raster_disc, palette, labels, legend_name) {
  ggplot() +
    geom_spatraster(data = raster_disc, maxcell = Inf) +
    geom_spatvector(data = region_cropped, fill = NA, color = "black", linewidth = 0.3) +
    scale_fill_manual(
      values = setNames(palette, labels),
      limits = labels,
      breaks = labels,
      drop = FALSE,
      name = legend_name,
      na.value = "transparent",
      na.translate = FALSE,
      guide = guide_legend(
        title.position = "top",
        ncol = 1,
        byrow = FALSE,
        label.position = "right",
        keywidth = unit(0.4, "cm"),
        keyheight = unit(0.4, "cm")
      )
    ) +
    theme_paper_clean()
}










# Importing mean swe maps for the whole investigated period, and glacier mask in 
# order to consider only pixels which are actually describing the seasonal snow pack.
# The masking procedure is also done in the following lines.
swe <- rast(fname_mean_swe, subds = "swe")
glacier <- rast(fname_glacier, subds = "years_continuous_snow")
swe <- mask(swe, glacier >= 36, maskvalues = TRUE)



# Making Mann-Kendall and Sen-Slope tests in order to assess SWE trends
trend_maps <- app(swe, trend_pixel, cores = 8)
names(trend_maps) <- c("tau", "pvalue", "slope")

slope  <- trend_maps$slope * 10
pvalue <- trend_maps$pvalue



# Trend sign and significance. The stricter class is assigned last, so that the
# 95% pixels overwrite the 90% ones they are nested into.
sign_code <- ifel(!is.na(slope), 3, NA)
sign_code <- ifel(slope > 0 & pvalue < 0.10, 2, sign_code)
sign_code <- ifel(slope > 0 & pvalue < 0.05, 1, sign_code)
sign_code <- ifel(slope < 0 & pvalue < 0.10, 4, sign_code)
sign_code <- ifel(slope < 0 & pvalue < 0.05, 5, sign_code)


# Plotting procedure
border_countries <- c("ITA", "FRA", "CHE", "AUT", "SVN", "DEU", "HRV")
world_borders <- vect(lapply(
  border_countries,
  function(iso) gadm(country = iso, level = 0, path = tempdir())
))
region_cropped <- crop(world_borders, ext(slope))

slope_palette <- c("#b34d33", "#d66d23", "#e69125", "#f0db4d", "#72e61c", "#2ea354", "#1d8c75", "#0d4d8a")
slope_labels  <- c("< -15", "-15 - -10", "-10 - -5", "-5 - -2.5",
                   "-2.5 - 0", "0 - 2.5", "2.5 - 5", "> 5")

sign_palette <- c("#0000ff", "#80d4ff", "#d9d9d9", "#ff0000", "#7b3018")
sign_labels  <- c("> 0 (95% confidence)", "> 0 (90% confidence)", "Not significant",
                  "< 0 (90% confidence)", "< 0 (95% confidence)")

p1 <- make_trend_plot(
  discretize(slope, c(-Inf, -15, -10, -5, -2.5, 0, 2.5, 5, Inf), slope_labels),
  slope_palette, slope_labels, "SWE trend (mm/decade)"
) +
  annotation_scale(location = "br", width_hint = 0.25, text_cex = 1.2) +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_y = unit(1.2, "cm"),
                         style = north_arrow_fancy_orienteering())

p2 <- make_trend_plot(
  discretize(sign_code, seq(0.5, 5.5, by = 1), sign_labels),
  sign_palette, sign_labels, "Trend sign and significance"
)

p_final <- p1 + p2 + plot_layout(ncol = 2)

ggsave("Images/swe_trend_and_significance.png", p_final,
       width = 22, height = 12, dpi = 300, bg = "white")