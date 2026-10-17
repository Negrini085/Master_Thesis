# The main goal of this script is to plot SWE climatologies of the first of the month values.
rm(list = ls())
gc()

library(trend)
library(terra)
library(geodata)
library(ggplot2)
library(ggspatial)
library(tidyterra)
library(patchwork)

fname <- "Results/SWE/first_may_maps.nc"
fname_mask <- "Results/years_continuous_snow.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")


# Function to make trend estimates
trend_fun <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 10) return(c(NA_real_, NA_real_))
  s <- trend::sens.slope(x)
  c(unname(s$estimates), s$p.value)
}


theme_snow_map <- function() {
  theme_minimal() +
    theme(
      panel.border = element_rect(fill = NA, colour = "grey50"),
      axis.title.x = element_text(size = 25, margin = margin(t = 15)),
      axis.title.y = element_text(size = 25, margin = margin(r = 15)),
      axis.text.y  = element_text(size = 20),
      axis.text.x  = element_blank(),
      legend.title = element_text(size = 25, margin = margin(b = 15)),
      legend.text  = element_text(size = 20),
      plot.title   = element_text(size = 20, face = "bold", hjust = 0.5,
                                  margin = margin(b = 10))
    )
}


base_snow_map <- function(r, borders = NULL, title = NULL, show_legend = TRUE) {
  p <- ggplot() +
    geom_spatraster(data = r, maxcell = Inf, show.legend = show_legend)
  
  if (!is.null(borders)) {
    borders <- crop(borders, ext(r))
    p <- p + geom_spatvector(data = borders, fill = NA, color = "black",
                             linewidth = 0.3)
  }
  
  p +
    coord_sf(expand = FALSE) +
    labs(x = "Longitude [°E]", y = "Latitude [°N]", title = title) +
    theme_snow_map()
}


# Function to make SWE maps
plot_swe_map <- function(r, borders = NULL, limits = c(0, 500),
                         legend_name = "SWE \n [mm]", title = NULL) {
  base_snow_map(r[[1]], borders, title) +
    scale_fill_viridis_c(
      name = legend_name, limits = limits, oob = scales::squish,
      na.value = "transparent", direction = -1, option = "mako"
    ) +
    theme(
      legend.key.width  = unit(1.5, "cm"),
      legend.key.height = unit(2, "cm")
    )
}


# Function to make trend maps
make_snow_plot <- function(raster_lyr, title, breaks, labels, palette,
                           legend_name, borders = italy_border) {
  n <- length(breaks) + 1
  stopifnot(length(labels) == n, length(palette) == n)
  
  rcl <- cbind(c(-Inf, breaks), c(breaks, Inf), seq_len(n))
  r   <- classify(raster_lyr, rcl, right = TRUE)
  levels(r) <- data.frame(ID = seq_len(n), label = labels)
  
  base_snow_map(r, borders, title) +
    scale_fill_manual(
      values   = setNames(palette, labels),
      limits   = labels,
      breaks   = labels,
      drop     = FALSE,
      name     = legend_name,
      na.value = "transparent",
      na.translate = FALSE,
      guide = guide_legend(
        title.position = "top", ncol = 1, byrow = FALSE, label.position = "right",
        keywidth = unit(0.4, "cm"), keyheight = unit(0.4, "cm")
      )) +
    theme(
      legend.key.width  = unit(0.8, "cm"),
      legend.key.height = unit(0.8, "cm"),
      legend.key.spacing.y = unit(0.3, "cm"),
      axis.text.x  = element_text(size = 15)
    )
}









# Importing rasters, computing climatologies and making trend assessment
swe_maps <- rast(fname)
swe_normal <- mean(swe_maps[[43:72]])

swe_trend <- app(swe_maps, fun = trend_fun, cores = 8)
names(swe_trend) <- c("slope", "pvalue")

slope <- 10 * swe_trend[["slope"]]
pval <- swe_trend[["pvalue"]]
pval <- ifel(slope <= 0, -1+pval, 1 - pval)

glacier_mask <- rast(fname_mask)
glacier_mask <- glacier_mask > 36
slope[glacier_mask] <- NA
pval[glacier_mask] <- NA

pval[is.na(pval) & !is.na(slope)] <- 0








# Making plots
border_countries <- c("ITA", "FRA", "CHE", "AUT", "SVN", "DEU", "HRV")
world_borders <- vect(lapply(border_countries, function(iso) gadm(country = iso, level = 0, path = tempdir())))
italy_border <- crop(world_borders, ext(pval))

p1 <- plot_swe_map(r = swe_normal, borders = world_borders)

pval_palette <- c("#7A2E0E", "#D7301F", "#F4A582", "#EEEEEE", "#92C5DE", "#2C7FB8", "#08306B")
slope_palette <- c("#e31a1c", "#fd8d3c", "#feb24c", "#ffed6f", "#e2f4a6", "#abdda4", "#7bccc4","#2b8cbe")

p2 <- make_snow_plot(
  slope, NULL, breaks = c(-15, -10, -5, -2.5, 1e-6, 2.5, 5),
  labels = c("< -15", "-15 to -10", "-10 to -5", "-5 to -2.5", "-2.5 to 0", "0 to 2.5", "2.5 to 5", "> 5"),
  palette = slope_palette, "SWE trend \n [mm/decade]"
)

p3 <- make_snow_plot(
  pval, NULL, breaks = c(-0.99, -0.95, -0.9, 0.9, 0.95, 0.99),
  labels = c("< 0 (99% confidence)", "< 0 (95% confidence)", "< 0 (90% confidence)", "Not significant", "> 0 (90% confidence)", "> 0 (95% confidence)", "> 0 (99% confidence)"),
  palette = pval_palette, "Trend sign \n & significance"
)

no_x <- theme(axis.title.x = element_blank(), axis.text.x = element_blank())
p <- (p1 + no_x) / (p2 + no_x) / p3 +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(size = 15, face = "bold"),
    plot.tag.position = c(0.00, 0.95)
  )
print(p)