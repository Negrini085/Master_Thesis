# The main goal of this script is to plot SWE climatologies of the first of the month values.
rm(list = ls())
gc()

library(terra)
library(geodata)
library(ggplot2)
library(tidyterra)
library(patchwork)

fname <- "Results/annual_mean_temp_trends.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_inputs/TEMP/")


theme_paper_clean <- function() {
  theme_void() +
    theme(
      legend.position  = "right",
      legend.direction = "vertical",
      legend.title = element_text(face = "bold", size = 25, vjust = 1),
      legend.text  = element_text(size = 20),
      legend.spacing.x = unit(0.3, 'cm'),
      legend.spacing.y = unit(0.2, 'cm'),
      plot.subtitle = element_text(face = "bold", hjust = 0.5, size = 11, margin = margin(b = 5)),
      plot.margin = margin(5, 5, 5, 5)
    )
}



make_continuous_plot <- function(raster_lyr, title, legend_name, lim = NULL,
                                 palette = c("#053061", "#2166AC",  "#4393C3", "#92C5DE", "#D1E5F0", "#F7F7F7", 
                                                       "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F")) {
  if (is.null(lim)) lim <- global(abs(raster_lyr), "max", na.rm = TRUE)[1, 1]
  
  ggplot() +
    geom_spatraster(data = raster_lyr, maxcell = Inf) +
    geom_spatvector(data = italy_border, fill = NA, color = "black", linewidth = 0.3) +
    scale_fill_gradientn(
      colours  = palette,
      limits   = c(-lim, lim),
      oob      = scales::squish,
      name     = legend_name,
      na.value = "transparent",
      guide = guide_colourbar(
        title.position = "top", title.hjust = 0.5,
        barwidth = unit(0.6, "cm"), barheight = unit(5, "cm"),
        frame.colour = "black", ticks.colour = "black"
      )
    ) +
    labs(subtitle = title) +
    theme_paper_clean()
}


make_class_plot <- function(raster_lyr, title, breaks, labels, palette, legend_name) {
  n <- length(breaks) + 1
  stopifnot(length(labels) == n, length(palette) == n)
  
  rcl <- cbind(c(-Inf, breaks), c(breaks, Inf), seq_len(n))
  r   <- classify(raster_lyr, rcl, right = TRUE)
  
  levels(r) <- data.frame(ID = seq_len(n), label = labels)
  
  ggplot() +
    geom_spatraster(data = r, maxcell = Inf, show.legend = TRUE) +
    geom_spatvector(data = italy_border, fill = NA, color = "black", linewidth = 0.3) +
    scale_fill_manual(
      values   = setNames(palette, labels),
      limits   = labels,
      breaks   = labels,
      drop     = FALSE,
      name     = legend_name,
      na.value = "transparent",
      na.translate = FALSE,
      guide = guide_legend(
        title.position = "top", ncol = 2, byrow = FALSE, label.position = "right",
        keywidth = unit(0.4, "cm"), keyheight = unit(0.4, "cm")
      )
    ) +
    labs(subtitle = title) +
    theme_paper_clean()
}










# Importing pvalue e slope
maps <- rast(fname)
slope <- maps["sen_slope"]
pval <- maps["p_value"]
pval <- ifel(slope <= 0, -1+pval, 1 - pval)

mask <- is.na(pval) & !is.na(slope)
pval[mask] <- 0



# Plotting procedure
e <- ext(8.7, 12.3, 45.5, 47.2)
# slope <- crop(slope, e)
# pval  <- crop(pval, e)
border_countries <- c("ITA", "FRA", "CHE", "AUT", "SVN", "DEU", "HRV")
world_borders <- vect(lapply(
  border_countries,
  function(iso) gadm(country = iso, level = 0, path = tempdir())
))
italy_border <- crop(world_borders, ext(pval))

pval_palette <- c("#08306B", "#2C7FB8", "#92C5DE", "#EEEEEE", "#F4A582", "#D7301F", "#7A2E0E")

p1 <- make_continuous_plot(
  slope, NULL, 0.5,
  legend_name = "Annual temp. \n trend [˚C/decade]"
)

p2 <- make_class_plot(
  pval, "",
  breaks = c(-0.99, -0.95, -0.9, 0.9, 0.95, 0.99),
  labels = c("< 0 (99% confidence)", "< 0 (95% confidence)", "< 0 (90% confidence)", "Not significant",
             "> 0 (90% confidence)", "> 0 (95% confidence)", "> 0 (99% confidence)"),
  palette = pval_palette, "Trend sign and significance"
)

final_plot <- p1 + p2 +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a') &
  theme(
    plot.tag = element_text(size = 15, face = "bold"),
    plot.tag.position = c(0.05, 0.95)
  )
print(final_plot)

p1