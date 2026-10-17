# The main goal of this script is to reproduce a trend and slope plot that can
# be seen in MODIS paper. 
rm(list = ls())
gc()
  
library(terra)
library(geodata)
library(ggplot2)
library(tidyterra)
library(patchwork)

fname <- "Results/SCD/scd_trend_1952_2023.nc"
fname_mask <- "Results/years_continuous_snow.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")
  
  
# Function to create a clean environment for plot creation
theme_paper_clean <- function() {
  theme_void() + 
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal", 
      legend.title = element_text(face = "bold", size = 25, vjust = 1),
      legend.text = element_text(size = 20),
      legend.spacing.x = unit(0.3, 'cm'), 
      legend.spacing.y = unit(0.2, 'cm'),
      plot.subtitle = element_text(face = "bold", hjust = 0.5, size = 11, margin = margin(b = 5)),
      plot.margin = margin(5, 5, 5, 5)
    )
}

# Function to actually make the plot
make_snow_plot <- function(raster_lyr, title, breaks, labels, palette, legend_name) {
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






# Importing trend analyses
maps <- rast(fname)
slope <- maps["sen_slope"]
pval <- maps["p_value_fdr"]
pval <- ifel(slope <= 0, -1+pval, 1 - pval)


# Importing glacier mask
glacier_mask <- rast(fname_mask)
glacier_mask <- !is.na(glacier_mask) & glacier_mask > 36
slope[glacier_mask] <- NA
pval[glacier_mask] <- NA

mask <- is.na(pval) & !is.na(slope)
pval[mask] <- 0

# Plotting procedure
border_countries <- c("ITA", "FRA", "CHE", "AUT", "SVN", "DEU", "HRV")
world_borders <- vect(lapply(
  border_countries,
  function(iso) gadm(country = iso, level = 0, path = tempdir())
))
italy_border <- crop(world_borders, ext(pval))
  
pval_palette <- c("#7A2E0E", "#D7301F", "#F4A582", "#EEEEEE", "#92C5DE", "#2C7FB8", "#08306B")
slope_palette <- c("#e31a1c", "#fd8d3c", "#feb24c", "#ffed6f", "#e2f4a6", "#abdda4", "#7bccc4","#2b8cbe")
  
p1 <- make_snow_plot(
  slope, "",
  breaks = c(-7.5, -5, -2.5, -1, 1e-6, 1, 2.5),
  labels = c("< -7.5", "-7.5 to -5", "-5 to -2.5", "-2.5 to -1", "-1 to 0", "0 to 1", "1 to 2.5", "> 2.5"),
  palette = slope_palette, "SCD trend [days/decade]"
)
  
p2 <- make_snow_plot(
  pval, "",
  breaks = c(-0.99, -0.95, -0.9, 0.9, 0.95, 0.99),
  labels = c("< 0 (99% confidence)", "< 0 (95% confidence)", "< 0 (90% confidence)", "Not significant", "> 0 (90% confidence)", "> 0 (95% confidence)", "> 0 (99% confidence)"),
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