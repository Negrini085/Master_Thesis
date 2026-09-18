  # The main goal of this script is to reproduce a trend and slope plot that can
  # be seen in MODIS paper. 
  rm(list = ls())
  gc()
  
  library(terra)
  library(trend)
  library(geodata)
  library(ggplot2)
  library(tidyterra)
  library(patchwork)
  
  years <- 2001:2025
  fname_mask <- "Dataset/annual_maps/LOS/los_2025.tif"
  setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/MODIS")
  
  
  # Function to create a clean environment for plot creation
  theme_paper_clean <- function() {
    theme_void() + 
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.title = element_text(face = "bold", size = 20, vjust = 1),
        legend.text = element_text(size = 15),
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
    
    # mappatura ID -> etichetta esplicita, solo per le classi realmente presenti
    ids <- sort(unique(freq(r)$value))
    levels(r) <- data.frame(ID = ids, label = labels[ids])
    
    ggplot() +
      geom_spatraster(data = r, maxcell = Inf) +
      geom_spatvector(data = italy_border, fill = NA, color = "black", linewidth = 0.3) +
      scale_fill_manual(
        values   = setNames(palette, labels),   # colori legati al NOME, non alla posizione
        limits   = labels,                      # legenda sempre completa e ordinata
        breaks   = labels,
        drop     = FALSE,
        name     = legend_name,
        na.value = "transparent",
        guide = guide_legend(
          title.position = "top", ncol = 2, byrow = FALSE, label.position = "right",
          keywidth = unit(0.4, "cm"), keyheight = unit(0.4, "cm")
        )
      ) +
      labs(subtitle = title) +
      theme_paper_clean()
  }
  
  
  
  
  # Importing snow cover duration maps and mask to avoid water and stuffs
  files <- paste0("Dataset/appo/SED/SED_", years, ".tif")
  appo_trend_maps <- rast(paste0("Dataset/appo/SCD/SCD_", years, ".tif"))
  scd_mask <- rast(fname_mask)
  scd_maps <- rast(files)
  
  
  # Masking pixels outside Italian domain
  compareGeom(scd_maps, scd_mask)
  scd_maps <- mask(scd_maps, is.na(scd_mask), maskvalue = TRUE)
  appo_trend_maps <- mask(appo_trend_maps, is.na(scd_mask), maskvalue = TRUE)
  sum_valid_scd <- sum(appo_trend_maps == 0)
  
  
  # Making trend evaluation using non parametric tests
  trend_maps <- rast("Dataset/appo/SED/trend_maps.tif")
  pval <- trend_maps$pval
  slope <- trend_maps$slope
  pval <- mask(pval, sum_valid_scd > 10, maskvalue = TRUE, updatevalue = 1)
  pval <- ifel(slope < 0, -1+pval, 1 - pval)

  # Plotting procedure
  italy_border <- gadm(country = "ITA", level = 0, path = tempdir())
  italy_cropped <- crop(italy_border, ext(scd_mask))
  
  pval_palette <- c("#8B4513","#FF0000","#E0E0E0","#87CEFA","#007FFF")
  slope_palette <- c("#e31a1c", "#fd8d3c", "#feb24c", "#ffed6f", "#e2f4a6", "#abdda4", "#7bccc4","#2b8cbe")
  
  p1 <- make_snow_plot(
    10*slope, "",
    breaks = c(-7.5, -5, -2.5, -1, 1e-6, 1, 2.5),
    labels = c("< -7.5", "-7.5 to -5", "-5 to -2.5", "-2.5 to -1", "-1 to 0", "0 to 1", "1 to 2.5", "> 2.5"),
    palette = slope_palette, "SED trend [days/decade]"
  )
  
  p2 <- make_snow_plot(
    pval, "",
    breaks = c(-0.95, -0.9, 0.9, 0.95),
    labels = c("< 0 (95% confidence)", "< 0 (90% confidence)", "Not significant", "> 0 (90% confidence)", "> 0 (95% confidence)"),
    palette = pval_palette, "Trend sign and significance"
  )
  
  final_plot <- p1 + p2 + 
    plot_layout(ncol = 2) +
    plot_annotation(tag_levels = 'A') &
    theme(
      plot.tag = element_text(size = 20, face = "bold"),
      plot.tag.position = c(0.05, 0.95)
    )
  print(final_plot)