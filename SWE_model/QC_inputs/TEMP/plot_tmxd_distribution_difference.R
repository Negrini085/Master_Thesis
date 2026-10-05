# The main goal of this script is to evaluate shifts in temperature distribution, 
# in order to find if global warming is causing a sudden shift in thermal regime
# which could affect melt patterns.
rm(list = ls())
gc()

library(terra)
library(ggplot2)
library(patchwork)

years_first <- 1951:1980
years_second <- 1991:2020
colors_thesis <- c(
  "1951-1980" = "#5B7FA3",
  "1991-2020" = "#D97A6C"
)
fname_dem <- "../../../SWE_analysis/DEM/DEM_compatible.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_inputs/TEMP/")



# Function for extracting temperature values during a given period and at a certain 
# elevation band.
extract_temp <- function(years, mask, dir_temp = "../../Dataset/TEMP/", layers = 90:120, variable = "tmxd"){
  
  temp_list <- vector("list", length(years))
  for(i in seq_along(years)){
    
    y <- years[i]
    cat("Processing year:", y, "\n")
    
    # Importing temperature values
    fname <- paste0(dir_temp, y, ".nc")
    appo_tmax <- rast(fname, subds = variable)[[layers]]
    
    # Applying elevation mask and estracting valid values
    appo_tmax[!mask] <- NA
    appo_values <- values(appo_tmax, mat = TRUE)
    appo_values <- appo_values[!is.na(appo_values)]
    
    # Saving values
    temp_list[[i]] <- data.frame(
      T = appo_values,
      Year = y
    )
    
    rm(appo_tmax, appo_values)
    invisible(gc())
  }
  
  temp_df <- do.call(rbind, temp_list)
  return(temp_df)
}



# Function for plotting temperature distributions
plot_temp_distributions <- function(
    data, variable = "T", group = "Period", stat = "median", adjust = 1,
    alpha = 0.35, xlab = "Tmax  [°C]", ylab = "Relative frequency",
    colors = NULL, show_stat = TRUE, ylim = c(0, 0.125)){
  
  # Checking statistic
  if(!stat %in% c("mean", "median")){
    stop("'stat' must be either 'mean' or 'median'")
  }
  
  
  # Calculating statistics for each group
  if(stat == "mean"){
    stat_values <- aggregate(
      data[[variable]], by = list(data[[group]]),
      FUN = mean, na.rm = TRUE
    )
  } else {
    stat_values <- aggregate(
      data[[variable]], by = list(data[[group]]),
      FUN = median, na.rm = TRUE
    )
  }
  
  names(stat_values) <- c(group, "stat_value")
  
  
  # Basic plot
  p <- ggplot(data, aes(x = .data[[variable]], fill = .data[[group]], colour = .data[[group]])) +
    geom_density(
      alpha = alpha,
      linewidth = 0.9,
      adjust = adjust
    ) +
    geom_vline(
      data = stat_values,
      aes(
        xintercept = stat_value,
        colour = .data[[group]]
      ),
      linetype = "dashed",
      linewidth = 0.8,
      show.legend = FALSE
    ) +
    labs(
      x = xlab,
      y = ylab,
      fill = "Period",
      colour = "Period"
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 15),
      legend.title = element_blank(),
      legend.text = element_text(size = 20),
      legend.position = "top"
    )
  
  
  # Adding custom colors
  if(!is.null(colors)){
    p <- p +
      scale_fill_manual(values = colors) +
      scale_colour_manual(values = colors)
  }
  
  # Adding statistic values to the plot
  # Adding statistic values to the plot
  if(show_stat){
    
    # Different vertical positions for labels
    stat_values <- stat_values[order(stat_values$stat_value), ]
    stat_values$label_x <- stat_values$stat_value + c(-1.5, 1.5)
    stat_values$label_y <- seq(
      from = ylim[2],
      to   = ylim[2],
      length.out = nrow(stat_values)
    )
    
    p <- p +
      geom_text(
        data = stat_values,
        aes(
          x = label_x,
          y = label_y,
          label = round(stat_value, 1),
          colour = .data[[group]]
        ),
        inherit.aes = FALSE,
        hjust = 0.5,
        vjust = 0,
        size = 5,
        fontface = "bold",
        show.legend = FALSE
      )
  }
  
  p <- p + coord_cartesian(ylim = ylim)
  return(p)
}







# Checking if raster geometry is compatible
dem <- rast(fname_dem)
temp <- rast("../../Dataset/TEMP/1945.nc")
cat("Temperature and DEM rasters compatibility: ", compareGeom(dem, temp, stopOnError = FALSE), "\n")
mask <- dem >= 2000 & dem < 2500


# Extracting maximum temperatures over selected periods (and adding period info)
tmxd_first <- extract_temp(years = years_first, mask = mask)
tmxd_second <- extract_temp(years = years_second, mask = mask)

tmxd_first$Period <- paste0(min(years_first), "-", max(years_first))
tmxd_second$Period <- paste0(min(years_second), "-", max(years_second))
tmxd_all <- rbind(tmxd_first, tmxd_second)


# Extracting maximum temperatures over selected periods (and adding period info)
tmnd_first <- extract_temp(years = years_first, mask = mask, variable = "tmnd")
tmnd_second <- extract_temp(years = years_second, mask = mask, variable = "tmnd")

tmnd_first$Period <- paste0(min(years_first), "-", max(years_first))
tmnd_second$Period <- paste0(min(years_second), "-", max(years_second))

tmnd_all <- rbind(tmnd_first, tmnd_second)


# Plotting procedure
p_tmxd <- plot_temp_distributions(
  data = tmxd_all, variable= "T", group = "Period",
  stat = "mean", adjust = 1, ylab = NULL, colors = colors_thesis
)

p_tmnd <- plot_temp_distributions(
  data = tmnd_all, variable= "T", group = "Period",
  stat = "mean", adjust = 1, xlab = "Tmin [°C]", colors = colors_thesis
)

p_combined <- (p_tmnd | p_tmxd) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(
      size = 20,
      face = "bold"
    )
  )

p_combined
