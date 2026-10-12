# The main goal of this script is to create monthly confusion matrices for 
# investigating when bias is stronger in model outputs.
rm(list = ls())
gc()

library(ggplot2)
library(patchwork)

matr_titles <- c(
  "September", "October", "November", "December", "January", "February", 
  "March", "April", "May", "June", "July", "August"
  )
month_start <- c(1, 31, 62, 92, 123, 154, 182, 213, 243, 274, 304, 335)
month_end <- c(30, 61, 91, 122, 153, 181, 212, 242, 273, 303, 334, 365)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_calibration/")


# Importing station names
files <- list.files(path = "Results/raw", full.names = TRUE)
names_swe <- sub("Results/raw/V_SDH", "HSD", files)

fname <- "../HS_series/Correct/STATION_check/Dataset/ANAGRAFICA"
df <- read.table(fname, header = TRUE)
names_hs <- df$name

station_names <- intersect(names_swe, names_hs)



# Cycle over seasons
plot_list <- list()
for(i in seq_along(month_start)){
  truth_mat <- array(0, dim = c(4))
  for(name in station_names){
    
    # Importing mine and Michele series
    fname_MINE <- paste0("Results/hydro/", sub("HSD", "V_SDH", name))
    df_MINE <- read.table(fname_MINE, header = FALSE)
    
    fname_HS <- paste0("../HS_series/Correct/Dataset/", name)
    df_HS <- read.table(fname_HS, header = FALSE)
    years <- unique(as.numeric(df_HS$V1))
    
    
    # Cycle on station years
    for(y in unique(years)){
      
      if(y > 2023) next
      
      # Selecting SWE and HS series
      mask <- as.numeric(df_MINE$V1) == y
      appo_MINE <- as.numeric(df_MINE$V2)[mask]
      appo_MINE <- appo_MINE[month_start[i]:month_end[i]]
      
      mask <- as.numeric(df_HS$V1) == y
      appo_HS <- as.numeric(df_HS$V2)[mask]
      appo_HS <- appo_HS[month_start[i]:month_end[i]]
      
      if(length(appo_MINE) != length(appo_HS)) stop(paste0("No compatible length for SWE & HS series at ", name, " during ", y))
      
      
      # Making checks on values
      mask <- appo_MINE > 0 & appo_HS > 0
      truth_mat[1] <- truth_mat[1] + sum(mask, na.rm = TRUE)
      
      mask <- appo_MINE > 0 & appo_HS == 0
      truth_mat[2] <- truth_mat[2] + sum(mask, na.rm = TRUE)
      
      mask <- appo_MINE == 0 & appo_HS > 0
      truth_mat[3] <- truth_mat[3] + sum(mask, na.rm = TRUE)
      
      mask <- appo_MINE == 0 & appo_HS == 0
      truth_mat[4] <- truth_mat[4] + sum(mask, na.rm = TRUE)
    }
    
    
    # Message
    print(paste0("Taken care of ", name, " series."))
  }
  
  
  # Plotting procedure
  cm_df <- data.frame(
    MINE = factor(c("Yes",    "Yes",      "No", "No"), levels = c("Yes", "No")),
    HS = factor(c("Yes",    "No",   "Yes",    "No"), levels = c("Yes", "No")),
    val = truth_mat
  )
  
  total <- sum(truth_mat)
  cm_df$pct <- paste0(cm_df$val, "\n(", round(100 * cm_df$val / total, 1), "%)")
  
  p_cm <- ggplot(cm_df, aes(x = HS, y = MINE, fill = val)) +
    geom_tile(color = "white", linewidth = 1.2) +
    geom_text(aes(label = pct), size = 4.5, fontface = "bold") +
    scale_fill_gradient(low = "#d6eaf8", high = "#1a5276") +
    scale_y_discrete(limits = c("No", "Yes")) +
    labs(
      title = matr_titles[i],
      x     = "HS",
      y     = if ((i - 1) %% 3 == 0) "Model" else NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none", panel.grid = element_blank())
  
  plot_list[[i]] <- p_cm
}

p <- wrap_plots(plot_list, nrow = 4, ncol = 3) +
  plot_annotation(tag_levels = "a") & 
  theme(plot.tag = element_text(size = 15))

print(p)