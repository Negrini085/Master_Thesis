  library(ncdf4)
  library(ggplot2)
  
  setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Alpine3D/Recent/")
  
  swe <- read.table("Datas/swe_evolution_parallel.dat")
  swe <- swe$V1
  
  df <- data.frame(
    day = 1:length(swe),
    swe = swe
  )
  
  data0 <- as.Date("2015-09-01")
  dates <- as.Date(paste0(2015:2025, "-09-01"))     # Creating date strings
  dates_ind <- as.numeric(dates - data0) + 1            # Finding date indexes
  
  
  # Plotting options (we will do a better job when it's all finished)
  save(swe, file = "swe_evolution.Rdata")
  ggplot(df, aes(x = df$day, y = df$swe)) + 
    scale_x_continuous(
      breaks = dates_ind,
      labels = format(dates, "%d %b %Y"), 
      expand = c(5e-3, 5e-3)
    ) +
    geom_line(color = "blue", size = 1.5) +
    labs(title = "Swiss SWE evolution: 2016 to 2025", x = "Days", y = "SWE Gm^3") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
