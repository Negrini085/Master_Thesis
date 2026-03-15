  library(ncdf4)
  library(ggplot2)
  
  setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Alpine3D/Historical/")
  
  swe <- read.table("Datas/swe_evolution.dat")
  swe <- swe$V1
  
  df <- data.frame(
    day = 1:length(swe),
    swe = swe
  )
  
  
  # Plotting options (we will do a better job when it's all finished)
  save(swe, file = "swe_evolution.Rdata")
  ggplot(df, aes(x = day, y = swe)) + 
    # scale_x_continuous(
    #   breaks = dates_ind,
    #   labels = format(dates, "%d %b %Y"), 
    #   expand = c(5e-3, 5e-3)
    # ) +
    geom_line(color = "blue", size = 1.5) +
    labs(title = "Swiss SWE evolution: 2016 to 2025", x = "Days", y = "SWE Gm^3") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
