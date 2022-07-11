theme_dft <- ggplot2::theme(axis.text = element_text(family = "sans", size = 10, colour = "black"), # axis text
														axis.title.x = element_text(family = "sans", size = 13, colour = "black", # x axis title
																												margin = margin(t = 10)),
														axis.title.y = element_blank(),
														plot.title = element_text(size = 16, family = "sans", hjust = 0.5),
														plot.subtitle = element_text(size = 13, colour = "black", hjust = -0.05,
																												 margin = margin(t = 10)),
														legend.key = element_blank(), # make the legend background blank
														legend.position = "bottom", # legend at the bottom
														legend.direction = "horizontal", # legend horizontal
														legend.title = element_blank(), # remove legend title
														legend.text = element_text(size = 9, family = "sans"),
														axis.ticks = element_blank(), # remove tick marks
														panel.grid = element_blank(), # remove grid lines
														panel.background = element_blank(), # remove background
														axis.line.x = element_line(size = 0.5, colour = "black")) 