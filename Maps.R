library(webTRISr)
library(ggplot2)
library(janitor)
library(lubridate)
library(stringr)
library(glue)
library(cowplot)
library(dplyr)
library(tidygeocoder)
library(stats)
library(ggmap)
library(plotly)
update.packages("ggplot2")

#Preliminary
bbox <- c(left = -6.3, right = 2.3, bottom = 49.425, top = 56)
england <- get_stamenmap(bbox, zoom = 6, maptype = "terrain-background")


#Map of all
ggmap(england)+
	geom_point(data = all_site_dictionary, aes(x = longitude, y = latitude), color = "black", size = 1, alpha= 0.2)

ggmap(england)+
	geom_point(data = all_site_dictionary, aes(x = longitude, y = latitude, color = activity))
						 
#Map coloured by activity
ggmap(england)+
	geom_point(data = all_site_dictionary%>%filter(activity=="Inactive"), 
						 aes(x = longitude, y = latitude), color = "red", size = 1, alpha= 0.2)+
	geom_point(data = all_site_dictionary%>%filter(activity=="Active"), 
						 aes(x = longitude, y = latitude),color = "dark green", size = 1, alpha= 0.2)+
	labs(title = "Location of All Monitoring Sites in England", color = "Site Activity")+
	xlab("Longitude")+
	ylab("Latitude")+
	theme(plot.title = element_text(size = 16, family = "sans", hjust = 0.5))
ggsave(filename = "map1.png", map1, width = 5, height = 4, dpi = 300, units = "in")
?ggsave

#Map of active coloured by missing
ggmap(england)+
	geom_point(data = valid_site_dictionary%>%filter(is.na(missing)!= TRUE), aes(x = longitude, y = latitude, color = missing*100), size = 1)+
	labs(title = "Location of Active Sites in England", color = "Missing Data (%)")+
	xlab("Longitude")+
	ylab("Latitude")+
	scale_color_gradientn(limits = c(0, 100), colors = c("#018571","#f3d48b", "#a6611a"))+
	theme(plot.title = element_text(size = 16, family = "sans", hjust = 0.5))




ggmap(england)+
	geom_point(data = filter_valid_site_dictionary, aes(x = longitude, y = latitude, color = missing*100), size = 1)+
	labs(title = "Subset of Active Sites in England", color = "Missing Data (%)")+
	xlab("Longitude")+
	ylab("Latitude")+
	scale_color_gradientn(limits = c(0, 100), colors = c("#018571","#f3d48b", "#a6611a"))+
	theme(plot.title = element_text(size = 16, family = "sans", hjust = 0.5))
