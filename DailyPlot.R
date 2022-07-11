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

setwd("C:/Users/MBUTLER1/OneDrive - Department for Transport/Stats & R/R Plots/Webtris API")

#These functions are used in building the site dictionaries
get_site_description <- function(site_number){
	description <- tryCatch(webtris_sites(site_number)["Name"], error=function(err) NA)
	road <- sub(".*MIDAS site at ", "", description)
	road <- sub(" priority.*", "", road)
	return(paste(road, word(description,-1)))
}
get_site_activity <- function(site_number){
	return(tryCatch(unlist(webtris_sites(site_number)["Status"]), error=function(err) NA))
}
get_site_longitude <- function(site_number){
	return(tryCatch(unlist(webtris_sites(site_number)["Longitude"]), error=function(err) NA))
}
get_site_latitude <- function(site_number){
	return(tryCatch(unlist(webtris_sites(site_number)["Latitude"]), error=function(err) NA))
}
get_three_year_quality <- function(site_number){
	quality <- webtris_quality(site_number, 01012019, 31012022)
	count <- quality %>%
		count(Quality!=100)
	return(count[2,2]/sum(count[,2]))
}#Low is good
get_three_year_missing <- function(site_number){
	quality <- webtris_quality(site_number, 01012019, 31012022)
	count <- quality %>%
		count(Quality==0)
	percent_missing <- count[2,2]/sum(count[,2])
	return(percent_missing)
}

##Graphing Functions(End_date must be last day of month)
get_day_traffic_data <- function(site_number, date){
	data <- webtris_report(sites=site_number,
												 start_date = date, end_date = date,
												 report_type = 'daily')
	data <- clean_names(data)
	return(data)
}
plot_day_traffic_data <- function(site_number, date, max=NA){
	data <- webtris_report(sites=site_number,
												 start_date = date, end_date = date,
												 report_type = 'daily')
	data <- clean_names(data)
	sum <- sum(data$total_volume)
	ggplot(data, aes(x = time_period_ending, y= total_volume))+
		geom_area()+
		scale_y_continuous(expand = c(0,0), limits = c(0, max))+
		ylab("Total Number of Vehicles")+
		xlab("Time of Day")+
		theme_dft+
		labs(title = paste(get_site_description(site_number)),
				 subtitle = paste("Total vehicles: ", sum),
				 caption = paste(wday(dmy(date), label = TRUE, abbr = FALSE),": ", date_hyphenate(date)))
}

date_hyphenate <- function(date){
	date_d <- character(10)
	date_string_split <- unlist(str_split(as.character(date), ""))
	
	if(length(date_string_split)<8) {
		date_d[1] <- 0
		date_d[2] <- date_string_split[c(1)]
		date_d[c(3,6)] <- as.character("-")
		date_d[c(4,5)]<- date_string_split[c(2:3)]
		date_d[c(7:10)]<- date_string_split[c(4:7)]
	} else {
		date_d[c(1:2)] <- date_string_split[c(1:2)]
		date_d[c(3,6)] <- as.character("-")
		date_d[c(4,5)]<- date_string_split[c(3:4)]
		date_d[c(7:10)]<- date_string_split[c(5:8)]
	}
	date_final<-paste(date_d,collapse="")
	return(date_final)
}
ma <- function(traffic_flow, n = 7){
	stats::filter(traffic_flow, rep(1 / n, n), sides = 2)
}
plot_longer_traffic_data <- function(site_number, date, end_date, max=NA){
	ddate <- as.Date(as.character(date_hyphenate(date)), format = "%d-%m-%Y")
	end_ddate <- as.Date(as.character(date_hyphenate(end_date)), format = "%d-%m-%Y")
	date_labels <- seq.Date(from = ddate, to = end_ddate, by = "day")
	data <- webtris_report(site_number, start_date = date, end_date = end_date, report_type = 'monthly-daily')
	data$date <- as.Date(str_c(data$DayNumber, ' ', data$Month), "%d %B %Y")
	data <- clean_names(data)
	data <- data %>%
		mutate(seven_day_average = ma(data$flow_value))
	a <-ggplot(data)+
		geom_rect(xmin=as.Date("2020-03-16"), xmax=as.Date("2020-06-01"), ymin = 0, ymax= Inf, alpha = 0.5, fill = "#DDDDDD")+
		geom_rect(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax= Inf, alpha = 0.5, fill = "#DDDDDD")+
		geom_rect(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-03"), ymin = 0, ymax= Inf, alpha = 0.5, fill = "#DDDDDD")+
		geom_line(aes(x = date, y= seven_day_average))+
		scale_y_continuous(expand = c(0,0), limits = c(0, NA))+
		scale_x_date(expand = c(0,0))+
		ylab("Number of Vehicles (Seven Day Average)")+
		xlab("Date")+
		theme_dft+
		labs(title = paste(get_site_description(site_number)),
				 caption = paste("From", min(data$date), "to", max(data$date)))
		
	#return("Missing data impacts x axis label")
	return(a)
}
plot_three_year_traffic_data <- function(site_number, max=NA){
	plot_longer_traffic_data(site_number, 01012019, 31052022)
}
make_week_grid <- function(site_number, date, max = NA){
	p1 <- plot_day_traffic_data(site_number, date, max)
	p2 <- plot_day_traffic_data(site_number, date+1000000, max)
	p3 <- plot_day_traffic_data(site_number, date+2000000, max)
	p4 <- plot_day_traffic_data(site_number, date+3000000, max)
	p5 <- plot_day_traffic_data(site_number, date+4000000, max)
	p6 <- plot_day_traffic_data(site_number, date+5000000, max)
	p7 <- plot_day_traffic_data(site_number, date+6000000, max)
	plot_grid(p1,p2,p3,p4,p5,p6,p7, nrow=2, ncol = 4)
}

#START HERE
all_site_dictionary <- read.csv("all_site_dictionary.csv")
valid_site_dictionary <- read.csv("Valid_Site_Dictionary.csv")
filter_valid_site_dictionary <- valid_site_dictionary %>%
	filter(missing <0.03)%>%
	filter(quality <0.03)

#Descriptive Statistics on Sites
plot_three_year_traffic_data(479)
filter_valid_site_dictionary <- filter_valid_site_dictionary %>%
	mutate(road = as.factor(sub("/.*", "", description)))

filter_valid_site_dictionary%>%
	group_by(road)%>%
	summarise(n())
filter_valid_site_dictionary%>%
	filter(road == "M25")%>%
	group_by(road)%>%
	summarise(n())
###Audit
#Lockdown 1: 26th March is 86th day, 1st of June is 153rd day
#Lockdown 2: 5th November is 310th day, 2nd of December of June is 337th day
#Lockdown 3: 6th Jan 2021 is 372nd day, 8th March 2021 is 433rd day
