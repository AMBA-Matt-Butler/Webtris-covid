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

##SKIP and call from csv
# Generate all 19570 sites
add_all_site_dictionary <- data.frame(id = 1:19570)%>%	
	mutate(activity = sapply(id, get_site_activity))%>%
	mutate(description = sapply(id, get_site_description))%>%
	mutate(longitude = sapply(id, get_site_longitude))%>%
	mutate(latitude = sapply(id, get_site_latitude))
all_site_dictionary[] <- lapply(all_site_dictionary, function(x) { attributes(x) <- NULL; x })
add_all_site_dictionary[] <- lapply(add_all_site_dictionary, function(x) { attributes(x) <- NULL; x })
all_site_dictionary <- rbind(all_site_dictionary, add_all_site_dictionary)
rownames(all_site_dictionary) <- NULL
write.csv(all_site_dictionary,"C:/Users/MBUTLER1/OneDrive - Department for Transport/Stats & R/R Plots/Webtris API\\all_site_dictionary.csv", row.names = FALSE)

#Generate valid site dictionary with  additional detail for active sites
valid_site_dictionary <- all_site_dictionary %>%
	filter(activity == "Active")
valid_site_dictionary <- valid_site_dictionary %>%	
	mutate(missing = sapply(id, get_three_year_missing))%>%
	mutate(quality = sapply(id, get_three_year_quality))


write.csv(valid_site_dictionary,"C:/Users/MBUTLER1/OneDrive - Department for Transport/Stats & R/R Plots/Webtris API\\valid_site_dictionary.csv", row.names = FALSE)

#Filter for good quality datea

#Add geocode (quite slow code, due to reverse_geocode)
geo_valid_site_dictionary <- valid_site_dictionary %>%
	reverse_geocode(lat = latitude, long = longitude)
geo_valid_site_dictionary <- geo_valid_site_dictionary%>%
	mutate(postcode = sapply(address, extract_postcode))
geo_valid_site_dictionary[] <- lapply(geo_valid_site_dictionary, function(x) { attributes(x) <- NULL; x })
write.csv(geo_valid_site_dictionary,"C:/Users/MBUTLER1/OneDrive - Department for Transport/Stats & R/R Plots/Webtris API\\geo_valid_site_dictionary.csv", row.names = FALSE)
