#Installing and loading packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(janitor)
library(lubridate)
#########################################

#data collection
sept2020 <- read_csv("sept2020.csv")
oct2020 <- read_csv("oct2020.csv")
nov2020 <- read_csv("nov2020.csv")
dec2020 <- read_csv("dec2020.csv")
jan2021 <- read_csv("jan2021.csv")
feb2021 <- read_csv("feb2021.csv")
mar2021 <- read_csv("mar2021.csv")
apr2021 <- read_csv("apr2021.csv")
may2021 <- read_csv("may2021.csv")
june2021 <- read_csv("june2021.csv")
july2021 <- read_csv("july2021.csv")
aug2021 <- read_csv("aug2021.csv")
########################################
str(sept2020)
str(oct2020)
str(nov2020)
str(dec2020)
str(jan2021)
str(feb2021)
str(mar2021)
str(apr2021)
str(may2021)
str(june2021)
str(july2021)
str(aug2021)

##########
##changing column type to combine 
sept2020 <- mutate(sept2020,start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))
oct2020 <- mutate(oct2020,start_station_id = as.character(start_station_id),
                  end_station_id = as.character(end_station_id))
nov2020 <- mutate(nov2020,start_station_id = as.character(start_station_id),
                  end_station_id = as.character(end_station_id))

## combining all datasets into one dataframe
all_trips<-bind_rows(sept2020,oct2020,nov2020,dec2020,jan2021,feb2021,
                     mar2021,apr2021,may2021,june2021,july2021,aug2021)
##GLIMPSE
glimpse(all_trips)
summary(all_trips)

all_trips_cleaned <- drop_na(all_trips)

summary(all_trips_cleaned$month)

#adding columns
all_trips_cleaned$date <- as.Date(all_trips_cleaned$started_at,"%d-%m-%y")
all_trips_cleaned$day <- format(as.Date(all_trips_cleaned$date), "%d")
all_trips_cleaned$year <- format(as.Date(all_trips_cleaned$date), "%Y")
all_trips_cleaned$day_of_week <- format(as.Date(all_trips_cleaned$date), "%A")
all_trips_cleaned$month <- format(as.Date(all_trips_cleaned$date), "%m")

#adding ride_length column
all_trips_cleaned <-mutate(all_trips_cleaned,started_at = dmy_hm(started_at),ended_at = dmy_hm(ended_at))
all_trips_cleaned$ride_length <- difftime(all_trips_cleaned$ended_at,all_trips_cleaned$started_at,units = "min")

#removing bad data
## data contains entries where the bikes were taken for testing and to the warehouse
all_trips_cleaned <- all_trips_cleaned[!(all_trips_cleaned$start_station_name == "Base - 2132 W Hubbard Warehouse"
                                         |all_trips_cleaned$start_station_id == "Hubbard Bike-checking (LBS-WH-TEST)"
                                         |all_trips_cleaned$start_station_name == "WATSON TESTING - DIVVY"
                                         |all_trips_cleaned$ride_length<=0),]



#analysis
riders_summary <- all_trips_cleaned %>% count(member_casual,month) 
riders_summary_day <- all_trips_cleaned %>% count(member_casual,day_of_week) 

bike_type<-all_trips_cleaned %>% count(member_casual,rideable_type,month)


ride_length<-aggregate(ride_length~member_casual,all_trips_cleaned,FUN=mean)
ride_length_by_month<-aggregate(ride_length~member_casual+month,all_trips_cleaned,FUN=mean)
ride_length_by_day<-aggregate(ride_length~member_casual+day_of_week,all_trips_cleaned,FUN=mean)

#exported dataframes using write.csv()
