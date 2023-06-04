# IMPORT

##Importing libraries
library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)

## Importing the CSV files
divvy_tripdata_202301 <- read.csv("/cloud/project/Google-Data-Analytics-Capstone-Case-Study-1/202301-divvy-tripdata.csv")
divvy_tripdata_202302 <- read.csv("/cloud/project/Google-Data-Analytics-Capstone-Case-Study-1/202302-divvy-tripdata.csv")
divvy_tripdata_202303 <- read.csv("/cloud/project/Google-Data-Analytics-Capstone-Case-Study-1/202303-divvy-tripdata.csv")
divvy_tripdata_202304 <- read.csv("/cloud/project/Google-Data-Analytics-Capstone-Case-Study-1/202304-divvy-tripdata.csv")

## Examining the individual datasets
str(divvy_tripdata_202301)
str(divvy_tripdata_202302)
str(divvy_tripdata_202303)
str(divvy_tripdata_202304)

## Confirming the total number of rows for the individual dataframes
rowtotal <- sum(
  nrow(divvy_tripdata_202301),
  nrow(divvy_tripdata_202302), 
  nrow(divvy_tripdata_202303), 
  nrow(divvy_tripdata_202304))

print(rowtotal)

## Combining the individual files into one 
divvy_tripdata_2023 <- rbind(divvy_tripdata_202301, divvy_tripdata_202302, divvy_tripdata_202303, divvy_tripdata_202304)

## Examining the combined datasets
str(divvy_tripdata_2023)

head(divvy_tripdata_2023)

## Confirming the total number of rows for the combined dataframe
print(nrow(divvy_tripdata_2023))

# DATA CLEANING

## The time format is currently yyyy-mm-dd hh:mm:ss to do a deeper analysis it is best to format into indiviudal columns and add a day of the week column.

divvy_tripdata_2023$date <- as.Date(divvy_tripdata_2023$started_at)
divvy_tripdata_2023$month <- format(as.Date(divvy_tripdata_2023$date), "%b")
divvy_tripdata_2023$day <- format(as.Date(divvy_tripdata_2023$date), "%d")
divvy_tripdata_2023$year <- format(as.Date(divvy_tripdata_2023$date), "%Y")
divvy_tripdata_2023$day_of_week <- format(as.Date(divvy_tripdata_2023$date), "%A")

head(divvy_tripdata_2023)

## Removing NA's

divvy_tripdata_2023 <- drop_na(divvy_tripdata_2023)

## Remove duplicates from dataframe

divvy_tripdata_2023_no_duplicates <- divvy_tripdata_2023[!duplicated(divvy_tripdata_2023$ride_id), ]
print(paste("Removed", nrow(divvy_tripdata_2023) - nrow(divvy_tripdata_2023_no_duplicates), "duplicate rows"))

# DATA MANIPULATION

## Creating a column to determine the ride length 

divvy_tripdata_2023_v2 <- mutate(divvy_tripdata_2023_no_duplicates, ride_length = difftime(ended_at, started_at, units = "mins"))

## Examining the V2 datasets

str(divvy_tripdata_2023_v2)

## filtering out trips with a ride length less than 0.

nrow(divvy_tripdata_2023_v2[divvy_tripdata_2023_v2$ride_length < 0,])
divvy_tripdata_2023_v3 <- divvy_tripdata_2023_v2[!divvy_tripdata_2023_v2$ride_length <0,]

## Examining the V3 datasets

glimpse(divvy_tripdata_2023_v3)

## determining the amount of members vs casual riders

rider_type_total <- table(divvy_tripdata_2023_v3$member_casual)
View(rider_type_total)


## Statistical analysis

trip_stats <- divvy_tripdata_2023_v3 %>% 
  group_by(member_casual) %>% 
  summarise(average_ride_length = mean(ride_length), standard_deviation = sd(ride_length), median_ride_length = median(ride_length), min_ride_length = min(ride_length), max_ride_length = max(ride_length))
head(trip_stats)

## Determine the mode for the day of the week (code learnt from tutorialspoint.com)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

weekday_mode <- getmode(divvy_tripdata_2023_v3$day_of_week)

print(weekday_mode)

## Determining the most popular day by rider type

divvy_tripdata_2023_v3$day_of_week <- ordered(divvy_tripdata_2023_v3$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

divvy_tripdata_2023_v3 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(rider_type_total = n(), average_ride_length = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)

## Determining the most popular months during 2020

popular_month <- divvy_tripdata_2023_v3 %>% 
  group_by(month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(-number_of_rides)

View(popular_month)

## Determine the most popular start station

station_mode <- getmode(divvy_tripdata_2023_v3$start_station_name)

print(station_mode)

## Determine the most popular start station for members

popular_start_stations_member <- divvy_tripdata_2023_v3 %>% 
  filter(member_casual == 'member') %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_starts = n()) %>% 
  filter(start_station_name != "") %>% 
  arrange(- number_of_starts)

head(popular_start_stations_member)

## Determine the most popular start station for casual riders

popular_start_stations_casual <- divvy_tripdata_2023_v3 %>% 
  filter(member_casual == 'casual') %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_starts = n()) %>% 
  filter(start_station_name != "") %>% 
  arrange(- number_of_starts)

head(popular_start_stations_casual)


