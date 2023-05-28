## Case Study: How Does a Bike-Share Navigate Speedy Success?
## Crafted for the Google Data Analytics certification Capstone Project-Case 1.


## The following Script is designed to consolidate 12 months of data into 
## a single data frame and analyze with this business task in mind:
## Deliverable: We aim to analyze the usage patterns and behaviors of annual  
## members and casual riders in order to provide valuable insights to the 
## Cyclistic executive team from May 2022 to April 2023.
## The data has been made available by Motivate International Inc. under this
## license: https://ride.divvybikes.com/data-license-agreement

## Installing Packages for Analysis

## Primary tool for Data Manipulation
install.packages("tidyverse")

## For handling Date Attributes
install.packages("lubridate")

## For sub-setting, summarizing, rearranging, and joining together data sets
install.packages("dplyr")

## For Cleaning Data
install.packages("janitor")

## For Character Strings
install.packages('stringr')

## For Visualizing Data
install.packages("ggplot2")

## For Documentation
install.packages("rmarkdown")
install.packages("languageserver")

## Loading Packages for Analysis
library(tidyverse)
library(lubridate)
library(dplyr)
library(janitor)
library(stringr)
library(ggplot2)
library(rmarkdown)

## DATA COLLECTION AND COMBINE

## Importing 12 Month Data Set from May 2022 to April 2023
MAY22<-read.csv("C:/Users/JustinVenkatsammy/OneDrive/Divvy_Data/202205-divvy-tripdata.csv")
JUN22<-read.csv("C:/Users/JustinVenkatsammy/OneDrive/Divvy_Data/202206-divvy-tripdata.csv")
JUL22<-read.csv("C:/Users/JustinVenkatsammy/OneDrive/Divvy_Data/202207-divvy-tripdata.csv")
AUG22<-read.csv("C:/Users/JustinVenkatsammy/OneDrive/Divvy_Data/202208-divvy-tripdata.csv")
SEP22<-read.csv("C:/Users/JustinVenkatsammy/OneDrive/Divvy_Data/202209-divvy-tripdata.csv")
OCT22<-read.csv("C:/Users/JustinVenkatsammy/OneDrive/Divvy_Data/202210-divvy-tripdata.csv")
NOV22<-read.csv("C:/Users/JustinVenkatsammy/OneDrive/Divvy_Data/202211-divvy-tripdata.csv")
DEC22<-read.csv("C:/Users/JustinVenkatsammy/OneDrive/Divvy_Data/202212-divvy-tripdata.csv")
JAN23<-read.csv("C:/Users/JustinVenkatsammy/OneDrive/Divvy_Data/202301-divvy-tripdata.csv")
FEB23<-read.csv("C:/Users/JustinVenkatsammy/OneDrive/Divvy_Data/202302-divvy-tripdata.csv")
MAR23<-read.csv("C:/Users/JustinVenkatsammy/OneDrive/Divvy_Data/202303-divvy-tripdata.csv")
APR23<-read.csv("C:/Users/JustinVenkatsammy/OneDrive/Divvy_Data/202304-divvy-tripdata.csv")

## Combining 12 Month Data Set into Single Dataframe
bikedata<- rbind(MAY22,JUN22,JUL22,AUG22,SEP22,OCT22,NOV22,DEC22,JAN23,FEB23,MAR23,APR23)

## DATA CLEANING

## Inspecting Single Dataframe of 12 Month Data
str(bikedata)
colnames(bikedata)
head(bikedata)
summary(bikedata)

## Adding Columns for date, month, day, and year of each bike ride
bikedata$date<-as.Date(bikedata$started_at)
bikedata$month<-format(as.Date(bikedata$date), "%m")
bikedata$day<-format(as.Date(bikedata$date), "%d")
bikedata$year<-format(as.Date(bikedata$date), "%Y")
bikedata$day_of_week<-format(as.Date(bikedata$date), "%A")

## Adding a "ride_length" calculation to bikedata (in seconds)
bikedata$ride_length <- difftime(bikedata$ended_at,bikedata$started_at)

## Reinspecting the structure of the columns
str(bikedata)

## Converting "ride_length" from Factor to Numeric to run calculations.
is.factor(bikedata$ride_length)
bikedata$ride_length <- as.numeric(as.character(bikedata$ride_length))
is.numeric(bikedata$ride_length)

## Removing duplicate rows and ride_lengths that are less than zero
## Creating new version of dataframe (v2) since data was removed
bikedata_v2<-bikedata[!(bikedata$start_station_name == "HQ QR" | bikedata$ride_length<0),]

## ANALYSIS

## Descriptive analysis on ride_length (all figures in seconds)
mean(bikedata_v2$ride_length) #straight average (total ride length / rides)
median(bikedata_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(bikedata_v2$ride_length) #longest ride
min(bikedata_v2$ride_length) #shortest ride

## Condensing the four lines above
summary(bikedata_v2$ride_length)

## Comparing members and casual users with mean, median, max, & min
aggregate(bikedata_v2$ride_length ~ bikedata_v2$member_casual, FUN = mean)
aggregate(bikedata_v2$ride_length ~ bikedata_v2$member_casual, FUN = median)
aggregate(bikedata_v2$ride_length ~ bikedata_v2$member_casual, FUN = max)
aggregate(bikedata_v2$ride_length ~ bikedata_v2$member_casual, FUN = min)

## Average ride time by each day for members vs casual users
aggregate(bikedata_v2$ride_length ~ bikedata_v2$member_casual + bikedata_v2$day_of_week, FUN = mean)

## Days of the week are out of order - updating order to start with Sunday
bikedata_v2$day_of_week <- ordered(bikedata_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

## Average ride time by each day for members vs casual users
aggregate(bikedata_v2$ride_length ~ bikedata_v2$member_casual + bikedata_v2$day_of_week, FUN = mean)

## Analyzing ridership data by type and weekday
bikedata_v2 %>%
        #Creating weekday field using wday()
        mutate(weekday = wday(started_at, label = TRUE)) %>%
        #grouping by usertype and weekday
        group_by(member_casual, weekday) %>%
        #calculating the number of rides and average duration
        summarise(number_of_rides = n()
                  # calculating the average duration
                  ,average_duration = mean(ride_length)) %>%
        # sorting
        arrange(member_casual, weekday)								

## Visualizing the number of rides by rider type
bikedata_v2 %>% 
        mutate(weekday = wday(started_at, label = TRUE)) %>% 
        group_by(member_casual, weekday) %>% 
        summarise(number_of_rides = n()
                  ,average_duration = mean(ride_length)) %>% 
        arrange(member_casual, weekday)  %>% 
        ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
        labs(title= "Total Rides by Weekday") +
        geom_col(position = "dodge")

## Creating a visualization for average duration weekly
bikedata_v2 %>% 
        mutate(weekday = wday(started_at, label = TRUE)) %>% 
        group_by(member_casual, weekday) %>% 
        summarise(number_of_rides = n()
                  ,average_duration = mean(ride_length)) %>% 
        arrange(member_casual, weekday)  %>% 
        ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
        labs(title= "Total Rides Average duration Weekly") +
        geom_col(position = "dodge")

## Creating a visualization for average duration monthly
bikedata_v2 %>% 
        group_by(member_casual, month) %>% 
        summarise(number_of_rides = n()
                  ,average_duration = mean(ride_length)) %>% 
        arrange(member_casual, month)  %>% 
        ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
        labs(title= "Average duration Monthly") +
        geom_col(position = "dodge")

## Creating a visualization for total rides monthly
bikedata_v2 %>% 
        group_by(member_casual, month) %>% 
        summarise(number_of_rides = n()) %>%
        arrange(member_casual, month)  %>% 
        ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
        labs(title= "Total Rides Monthly") +
        geom_col(position = "dodge")

## Creating a visualization for total ride lengths monthly
bikedata_v2 %>% 
        group_by(member_casual, month) %>% 
        summarise(ride_length = n()) %>%
        arrange(member_casual, month)  %>% 
        ggplot(aes(x = month, y = ride_length, fill = member_casual)) +
        labs(title= "Total Rides Lengths Monthly") +
        geom_col(position = "dodge")

## ACTION

## Findings
## (1) Ride length for Casual members is nearly double of Full time Members.
## (2) Members take more trips than Casual riders on weekdays than Casual riders
##     Monday through Friday.
## (3) Duration for Casual Members is substantially larger than for Members.
##Recommendations
## (1) Consider offering discounted membership fees for the first year to entice
##     casual riders to ponder full-time membership.
## (2) Offer a discount incentive for Casual Members based on duration length
## (3) Offering a weekday membership over the summer to convert Casual riders to
##     Members.
## Further Exploration
## (1) Determine if bikes need repairs based on a substantial decrease in
##     duration.
## (2) Determine if there are enough bikes to sustain the demand for units on
##     higher-demand days, ensuring optimal revenue potential.

