library(plyr)
library(tidyverse)
library(data.table)

# get all the zip files
zipF <- list.files(path = "C:/Users/Ran/Desktop/citibike/CitibikeNYC_2018", pattern = "*.zip", full.names = TRUE)

# unzip all files
ldply(.data = zipF, .fun = unzip, exdir = "C:/Users/Ran/Desktop/citibike/CitibikeNYC_2018")

#Setting working directory

setwd("C:/Users/Ran/Desktop/citibike/CitibikeNYC_2018/Citibike_2018_data")

#a function that reads the files and binding them together (the final goal is 4 csvs, 1 for each quarter)

reading.data <- function(f_path) {
  
  files <- list.files(path = f_path,
                      pattern = "*.csv", full.names = TRUE)
  
  my_data  <<- as_tibble(rbindlist(lapply(files, read_csv)))
 
  
 }


#getting the data for the first quarter (January to March)

citibike_Q1_2018 <- reading.data(f_path = "Q1_Jan_to_March")

#checking for nas in the dates

sum(is.na(citibike_Q1_2018$stoptime))

#usage by month

citibike_Q1_2018$month <- lubridate::month(citibike_Q1_2018$starttime, label = TRUE, abbr = TRUE)

#usage by day of the week

citibike_Q1_2018$day_of_week <- lubridate::wday(citibike_Q1_2018$starttime, label = TRUE, abbr = TRUE) 

#usage by hour of the day

citibike_Q1_2018$hour_of_day <- lubridate::hour(citibike_Q1_2018$starttime)

#changing columns names

colnames(citibike_Q1_2018) <- c('trip_duration','start_time','stop_time', 
                                'start_station_id', 'start_station_name', 
                                'start_station_latitude', 'start_station_longitude',
                                'end_station_id', 'end_station_name', 
                                'end_station_latitude', 'end_station_longitude', 'bikeid',
                                'usertype', 'birth_year', 'gender', 'month', 'day_of_week', 
                                'hour_of_day')

#extracting usertypes function

only_usertype <- function(user_type) {
  
  citibike_Q1_2018 %>% filter(usertype == user_type)
  
                 }

subscribers_only <- only_usertype('Subscriber')
customers_only <- only_usertype('Customer')

#busiest stations function

busiest_station <- function(tbl, clm1, clm2, clm3) {
  
                   clm1 <- enquo(clm1)
                   clm2 <- enquo(clm2)
                   clm3 <- enquo(clm3)
                   tbl %>% select (!!clm1, !!clm2, !!clm3) %>%  dplyr::count(!!clm1, sort = TRUE)
  
                   }

busiest_subscribers_start_stations <- busiest_station(subscribers_only, 
                                                      start_station_name, 
                                                      start_station_latitude, 
                                                      start_station_longitude )

busiest_subscribers_end_stations <- busiest_station(subscribers_only, 
                                                    end_station_name, 
                                                    end_station_latitude, 
                                                    end_station_longitude)

busiest_customer_start_stations <- busiest_station(customers_only, 
                                                   start_station_name, 
                                                   start_station_latitude, 
                                                   start_station_longitude)

busiest_customer_end_stations <- busiest_station(customers_only, 
                                                 end_station_name, 
                                                 end_station_latitude, 
                                                 end_station_longitude)

#gender_function

what_gender <- function (gender_num) {
  
               citibike_Q1_2018 %>% filter(gender == gender_num) 
  
               }

men_only <- what_gender(1)
women_only <- what_gender(2)
unspecified_gender <- what_gender(0)

#summarizing general avgrages in the data

data_overview_averages <- citibike_Q1_2018 %>% 
                                  summarise(number_of_men = nrow(men_only),
                                  number_of_women = nrow(women_only),
                                  avg_trip_in_min = mean(trip_duration)/60,
                                  avg_men_tripduration  = mean(men_only$trip_duration)/60,
                                  avg_women_tripduration = mean (women_only$trip_duration)/60,
                                  avg_user_age = mean(2019 - birth_year),
                                  avg_women_age = mean(2019 - women_only$birth_year),
                                  avg_men_age = mean (2019 - men_only$birth_year),
                                  avg_subscriber_age = mean (2019 - subscribers_only$birth_year),
                                  avg_customer_age = mean (2019 - customers_only$birth_year),
                                  avg_trip_subscriber = mean(subscribers_only$trip_duration)/60,
                                  avg_trip_customer = mean(customers_only$trip_duration)/60)
                                                   

#usage by hour of the day

hourly_activity <- citibike_Q1_2018 %>% count(hour_of_day)
subscribers_by_hour <- subscribers_only %>%  count(hour_of_day)
customers_by_hour <- customers_only %>% count(hour_of_day)

ggplot(hourly_activity, aes(x = hour_of_day, y = n)) + geom_line() + theme_classic()
ggplot(subscribers_by_hour, aes(x = hour_of_day, y = n)) + geom_line() + theme_classic()
ggplot(customers_by_hour, aes(x = hour_of_day, y = n)) + geom_line() + theme_classic()


#usage by day of the week

daily_activity <- citibike_Q1_2018 %>% count(day_of_week)
subscribers_daily_activity <- subscribers_only %>% count(day_of_week)
customers_daily_activity <- customers_only %>% count(day_of_week)

ggplot(daily_activity, aes(x = day_of_week, y = n)) + geom_col() + theme_classic()
ggplot(subscribers_daily_activity, aes(x = day_of_week, y = n)) + geom_col() + theme_classic()
ggplot(customers_daily_activity, aes(x = day_of_week, y = n)) + geom_col() + theme_classic()


#usage by month

monthly_activity <- citibike_Q1_2018 %>% count(month)
subscribers_monthly_activity <- subscribers_only %>% count(month)
customers_monthly_activity <- customers_only %>% count(month)

ggplot(monthly_activity, aes(x = month, y = n)) + geom_col() + theme_classic()
ggplot(subscribers_monthly_activity, aes(x = month, y = n)) + geom_col() + theme_classic()
ggplot(customers_monthly_activity, aes(x = month, y = n)) + geom_col() + theme_classic()



  


