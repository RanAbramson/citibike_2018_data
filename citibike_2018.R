```
library(plyr)
library(tidyverse)
library(data.table)
library(ggmap)
library(curl)



# get all the zip files
zipF <- list.files(path = "C:/Users/Ran/Desktop/citibike/CitibikeNYC_2018", pattern = "*.zip", full.names = TRUE)

# unzip all files
ldply(.data = zipF, .fun = unzip, exdir = "C:/Users/Ran/Desktop/citibike/CitibikeNYC_2018")

#Setting working directory

setwd("C:/Users/Ran/Desktop/citibike/CitibikeNYC_2018")

#a function that reads the files and binding them together

reading.data <- function(f_path) {
  
  files <- list.files(path = f_path,
                      pattern = "*.csv", full.names = TRUE)
  
  my_data  <<- as_tibble(rbindlist(lapply(files, read_csv)))
 
  
 }


#getting the data 

citibike_2018 <- reading.data(f_path = "Citibike_2018_data")

#checking for nas in the dates
# sum(is.na(citibike_2018$stoptime))
# sum(is.na(citibike_2018$startime))

#usage by month

citibike_2018$month <- lubridate::month(citibike_2018$starttime, label = TRUE, abbr = TRUE)

#usage by day of the week

citibike_2018$day_of_week <- lubridate::wday(citibike_2018$starttime, label = TRUE, abbr = TRUE) 

#usage by hour of the day

citibike_2018$hour_of_day <- lubridate::hour(citibike_2018$starttime)

#changing columns names

colnames(citibike_2018) <- c('trip_duration','start_time','stop_time', 
                                'start_station_id', 'start_station_name', 
                                'start_station_latitude', 'start_station_longitude',
                                'end_station_id', 'end_station_name', 
                                'end_station_latitude', 'end_station_longitude', 'bikeid',
                                'usertype', 'birth_year', 'gender', 'month', 'day_of_week', 
                                'hour_of_day')

#extracting usertypes function

only_usertype <- function(user_type) {
  
  citibike_2018 %>% filter(usertype == user_type)
  
                 }

subscribers_only <- only_usertype('Subscriber')
customers_only <- only_usertype('Customer')

#busiest stations function that includes start and end stations and coordinates


busiest_station <- function(tbl, clm1, clm2, clm3, clm4) {
  
                   clm1 <- enquo(clm1)
                   clm2 <- enquo(clm2)
                   clm3 <- enquo(clm3)
                   clm4 <- enquo(clm4)
                   tbl %>% select (!!clm1, !!clm2, !!clm3, !!clm4) %>%  dplyr::count(!!clm1, !!clm2, !!clm3, !!clm4, sort = TRUE)
  
                   }

busiest_subscribers_start_stations <- busiest_station(subscribers_only, 
                                                      start_station_name, 
                                                      start_station_latitude, 
                                                      start_station_longitude, usertype )

busiest_subscribers_end_stations <- busiest_station(subscribers_only, 
                                                    end_station_name, 
                                                    end_station_latitude, 
                                                    end_station_longitude, usertype)

busiest_customer_start_stations <- busiest_station(customers_only, 
                                                   start_station_name, 
                                                   start_station_latitude, 
                                                   start_station_longitude, usertype)

busiest_customer_end_stations <- busiest_station(customers_only, 
                                                 end_station_name, 
                                                 end_station_latitude, 
                                                 end_station_longitude, usertype)

#gender_function

what_gender <- function (gender_num) {
  
  citibike_2018 %>% filter(gender == gender_num) 
  
}

men_only <- what_gender(1)
women_only <- what_gender(2)
unspecified_gender <- what_gender(0)

#summarizing general avgrages in the data

data_overview_averages <- citibike_2018 %>% 
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
                                                   

#usage by hour of the day - subscribers

hourly_activity <- citibike_2018 %>% select(hour_of_day, usertype) %>% group_by(usertype)%>%
                    count(hour_of_day)
#plotting the output

ggplot(subscribers_by_hour, aes(x = hour_of_day, y = n)) + 
       geom_line(size = 1.7, color = "Turquoise") + 
       labs(x = "Hour of Day", y = "Rides", title = "Hourly Subscribers' Use ") + 
       theme_minimal() 

#usage by hour of the day - subscribers

subscribers_by_hour <- subscribers_only %>%  count(hour_of_day)

#plotting the output

ggplot(customers_by_hour, aes(x = hour_of_day, y = n)) + 
       geom_line(size = 1.7, color = "Red") + 
       labs(x = "Hour of Day", y = "Rides", title = "Hourly Customers' Use ") + 
       theme_minimal()

#usage by hour of the day - customers

customers_by_hour <- customers_only %>% count(hour_of_day)

#plotting the output

ggplot(hourly_activity, aes(x = hour_of_day, y = n, color = usertype)) +
           geom_line(size = 1.7) + 
           labs(x = "Hour of Day", y = "Rides", title = "Hourly Use Total") +
           theme_minimal()

#usage by day of the week - total

daily_activity <- citibike_2018 %>% select(day_of_week, usertype) %>% group_by(usertype)%>%
                  count(day_of_week)

#usage by day of the week - subscribers

subscribers_daily_activity <- subscribers_only %>% count(day_of_week)

#plotting the output

ggplot(subscribers_daily_activity, aes(x = day_of_week, y = n)) + 
       geom_col(color = "Turquoise", 
       fill = "Turquoise", width = 0.5) + 
       labs(x = "Day of the Week", y = "Rides", 
       title = "Subscribers' Daily Use") +
       theme_minimal()

#usage by day of the week - customers

customers_daily_activity <- customers_only %>% count(day_of_week)

#plotting

ggplot(customers_daily_activity, aes(x = day_of_week, y = n)) + 
                                 geom_col(color = "Red", 
                                          fill = "Red", width = 0.5) + ylim(0, 3000000) +
                                 labs(x = "Day of the Week", y = "Rides", 
                                      title = "Customers' Daily Use") +
                                      theme_minimal()

#usage by month - total

monthly_activity <- citibike_2018 %>% count(month)

#usage by month - subscribers

subscribers_monthly_activity <- subscribers_only %>% count(month)

#plotting the output

ggplot(subscribers_monthly_activity, aes(x = month, y = n)) + 
                                     geom_col(color = "Turquoise", 
                                     fill = "Turquoise", width = 0.5) + 
                                     labs(x = "Month", y = "Rides", 
                                     title = "Subscribers' Monthly Use") +
                                     theme_minimal()

#usage by month - customers

customers_monthly_activity <- customers_only %>% count(month)

#plotting the output

ggplot(customers_monthly_activity, aes(x = month, y = n)) + 
                                   geom_col(color = "Red", 
                                   fill = "Red", width = 0.5) + ylim(0,2000000) +
                                   labs(x = "Month", y = "Rides", 
                                   title = "Customers' Monthly Use") +
                                   theme_minimal()

#most popular routes

#total

most_popular_routes <- citibike_2018 %>% count(start_station_name, start_station_latitude, start_station_longitude,
                                               end_station_name, end_station_latitude, 
                                               end_station_longitude, usertype) %>% ungroup() %>% arrange(desc(n))
#subscribers

subscribers_popular_routes <- subscribers_only %>% count(start_station_name, start_station_latitude, start_station_longitude,
                                                         end_station_name, end_station_latitude, 
                                                         end_station_longitude, usertype) %>% ungroup() %>% arrange(desc(n))
#customers

customers_popular_routes <- customers_only %>% count(start_station_name, start_station_latitude, start_station_longitude,
                                                     end_station_name, end_station_latitude, 
                                                     end_station_longitude, usertype) %>% ungroup() %>% arrange(desc(n))

#listing the ten start stations by subscribers and customers

ten_busiest_subs_s_stations <- busiest_subscribers_start_stations[1:10,]

ten_busiest_customers_s_stations <- busiest_customer_start_stations[1:10,] 

#listing the ten busiest end stations by subscribers and customers

ten_busiest_subs_e_stations <- busiest_subscribers_end_stations[1:10,]

ten_busiest_customers_e_stations <- busiest_customer_end_stations[1:10,] 

#binding the busiest start stations to a table that keeps the user type

ten_busiest_start_stations <- rbind(ten_busiest_subs_s_stations, 
                                    ten_busiest_customers_s_stations)

#binding the busiest end stations to a table that keeps the user type

ten_busiest_end_stations <- rbind(ten_busiest_subs_e_stations, 
                                    ten_busiest_customers_e_stations)

#mapping the data

#requiring google maps API key
register_google(key = "AIzaSyA0cb5Adro-0ymsx1zlmXIDmkBsWYLKa2o")

#getting a map of Manhattan
nycmap <- get_map("Manhattan", zoom = 12, maptype = "satellite", source = "google")

#mapping the busiest start stations by user type

s_stations_map <- ggmap(nycmap, extent = "device", base_layer = ggplot
               (ten_busiest_start_stations, aes
                 (x = start_station_longitude, y = start_station_latitude, color = usertype,
                   size = n)
               ))
##mapping the busiest end stations by user type

e_stations_map <- ggmap(nycmap, extent = "device", base_layer = ggplot
               (ten_busiest_end_stations, aes
                 (x = end_station_longitude, y = end_station_latitude, color = usertype,
                   size = n)
                   ))

#adding a layer that shows the activity volume of the stations
#producing the maps

s_stations_map + geom_point(alpha = 0.7)

e_stations_map + geom_point(alpha = 0.7)

```


  


