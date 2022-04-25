library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

#read data from csv files

apr_data <- read.csv("C:\\Users\\selvi\\Desktop\\UBER_Dataset\\uber-raw-data-may14.csv")
may_data <- read.csv("C:\\Users\\selvi\\Desktop\\UBER_Dataset\\uber-raw-data-may14.csv")
jun_data <- read.csv("C:\\Users\\selvi\\Desktop\\UBER_Dataset\\uber-raw-data-may14.csv")
jul_data <- read.csv("C:\\Users\\selvi\\Desktop\\UBER_Dataset\\uber-raw-data-may14.csv")
aug_data <- read.csv("C:\\Users\\selvi\\Desktop\\UBER_Dataset\\uber-raw-data-may14.csv")
sep_data <- read.csv("C:\\Users\\selvi\\Desktop\\UBER_Dataset\\uber-raw-data-may14.csv")
           
data_2014 <- rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)
           
data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")
           
data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
           
data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)
           
data_2014$day <- format(day(data_2014$Date.Time))
data_2014$month <- format(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- format(year(data_2014$Date.Time))
data_2014$dayofweek <- format(wday(data_2014$Date.Time, label = TRUE))   


data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))

head(data_2014)

#Plotting the trips by the hours in a day
hour_data <- data_2014 %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 
datatable(hour_data)

#graph1
ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

month_hour_data <- data_2014 %>%
  group_by(month,hour) %>%
  dplyr::summarise(Total = n())

#graph2 
ggplot(month_hour_data, aes(hour, Total,fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Hour and Month") +

  scale_y_continuous(labels = comma)
 

#Plotting data by trips during every day of the month

day_group <- data_2014 %>%
  group_by(day) %>%
  dplyr::summarize(Total = n()) 
datatable(day_group)

#graph3
ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

day_month_group <- data_2014 %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())


#Number of Trips taking place during months in a year

month_group <- data_2014 %>%
  group_by(month) %>%
  dplyr::summarize(Total = n()) 
datatable(month_group)

ggplot(month_group , aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

month_weekday <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())

ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)


#Finding out the number of Trips by bases
ggplot(data_2014, aes(Base)) + 
  geom_bar(fill = "darkred") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")
ggplot(data_2014, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = colors)
ggplot(data_2014, aes(Base, fill = dayofweek)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and DayofWeek") +
  scale_fill_manual(values = colors)

#Creating a map visualization of rides in New York
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

ggplot(data_2014, aes(x=Lon, y=Lat))+ 
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat))+ 
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")

ggplot(data_2014, aes(x=Lon, y=Lat, color = Base)) +
  geom_point(size=1) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")

#regression
d <- read.csv("C:\\Users\\selvi\\Desktop\\UBER_Dataset\\train.csv")
print(typeof(d))
d1 <- read.csv("C:\\Users\\selvi\\Desktop\\UBER_Dataset\\testdata.csv")

model = lm(formula=fare_amount~((sin((pickup_latitude-dropoff_latitude) / 2) ^ 2) +
                                  cos(pickup_latitude) * cos(dropoff_latitude) *
                                  (sin((pickup_longitude-dropoff_longitude) / 2) ^ 2)) * passenger_count,data=d)
print(summary(model))

predict(model, newdata = d1)
