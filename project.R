library(dplyr)
library(rjson)
library(tidyverse)
library(tidyjson)
library(ggplot2)
library(lubridate)


data_json <- rjson::fromJSON(file="watch-history.json")
data <- spread_all(data_json)

data <- data |> mutate(time = as_datetime(time), hour = hour(time), date = as_date(time), 
                       weekday = wday(time, label = TRUE ) )  |>
                select( title, titleUrl, time, date, hour, weekday) |> drop_na() |> arrange(time)

ggplot(data |> group_by(date) |> summarise(  n = n()  ) ) +s
  aes(x=date, y=n) +
  geom_point()


ggplot(head(data, 300)) +
  aes(x=hour) +
  facet_wrap(vars(date))+
  geom_density(aes(x=hour))


ggplot(head(data |> group_by(date) |> mutate(n= n()) |> arrange(desc(n)),  2500) ) +
  aes(x=hour, n=n) +
  facet_wrap(vars(date))+
  annotate("text", label = "chicken", size = 3, x = 3, y = 0.2) +
  geom_density(aes(x=hour))

ggplot(data |> group_by(hour) |> summarise(bin = n())) +
  geom_line(aes(x=hour, y= bin/26280))+
  geom_point(aes(x=hour, y= bin/26280)) 


#the day i watched 180 videos
ggplot(data |> ungroup() |> filter(date==as_date("2021-05-13"))) +
  aes(x=time) +
  geom_density()



x <- data |> group_by(date) |> summarise(  n = n()  ) |> arrange(date) |> 
             mutate( delta_t = date[row_number()+1] - date)

x |> group_by(delta_t) |> summarise(n = n())




ggplot(y) +
  geom_density(aes(x=delta_t))+
  xlim(0,10000)




ggplot(data |> group_by(date) |> summarise(videos_that_day = n()) |>
         mutate(videos_previous_day = videos_that_day[row_number()+1]))+
  aes(y=videos_that_day, x = videos_previous_day)+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')+
  geom_point()
  




response_json <- GET("https://www.googleapis.com/youtube/v3/videos?id=WmRSBSdCUpQ&part=contentDetails&key=AIzaSyBZpYJo1SjGE_eGTze3RclPnDQoHony9hg")

response <- rjson::fromJSON(response_json)
play_time <- fromJSON(rawToChar(response$content))$items[[1]]$contentDetails$duration



