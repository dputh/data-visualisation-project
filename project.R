library(dplyr)
library(rjson)
library(tidyverse)
library(tidyjson)
library(ggplot2)
library(lubridate)

#load data a
data_json <- jsonlite::fromJSON(txt="data/watch-history.json", flatten=TRUE)
data <- tibble(data_json)

data <- data |> unnest(subtitles) |> mutate(time = as_datetime(time), hour = hour(time), date = as_date(time), 
                       weekday = wday(time, label = TRUE ))   |>
                select( title, titleUrl, time, date, hour, weekday, channel=name) |> drop_na() |> arrange(time)

#videos per day over time
ggplot(data |> group_by(date) |> summarise(  n = n()  ) ) +
  aes(x=date, y=n) +
  geom_point()

#watch density of some days
ggplot(head(data, 300)) +
  aes(x=hour) +
  facet_wrap(vars(date))+
  geom_density(aes(x=hour))

#watch density of top days
ggplot(head(data |> group_by(date) |> mutate(n= n()) |> arrange(desc(n)),  2500) ) +
  aes(x=hour, n=n) +
  facet_wrap(vars(date))+
  annotate("text", label = "help", size = 3, x = 3, y = 0.2) +
  geom_density(aes(x=hour))


#average daily density
ggplot(data |> group_by(hour) |> summarise(bin = n())) +
  geom_line(aes(x=hour, y= bin/26280))+
  geom_point(aes(x=hour, y= bin/26280)) 

#histogram of daily view counts
ggplot(data |> group_by(date) |> mutate(n = n()) |> distinct(n))+
  aes(x=n)+
  geom_histogram()


#the day i watched 180 videos
ggplot(data |> ungroup() |> filter(date==as_date("2021-05-13"))) +
  aes(x=time) +
  geom_histogram()



x <- data |> group_by(date) |> summarise(  n = n()  ) |> arrange(date) |> 
             mutate( delta_t = date[row_number()+1] - date)

x |> group_by(delta_t) |> summarise(n = n())


y <- data |> 
  mutate( delta_t = time[row_number()+1] - time) |> drop_na()


ggplot(y) +
  geom_density(aes(x=delta_t))+
  xlim(0,10000)


ggplot(data |> group_by(date) |> summarise(videos_that_day = n()) |>
         mutate(videos_previous_day = videos_that_day[row_number()+1]))+
  aes(y=videos_that_day, x = videos_previous_day)+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')+
  geom_point()
  




response <- GET("https://www.googleapis.com/youtube/v3/videos?id=WmRSBSdCUpQ&part=contentDetails&key=AIzaSyBZpYJo1SjGE_eGTze3RclPnDQoHony9hg")

play_time <- content(response)$items[[1]]$contentDetails$duration |> 
             str_remove_all("[PTS]") |>
             str_split("M")
play_time_seconds = strtoi(play_time[[1]][1])*60+ strtoi(play_time[[1]][2])


words = c("chicken"=0)
for(i in 1:48498){
  for(w in str_split(data$title[i]," ")[[1]]){
    if (is.na(words[w])){ words[w]=1 
    } else {
      words[w]=words[w]+1
     }
    }
}


t<-tibble(word="chicken", count=0)

for(key in names(words)){
  t<-add_row(t, word=key, count=words[key])
}










