# Covariates
library(tidyverse)
source("r/get-weather.R")


wind <- function(x){
  y <- x-45
  y <- ifelse(y > 0 , y, y + 360)
  y <- ceiling(y/90)
  y
}



df <- list.files("data", "*.csv", full.names = TRUE) %>%  # Get vector of .csv filenames
  setdiff("data/Locations.csv") %>%                       # Remove location csv
  gsub("*data/(.*).csv", "\\1", .) %>% 
  map(~ get_weather_data(.)) %>% 
  map_df(.) 


df1 <-  df %>% 
  as.tbl() %>% 
  separate(time, c("date", "time"), sep = " ") %>%
  separate(time, c('hour'), sep = ":") %>% 
  mutate(hour = as.numeric(hour)) %>%  
  filter(hour >= 21 | hour <= 5) %>% 
  slice(-1:-5) %>% 
  group_by(Site, date) %>% 
  summarize(windSpeed = mean(windSpeed), 
            windBearing = mean(windBearing)) %>%
  mutate(windCat = wind(windBearing), 
         date_j = yday(date)) %>% 
  mutate(windCat = if_else(windSpeed < 5, 0, windCat))
  

dat %>% 
  as.tbl() %>% 
  separate(time, c("date", "time"), sep = " ") %>%
  separate(time, c('hour'), sep = ":") %>% 
  mutate(hour = as.numeric(hour)) %>%  
  filter(hour >= 21 | hour <= 5) %>% 
  slice(-1:-5) %>% 
  group_by(Site, date) %>% 
  summarize(windSpeed = mean(windSpeed), 
            windBearing = mean(windBearing)) %>%
  mutate(windCat = wind(windBearing), 
         date_j = yday(date)) %>% 
  mutate(windCat = if_else(windSpeed < 5, 0, windCat))


