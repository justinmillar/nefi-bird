library(darksky)
library(tidyverse)

get_weather_data <- function(site) {
  
  l <- read_csv("data/Locations.csv") %>% 
    filter(Site == site)
  
  lat <- l$Latitude
  lon <- l$Longitude
  
  d <- read_csv(paste("data/", site, ".csv", sep = '')) %>% 
    mutate(Date = as.Date(Night_Number, origin = as.Date("2016-01-01"))) %>% 
    mutate(Time = paste(Date, "T00:00:00", sep = '')) %>% 
    pull(Date) %>% 
    map(~get_forecast_for(lat, lon, .x)) %>% 
    map_df("hourly")
  
  return(d)
  
}

