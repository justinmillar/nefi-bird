# Note: You have to install the darksky package, 
#       and sign-up at https://darksky.net/dev to get an API-KEY
#       then add the key to your .Renviron by running 
#       Sys.setenv(DARKSKY_API_KEY = "API-KEY") in your console

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

