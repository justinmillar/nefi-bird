# Exploratory Data Analysis

library(tidyverse)

df <- list.files("data", "*.csv", full.names = TRUE) %>%  # Get vector of .csv filenames
  setdiff("data/Locations.csv") %>%                       # Remove location csv
  set_names(nm = gsub("*data/(.*).csv", "\\1", .)) %>%    # Name vector of id field
  map_df(~read_csv(.), .id = "Location")                  # Bring all files in single dataframe

# Plot Nightly Counts
ggplot(df, aes(x = Night_Number, y = NightCount)) +
  geom_point() + 
  facet_wrap(~ Location)

# Note some gaps in the data, notably in the middle of Erie A data
  
# Identify potential outliers
df %>% 
  filter(NightCount < 0 | NightCount > 20000)

# Plot NightlyTPR
ggplot(df, aes(x = Night_Number, y = NightTPR)) +
  geom_point() + 
  facet_wrap(~ Location)
