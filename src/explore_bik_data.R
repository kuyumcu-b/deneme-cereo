# Explore the bike data characteristics
# 
# Explore the bike data to look at the relationship between
# temperature and number of riders
#
# Step 1: Load the bike data and look at the metadata

library(tidyverse)

### Read in the data ----
bike <- read.csv("data/daily_bike_data.csv")

head(bike)
str(bike)
sort(names(bike))

# Transform dteday to date. In the workshop, it is default
bike$dteday <- as.Date(bike$dteday)

# Time trend of ridership
ggplot(data = bike) +
  geom_line(aes(x = dteday, y = cnt))

ggplot(data = bike) +
  geom_point(aes(x = temp, y = cnt))

### Data cleaning ----
# dplyr verbs for data transformations
#   select: select columns that we want to keep
#   filter: select rows that we want to keep
#   mutate: transform data while keeping other columns
#   transmute: creates new columns and does not keep old columns
#   %>%: "pipe" pipes the output from one command as the input
#       for the next command
bike %>% select(dteday, season, weathersit, temp, cnt)
select(bike, dteday, season, weathersit, temp, cnt)

# One way of selecting spring records and jus a few cols
spring_bike <- filter(bike, season == "spring")
spring_bike_temp_cnt <- select(spring_bike, temp, cnt)

spring_bike_temp_cnt2 <- bike %>%
  filter(season == "spring") %>%
  select(temp, cnt)

## Exercise: select weathersit and cnt for all winter records
# own work:
winter_bike_weathersit_cnt <- bike %>%
  filter(season == "winter") %>%
  select(weathersit, cnt)

### Mutate and Transmute with Factors and Dates
summary(bike$weathersit)
