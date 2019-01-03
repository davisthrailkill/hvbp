library(tidyverse)
library(leaflet)
library(shinydashboard)

cities_frame <- readRDS("../data/cities_df.rds")

states <- as.data.frame(cities_frame) %>% 
  select(State) %>% 
  unique()

categories <- as.data.frame(cities_frame) %>% 
  select(Category) %>% 
  unique()

measures <- as.data.frame(cities_frame) %>% 
  select(Short_Question_Text) %>% 
  unique()

year <- as.data.frame(cities_frame) %>% 
  select(Year) %>% 
  unique()


  