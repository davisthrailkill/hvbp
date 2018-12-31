library(tidyverse)
library(leaflet)
library(shinydashboard)

cities_frame <- readRDS("../data/cities_df.rds")

states = as.data.frame(cities_frame) %>% 
  select(State) %>% 
  unique()
  