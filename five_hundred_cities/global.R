library(tidyverse)
library(leaflet)
library(shinydashboard)
library(DT)
library(rgdal)

cities_frame <- readRDS("../data/cities_df.rds")
cities_geo <- readRDS("../data/cities_geo.rds")
tracts_frame <- readRDS("../data/tracts_df.rds")
tracts_geo <- readRDS("../data/tracts_geo.rds")
tracts_shapes <- readOGR("../data/500Cities_Boundaries/CityBoundaries.shp")

states <- as.data.frame(cities_frame) %>% 
  select(State) %>% 
  unique()

cities <- as.data.frame(cities_frame) %>% 
  select(City) %>% 
  unique()

categories <- as.data.frame(cities_frame) %>% 
  select(Category) %>% 
  unique()

prevention_measures <- as.data.frame(cities_frame) %>% 
  filter(Category == "Prevention") %>% 
  select(Short_Question_Text) %>% 
  unique()

outcome_measures <- as.data.frame(cities_frame) %>% 
  filter(Category == "Health Outcomes") %>% 
  select(Short_Question_Text) %>% 
  unique()

behavior_measures <- as.data.frame(cities_frame) %>% 
  filter(Category == "Unhealthy Behaviors") %>% 
  select(Short_Question_Text) %>% 
  unique()

odd_measures <- c("High Blood Pressure",
                  "Taking BP Medication",
                  "Cholesterol Screening",
                  "High Cholesterol")

odd_years <- as.data.frame(cities_frame) %>% 
  filter(Short_Question_Text %in% odd_measures) %>% 
  select(Year) %>% 
  unique()

even_years <- as.data.frame(cities_frame) %>% 
  filter(!Short_Question_Text %in% odd_measures) %>% 
  select(Year) %>% 
  unique()


  