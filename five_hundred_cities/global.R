library(tidyverse)
library(leaflet)
library(shinydashboard)
library(DT)
library(rgdal)
library(sf)
library(plotly)

cities_frame <- readRDS("../data/cities_df.rds")
cities_geo <- readRDS("../data/cities_geo.rds")
tracts_frame <- readRDS("../data/tracts_df.rds")
tracts_geo <- readRDS("../data/tracts_geo.rds")
tracts_shapes <- readOGR("../data/500Cities_Boundaries/CityBoundaries.shp")
combined_city_metrics <- readRDS("../data/combined_city_metrics.rds")
combined_tract_metrics <- readRDS("../data/tracts_metrics.rds")

tracts<-spTransform(tracts_shapes, CRS("+init=epsg:4326"))

states <- as.data.frame(combined_city_metrics) %>% 
  select(State) %>% 
  unique()

cities <- as.data.frame(combined_city_metrics) %>% 
  select(City) %>% 
  unique()

categories <- as.data.frame(combined_city_metrics) %>% 
  select(Category) %>% 
  unique()

# prevention_measures <- as.data.frame(cities_frame) %>% 
#   filter(Category == "Prevention") %>% 
#   select(Short_Question_Text) %>% 
#   unique()
# 
# outcome_measures <- as.data.frame(cities_frame) %>% 
#   filter(Category == "Health Outcomes") %>% 
#   select(Short_Question_Text) %>% 
#   unique()
# 
# behavior_measures <- as.data.frame(cities_frame) %>% 
#   filter(Category == "Unhealthy Behaviors") %>% 
#   select(Short_Question_Text) %>% 
#   unique()

# odd_measures <- c("High Blood Pressure",
#                   "Taking BP Medication",
#                   "Cholesterol Screening",
#                   "High Cholesterol")
# 
# odd_years <- as.data.frame(cities_frame) %>% 
#   filter(Short_Question_Text %in% odd_measures) %>% 
#   select(Year) %>% 
#   unique()
# 
# even_years <- as.data.frame(cities_frame) %>% 
#   filter(!Short_Question_Text %in% odd_measures) %>% 
#   select(Year) %>% 
#   unique()


  