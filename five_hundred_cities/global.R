library(tidyverse)
library(dplyr)
library(leaflet)
library(shinydashboard)
library(DT)
library(rgdal)
library(sf)
library(plotly)
library(tmap)
library(tmaptools)
library(raster)


cities_frame <- readRDS("../data/cities_df.rds")
cities_geo <- readRDS("../data/cities_geo.rds")
tracts_frame <- readRDS("../data/tracts_df.rds")
tracts_geo <- readRDS("../data/tracts_geo.rds")
tracts_shapes <- st_read("../data/500Cities_Boundaries/CityBoundaries.shp",
                         stringsAsFactors = FALSE)
combined_city_metrics <- readRDS("../data/combined_city_metrics_v2.rds")
combined_tract_metrics <- readRDS("../data/tracts_metrics.rds")
combined_citytract <- readRDS("../data/combined_citytracts.rds")

# tracts<-spTransform(tracts_shapes, CRS("+init=epsg:4326"))

# us_states <- combined_city_metrics %>% 
#   aggregate_map(by = "State")

states <- combined_tract_metrics %>% 
  dplyr::select(State) %>% 
  unique()

cities <- combined_tract_metrics %>% 
  dplyr::select(City) %>% 
  unique()

categories <- combined_tract_metrics %>% 
  dplyr::select(Category) %>% 
  unique()

in_msrs <- combined_tract_metrics %>% 
  filter(Category != "Health Outcomes") %>% 
  dplyr::select(Measure) %>% 
  unique()

out_msrs <- combined_tract_metrics %>% 
  filter(Category == "Health Outcomes") %>% 
  dplyr::select(Measure) %>% 
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


  