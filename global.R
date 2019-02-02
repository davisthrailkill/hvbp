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


cities_frame <- readRDS("data/cities_df.rds")
cities_geo <- readRDS("data/cities_geo.rds")
tracts_frame <- readRDS("data/tracts_df.rds")
tracts_geo <- readRDS("data/tracts_geo.rds")
combined_city_metrics <- readRDS("data/combined_city_metrics_v2.rds")
combined_tract_metrics <- readRDS("data/tracts_metrics.rds")
combined_citytract <- readRDS("data/combined_citytracts.rds")
cities_wide <- readRDS("data/cities_wide.rds")



states <- combined_tract_metrics %>% 
  dplyr::select(State) %>% 
  unique()

scatter_states <- cities_wide %>% 
  dplyr::select(State) %>% 
  unique()

cities <- combined_tract_metrics %>% 
  dplyr::select(City) %>% 
  unique()

categories <- combined_tract_metrics %>% 
  dplyr::select(Category) %>% 
  unique()

in_msrs <- combined_city_metrics %>% 
  filter(Category != "Health Outcomes" & Measure != "Violent crime") %>% 
  dplyr::select(Measure) %>% 
  unique()

out_msrs <- combined_city_metrics %>% 
  filter(Category == "Health Outcomes") %>% 
  dplyr::select(Measure) %>%
  unique()

downloadTable <- combined_city_metrics %>% 
  dplyr::select(State, City, Category, Measure, Estimate, Population) %>% 
  arrange(State, City, Category, desc(Estimate))


  