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


  