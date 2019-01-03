library(tidyverse)
library(DT)
library(ggthemes)
library(mapproj)
library(shinydashboard)

specialties_year <- readRDS("data/specialties_year.rds")
top5_specialties_rates <- readRDS("data/top5_specs_year.rds")
providers <- readRDS("data/cms_state_providers_specs.rds")
state_rates <- readRDS("data/cms_state_providers.rds")
providers_table <- readRDS("data/opioid_providers_table.rds")
prescription_rate_map <- readRDS("data/prescription_rate_map.rds")

specialties <- as.data.frame(providers) %>% 
  select(specialty_description) %>% 
  unique()

states <- as.data.frame(providers) %>% 
  select(nppes_provider_state) %>% 
  unique()

years <- as.data.frame(providers) %>% 
  select(year) %>% 
  unique()