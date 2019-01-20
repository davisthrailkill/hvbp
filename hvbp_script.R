library(tidyverse)
library(dplyr)
library(ggmap)
library(leaflet)
library(sf)

# tps <- read.csv("data/hvbp_tps.csv")
# clinical_care <- read.csv("data/hvbp_clinical.csv")
# pat_experience <- read.csv("data/hvbp_patient_experience.csv")
# safety <- read.csv("data/hvbp_safety.csv")
# efficiency <- read.csv("data/hvbp_efficiency.csv")
# hospitals <- read.csv("data/hospital_info.csv", stringsAsFactors = FALSE)
cities16 <- read.csv("data/500_cities_2016.csv")
cities18 <- read.csv("data/500_cities_2018.csv")
chdb_city <- read.csv("data/chdb_city.csv")
chdb_tract <- read.csv("data/chdb_tract.csv")
tract_zip <- read.csv("data/tract_zip.csv")
tracts_shapes <- st_read("data/500Cities_Boundaries/CityBoundaries.shp")

# # clean datasets
# # tps
# tps[tps == "Not Available"] <- NA
# tps$Unweighted.Normalized.Clinical.Care.Domain.Score <- as.numeric(as.character(tps$Unweighted.Normalized.Clinical.Care.Domain.Score))
# tps$Weighted.Normalized.Clinical.Care.Domain.Score <- as.numeric(as.character(tps$Weighted.Normalized.Clinical.Care.Domain.Score))
# tps$Unweighted.Patient.and.Caregiver.Centered.Experience.of.Care.Care.Coordination.Domain.Score <-
#   as.numeric(as.character(tps$Unweighted.Patient.and.Caregiver.Centered.Experience.of.Care.Care.Coordination.Domain.Score))
# tps$Weighted.Patient.and.Caregiver.Centered.Experience.of.Care.Care.Coordination.Domain.Score <-
#   as.numeric(as.character(tps$Weighted.Patient.and.Caregiver.Centered.Experience.of.Care.Care.Coordination.Domain.Score))
# tps$Unweighted.Normalized.Efficiency.and.Cost.Reduction.Domain.Score <- as.numeric(as.character(tps$Unweighted.Normalized.Efficiency.and.Cost.Reduction.Domain.Score))
# tps$Weighted.Efficiency.and.Cost.Reduction.Domain.Score <- as.numeric(as.character(tps$Weighted.Efficiency.and.Cost.Reduction.Domain.Score))
# tps$Unweighted.Normalized.Safety.Domain.Score <- as.numeric(as.character(tps$Unweighted.Normalized.Safety.Domain.Score))
# tps$Weighted.Safety.Domain.Score <- as.numeric(as.character(tps$Weighted.Safety.Domain.Score))
# tps$Total.Performance.Score <- as.numeric(as.character(tps$Total.Performance.Score))
# 
# # clinical
# clinical <- clinical_care %>% 
#   apply(2, function(y) gsub(" out of 10", "", y)) %>% 
#   apply(2, function(y) gsub(" out of 9", "", y)) %>% 
#   apply(2, function(y) gsub("Not Available", NA, y)) %>% 
#   as.data.frame()
# 
# #clinical$MORT.30.AMI.Achievement.Threshold <- as.numeric(clinical$MORT.30.AMI.Achievement.Threshold)
# #clinical$MORT.30.AMI.Benchmark <- as.numeric(clinical$MORT.30.AMI.Benchmark)
# clinical$MORT.30.AMI.Baseline.Rate <- as.numeric(as.character(clinical$MORT.30.AMI.Baseline.Rate))
# clinical$MORT.30.AMI.Performance.Rate <- as.numeric(as.character(clinical$MORT.30.AMI.Performance.Rate))
# #clinical$MORT.30.HF.Achievement.Threshold <- as.numeric(clinical$MORT.30.HF.Achievement.Threshold)
# #clinical$MORT.30.HF.Benchmark <- as.numeric(clinical$MORT.30.HF.Benchmark)
# clinical$MORT.30.HF.Baseline.Rate <- as.numeric(as.character(clinical$MORT.30.HF.Baseline.Rate))
# clinical$MORT.30.HF.Performance.Rate <- as.numeric(as.character(clinical$MORT.30.HF.Performance.Rate))
# #clinical$MORT.30.PN.Achievement.Threshold <- as.numeric(clinical$MORT.30.PN.Achievement.Threshold)
# #clinical$MORT.30.PN.Benchmark <- as.numeric(clinical$MORT.30.PN.Benchmark)
# clinical$MORT.30.PN.Baseline.Rate <- as.numeric(as.character(clinical$MORT.30.PN.Baseline.Rate))
# clinical$MORT.30.PN.Performance.Rate <- as.numeric(as.character(clinical$MORT.30.PN.Performance.Rate))
# 
# # patient experience
# pat_exp <- pat_experience %>% 
#   apply(2, function(y) gsub(" out of 10", "", y)) %>% 
#   apply(2, function(y) gsub(" out of 9", "", y)) %>% 
#   apply(2, function(y) gsub("Not Available", NA, y)) %>% 
#   as.data.frame()
# 
# pat_exp$Communication.with.Nurses.Baseline.Rate <- as.numeric(as.character(pat_exp$Communication.with.Nurses.Baseline.Rate))
# pat_exp$Communication.with.Nurses.Performance.Rate <- as.numeric(as.character(pat_exp$Communication.with.Nurses.Performance.Rate))
# pat_exp$Communication.with.Doctors.Baseline.Rate <- as.numeric(as.character(pat_exp$Communication.with.Doctors.Baseline.Rate))
# pat_exp$Communication.with.Doctors.Performance.Rate <- as.numeric(as.character(pat_exp$Communication.with.Doctors.Performance.Rate))
# pat_exp$Responsiveness.of.Hospital.Staff.Baseline.Rate <- as.numeric(as.character(pat_exp$Responsiveness.of.Hospital.Staff.Baseline.Rate))
# pat_exp$Responsiveness.of.Hospital.Staff.Performance.Rate <- as.numeric(as.character(pat_exp$Responsiveness.of.Hospital.Staff.Performance.Rate))
# pat_exp$Care.Transition.Baseline.Rate <- as.numeric(as.character(pat_exp$Care.Transition.Baseline.Rate))
# pat_exp$Care.Transition.Performance.Rate <- as.numeric(as.character(pat_exp$Care.Transition.Performance.Rate))
# pat_exp$Communication.about.Medicines.Baseline.Rate <- as.numeric(as.character(pat_exp$Communication.about.Medicines.Baseline.Rate))
# pat_exp$Communication.about.Medicines.Performance.Rate <- as.numeric(as.character(pat_exp$Communication.about.Medicines.Performance.Rate))
# pat_exp$Cleanliness.and.Quietness.of.Hospital.Environment.Baseline.Rate <-
#   as.numeric(as.character(pat_exp$Cleanliness.and.Quietness.of.Hospital.Environment.Baseline.Rate))
# pat_exp$Cleanliness.and.Quietness.of.Hospital.Environment.Performance.Rate <-
#   as.numeric(as.character(pat_exp$Cleanliness.and.Quietness.of.Hospital.Environment.Performance.Rate))
# pat_exp$Discharge.Information.Baseline.Rate <- as.numeric(as.character(pat_exp$Discharge.Information.Baseline.Rate))
# pat_exp$Discharge.Information.Performance.Rate <- as.numeric(as.character(pat_exp$Discharge.Information.Performance.Rate))
# pat_exp$Overall.Rating.of.Hospital.Baseline.Rate <- as.numeric(as.character(pat_exp$Overall.Rating.of.Hospital.Baseline.Rate))
# pat_exp$Overall.Rating.of.Hospital.Performance.Rate <- as.numeric(as.character(pat_exp$Overall.Rating.of.Hospital.Performance.Rate))
# 
# # safety
# safe <- safety %>%
#   apply(2, function(y) gsub(" out of 10", "", y)) %>% 
#   apply(2, function(y) gsub(" out of 9", "", y)) %>% 
#   apply(2, function(y) gsub("Not Available", NA, y)) %>% 
#   as.data.frame()
# 
# safe$PSI.90.Baseline.Rate <- as.numeric(as.character(safe$PSI.90.Baseline.Rate))
# safe$PSI.90.Performance.Rate <- as.numeric(as.character(safe$PSI.90.Performance.Rate))
# safe$HAI.1.Baseline.Rate <- as.numeric(as.character(safe$HAI.1.Baseline.Rate))
# safe$HAI.1.Performance.Rate <- as.numeric(as.character(safe$HAI.1.Performance.Rate))
# safe$HAI.2.Baseline.Rate <- as.numeric(as.character(safe$HAI.2.Baseline.Rate))
# safe$HAI.2.Performance.Rate <- as.numeric(as.character(safe$HAI.2.Performance.Rate))
# safe$HAI.3.Baseline.Rate <- as.numeric(as.character(safe$HAI.3.Baseline.Rate))
# safe$HAI.3.Performance.Rate <- as.numeric(as.character(safe$HAI.3.Performance.Rate))
# safe$HAI.4.Baseline.Rate <- as.numeric(as.character(safe$HAI.4.Baseline.Rate))
# safe$HAI.4.Performance.Rate <- as.numeric(as.character(safe$HAI.4.Performance.Rate))
# safe$HAI.5.Baseline.Rate <- as.numeric(as.character(safe$HAI.5.Baseline.Rate))
# safe$HAI.5.Performance.Rate <- as.numeric(as.character(safe$HAI.5.Performance.Rate))
# safe$HAI.6.Baseline.Rate <- as.numeric(as.character(safe$HAI.6.Baseline.Rate))
# safe$HAI.6.Performance.Rate <- as.numeric(as.character(safe$HAI.6.Performance.Rate))
# safe$PC.01.Baseline.Rate <- as.numeric(as.character(safe$PC.01.Baseline.Rate))
# safe$PC.01.Performance.Rate <- as.numeric(as.character(safe$PC.01.Performance.Rate))
# 
# # efficiency
# eff <- efficiency %>% 
#   apply(2, function(y) gsub(" out of 10", "", y)) %>% 
#   apply(2, function(y) gsub(" out of 9", "", y)) %>% 
#   apply(2, function(y) gsub("Not Available", NA, y)) %>% 
#   as.data.frame()
# 
# eff$MSPB.1.Baseline.Rate <- as.numeric(as.character(eff$MSPB.1.Baseline.Rate))
# eff$MSPB.1.Performance.Rate <- as.numeric(as.character(eff$MSPB.1.Performance.Rate))
# 
# # geocoding hospital locations
# #hospitals_first_half <- hospitals[1:2500,]
# #hospital_first_locations <- paste(hospitals_first_half$Address,
#                                   #hospitals_first_half$City,
#                                   #hospitals_first_half$State,
#                                   #hospitals_first_half$ZIP.Code)
# #hospital_coords <- geocode(hospital_first_locations)
# 
# #for(i in 1:nrow(hospitals)){
#   #result <- geocode(hospitals$Location[i], output = "latlon", source = "google")
#   #hospitals$lat[i] <- as.numeric(result[1])
#   #hospitals$lon[i] <- as.numeric(result[2])
# #}
# 
# 
# # subset the hospital dataset to only include provider.id, type, ownership
# hospital_df <- hospitals[,c("Provider.ID", "Hospital.Type", "Hospital.Ownership")] %>% 
#   rename("Provider.Number" = "Provider.ID")
# 
# # hospital_frame <- data.frame(cbind(hospital_df, hospital_coords))
# 
# # change type of provider.number in datasets to be integers for merging
# tps$Provider.Number <- as.integer(as.character(tps$Provider.Number))
# clinical$Provider.Number <- as.integer(as.character(clinical$Provider.Number))
# eff <- eff %>% 
#   rename("Provider.Number" = "Provider_Number")
# eff$Provider.Number <- as.integer(as.character(eff$Provider.Number))
# safe$Provider.Number <- as.integer(as.character(safe$Provider.Number))
# pat_exp$Provider.Number <- as.integer(as.character(pat_exp$Provider.Number))
# 
# # left join datasets with hospital dataset
# tps <- tps %>% 
#   left_join(hospital_df, by = "Provider.Number")
# clinical <- clinical %>%
#   left_join(hospital_df, by = "Provider.Number")
# eff <- eff %>%
#   left_join(hospital_df, by = "Provider.Number")
# safe <- safe %>% 
#   left_join(hospital_df, by = "Provider.Number")
# pat_exp <- pat_exp %>% 
#   left_join(hospital_df, by = "Provider.Number")
# 
# # let's take a look at counts by ownership
# ggplot(tps, aes(x=factor(Hospital.Ownership))) +
#   geom_bar(stat = "count") +
#   labs(x="Hospital Ownership", y="Count", title="Count of Hospitals by Ownership") +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))
# 
# # let's take a look at counts by type
# ggplot(tps, aes(x=factor(Hospital.Type))) +
#   geom_bar(stat = "count") +
#   labs(x="Hospital Type", y="Count", title="Count of Hospitals by Type") +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))
# 
# 
# ownership_scores <- tps %>% 
#   group_by(Hospital.Ownership) %>% 
#   summarize(Mean.Score = mean(Total.Performance.Score, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   dplyr::select(Hospital.Ownership, Mean.Score)
# 
# ggplot(ownership_scores, aes(x=reorder(Hospital.Ownership, -Mean.Score), y=Mean.Score)) +
#   geom_bar(stat = "identity") +
#   labs(x="Hospital Ownership", y="Mean Performance Score",
#        title="Mean Performance Score by Hospital Ownership") +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))




### 500 CITIES ###

tracts_shapes$STFIPS <- as.numeric(as.character(tracts_shapes$STFIPS))
tracts_shapes$PLACEFIPS <- as.numeric(as.character(tracts_shapes$PLACEFIPS))  
tracts_shapes$STPLFIPS <- as.numeric(as.character(tracts_shapes$STPLFIPS))

tracts_shapes_df <- tracts_shapes %>% 
  dplyr::select(STPLFIPS, geometry)


cities18 <- cities18 %>% 
  rename("Population2010" = "PopulationCount")

cities16v2 <- dplyr::select(cities16, -c("Data_Value_Footnote_Symbol", "Data_Value_Footnote"))
cities18v2 <- dplyr::select(cities18, -c("Data_Value_Footnote_Symbol", "Data_Value_Footnote"))

cities_combined <- bind_rows(cities16v2, cities18v2)
cities_combined <- cities_combined %>%
  rename("State" = "StateDesc",
         "City" = "CityName")

cities_combined <- cities_combined %>% 
  separate(GeoLocation, into = c("Lat", "Long"), sep = ",", remove = FALSE)

cities_combined$Lat <- gsub("\\(", "", cities_combined$Lat)
cities_combined$Long <- gsub("\\)", "", cities_combined$Long)

cities_combined$Lat <- as.numeric(cities_combined$Lat)
cities_combined$Long <- as.numeric(cities_combined$Long)

cities_combined$id <- paste(as.character(cities_combined$UniqueID), as.character(cities_combined$Measure),
                            sep = "_")


cities_geography <- cities_combined %>% 
  filter(StateAbbr != "US" & is.na(TractFIPS)) %>% 
  dplyr::select(StateAbbr, State, City, CityFIPS, GeoLocation, Lat, Long)

tract_geography <- cities_combined %>% 
  filter(StateAbbr != "US" & !is.na(TractFIPS)) %>% 
  dplyr::select(StateAbbr, State, City, CityFIPS, TractFIPS, GeoLocation, Lat, Long)

cities_geo <- cities_geography %>% 
  distinct() %>% 
  saveRDS(file = "data/cities_geo.rds")

tracts_geo <- tract_geography %>% 
  distinct() %>% 
  saveRDS(file = "data/tracts_geo.rds")

cities_combined_v2 <- cities_combined %>% 
  dplyr::select(Year, StateAbbr, State, City, GeographicLevel, Category, Measure, Data_Value_Type,
         Data_Value, Population2010, GeoLocation, Lat, Long, CityFIPS, TractFIPS, 
         Short_Question_Text, id) %>% 
  spread(Data_Value_Type, Data_Value)

cities_combined_v2 <- dplyr::select(cities_combined_v2, -c("Year", "Age-adjusted prevalence"))
  
id_meanvalue <- cities_combined_v2 %>% 
  group_by(id) %>% 
  summarize(mean_data_value = mean(`Crude prevalence`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  dplyr::select(id, mean_data_value)

cities_combined_v3 <- cities_combined_v2 %>% 
  left_join(id_meanvalue, by = "id")

cities_combined_v4 <- dplyr::select(cities_combined_v3, -`Crude prevalence`)

cities_combined_v5 <- distinct(cities_combined_v4)


# extract cities from cities_combined
cities_df <- cities_combined_v5 %>% 
  filter(GeographicLevel != "Census Tract") #%>% 
  #saveRDS(file = "data/cities_df.rds")

# extract tracts from cities_combined
tracts_df <- cities_combined_v5 %>% 
  filter(GeographicLevel == "Census Tract") #%>% 
  #saveRDS(file = "data/tracts_df.rds")





#### Looking at City Health Dashboard Data - Unsure if this will be used

social_economic <- c("High school graduation", "Racial/ethnic diversity",
                     "Third-grade reading proficiency","Absenteeism",
                     "Children in poverty", "Housing cost, excessive",
                     "Income inequality", "Neighborhood racial/ethnic segregation",
                     "Unemployment", "Violent crime")

physical_environment <- c("Park access", "Walkability", "Air pollution - particulate matter",
                          "Housing with Potential Lead Risk", "Lead exposure risk index",
                          "Limited access to healthy foods")

health_behaviors <- c("Binge drinking", "Physical inactivity", "Smoking", "Teen births")

health_outcomes <- c("Breast dancer deaths", "Cardiovascular disease deaths", 
                     "Colorectal cancer deaths", "Diabetes", "Frequent mental distress",
                     "Frequent physical distress", "High blood bressure", "Life expectancy",
                     "Low birthweight", "Obesity", "Opioid overdose deaths", 
                     "Premature deaths (all causes)")

clinical_care <- c("Dental care", "Prenatal care", "Preventive services", "Uninsured")

chdb_city$category <- ifelse(chdb_city$metric_name %in% social_economic, "Social and Economic Factors",
                      ifelse(chdb_city$metric_name %in% physical_environment, "Physical Environment",
                      ifelse(chdb_city$metric_name %in% health_behaviors, "Unhealthy Behaviors",
                      ifelse(chdb_city$metric_name %in% health_outcomes, "Health Outcomes",
                      ifelse(chdb_city$metric_name %in% clinical_care, "Prevention", NA)))))

chdb_city$category <- as.factor(chdb_city$category)

chdb_tract$category <- ifelse(chdb_tract$metric_name %in% social_economic, "Social and Economic Factors",
                       ifelse(chdb_tract$metric_name %in% physical_environment, "Physical Environment",
                       ifelse(chdb_tract$metric_name %in% health_behaviors, "Unhealthy Behaviors",
                       ifelse(chdb_tract$metric_name %in% health_outcomes, "Health Outcomes",
                       ifelse(chdb_tract$metric_name %in% clinical_care, "Prevention", NA)))))

chdb_tract$category <- as.factor(chdb_tract$category)

chdb_city_geo <- chdb_city %>% 
  left_join(cities_geography, by = c("city_name" = "City", "stpl_fips" = "CityFIPS")) %>% 
  distinct()

chdb_city_socecon <- chdb_city_geo %>% 
  filter(category == "Social and Economic Factors" & group_name == "total population")

chdb_cities <- unique(chdb_city_socecon$city_name)
cities_chdb_df <- cities_df %>%
  filter(City %in% chdb_cities)

city_pops <- cities_chdb_df %>% 
  dplyr::select(CityFIPS, Population2010) %>% 
  distinct()

chdb_city_socecon <- chdb_city_socecon %>% 
  left_join(city_pops, by = c("stpl_fips" = "CityFIPS"))

chdb_city_socecon_v2 <- chdb_city_socecon %>% 
  dplyr::select(StateAbbr, State, city_name, stpl_fips, geo_level, category, metric_name,
         est, Population2010, GeoLocation, Lat, Long)

chdb_city_socecon_v3 <- chdb_city_socecon_v2 %>% 
  rename(City = city_name, CityFIPS = stpl_fips,
           GeographicLevel = geo_level, Category = category, Measure = metric_name,
           Estimate = est, Population = Population2010)

# need to dplyr::select only relevant columns from cities_chdb_df, then bind_rows with socecon_v2
cities_chdb_df_v2 <- cities_chdb_df %>%
  dplyr::select(StateAbbr, State, City, CityFIPS, GeographicLevel, Category, Short_Question_Text,
         mean_data_value, Population2010, GeoLocation, Lat, Long) %>% 
  rename(Measure = Short_Question_Text, Estimate = mean_data_value, Population = Population2010)

# convert Year to factor in cities_chdb_df_v2 in order to bind
# cities_chdb_df_v2$Year <- as.factor(as.character(cities_chdb_df_v2$Year))

# bind rows cities_chdb_df_v2 and chdb_city_socecon_v3
combined_metrics_df <- bind_rows(cities_chdb_df_v2, chdb_city_socecon_v3)

# clean character columns to factors
combined_metrics_df$City <- as.factor(combined_metrics_df$City)
combined_metrics_df$Category <- as.factor(combined_metrics_df$Category)
combined_metrics_df$Measure <- as.factor(combined_metrics_df$Measure)

# combined_metrics_df <- combined_metrics_df %>% 
#   rename(FIPS = CityFIPS)

# combined_citymetrics_df <- combined_metrics_df %>% 
#   rename(FIPS = CityFIPS)

combined_metrics_df_v2 <- combined_metrics_df %>% 
  left_join(tracts_shapes_df, by = c("CityFIPS" = "STPLFIPS")) %>% 
  rename(FIPS = CityFIPS)


chdb_tract_geo <- chdb_tract %>% 
  left_join(tract_geography, by = c("city_name" = "City", "stcotr_fips" = "TractFIPS")) %>% 
  distinct()

chdb_tract_socecon <- chdb_tract_geo %>% 
  filter(category == "Social and Economic Factors" & group_name == "total population")

tracts_chdb <- unique(chdb_tract_socecon$stcotr_fips)
tracts_chdb_df <- tracts_df %>% 
  filter(TractFIPS %in% tracts_chdb)

# tract_pops <- tracts_chdb_df %>%
#   dplyr::select(TractFIPS, Population2010) %>%
#   distinct()
# 
# chdb_tract_socecon <- chdb_tract_socecon %>% 
#   left_join(tract_pops, by = c("stcotr_fips" = "TractFIPS"))

counties_lookup <- chdb_tract_socecon %>% 
  dplyr::select(county_name, stcotr_fips)

chdb_tract_socecon_v2 <- chdb_tract_socecon %>% 
  dplyr::select(StateAbbr, State, city_name, county_name, CityFIPS, stcotr_fips, geo_level, category, metric_name,
         est, denom, GeoLocation, Lat, Long)

chdb_tract_socecon_v3 <- chdb_tract_socecon_v2 %>% 
  rename(City = city_name, County = county_name, TractFIPS = stcotr_fips,
          GeographicLevel = geo_level, Category = category, Measure = metric_name,
          Estimate = est, Population = denom)

tracts_chdb_df <- tracts_chdb_df %>% 
  left_join(counties_lookup, by = c("TractFIPS" = "stcotr_fips")) %>% 
  distinct()

tracts_chdb_df_v2 <- tracts_chdb_df %>%
  dplyr::select(StateAbbr, State, City, county_name, CityFIPS, TractFIPS, GeographicLevel, Category, Short_Question_Text,
         mean_data_value, Population2010, GeoLocation, Lat, Long) %>% 
  rename(County = county_name, Measure = Short_Question_Text, Estimate = mean_data_value, Population = Population2010)

# convert Year to factor in cities_chdb_df_v2 in order to bind
# tracts_chdb_df_v2$Year <- as.factor(as.character(tracts_chdb_df_v2$Year))

# bind rows cities_chdb_df_v2 and chdb_city_socecon_v3
tracts_metrics_df <- bind_rows(tracts_chdb_df_v2, chdb_tract_socecon_v3)

# clean character columns to factors
tracts_metrics_df$City <- as.factor(tracts_metrics_df$City)
tracts_metrics_df$Category <- as.factor(tracts_metrics_df$Category)
tracts_metrics_df$Measure <- as.factor(tracts_metrics_df$Measure)
tracts_metrics_df$County <- as.factor(tracts_metrics_df$County)

tracts_metrics_df <- tracts_metrics_df %>% 
  rename(FIPS = CityFIPS)

# combined_tractmetrics_df <- tracts_metrics_df %>% 
#   rename(FIPS = TractFIPS)

tracts_metrics_df_v2 <- tracts_metrics_df %>% 
  left_join(tracts_shapes_df, by = c("FIPS" = "STPLFIPS"))

# mean_check <- combined_city_metrics %>% 
#   filter(Measure == "Health Insurance")

# measure_exp_df <- combined_metrics_df_v2 %>% 
#   dplyr::select(Year, StateAbbr, State, City, FIPS, GeographicLevel, Measure, Estimate, Population,
#          GeoLocation, Lat, Long) %>% 
#   spread(key = Measure, value = Estimate)


# bind rows - cities and tracts into one
# combined_citytracts <- bind_rows(combined_metrics_df_v2, tracts_metrics_df_v2)

# change city, geographic level, and measure into factor
# combined_citytracts$City <- as.factor(combined_citytracts$City)
# combined_citytracts$GeographicLevel <- as.factor(combined_citytracts$GeographicLevel)
# combined_citytracts$Measure <- as.factor(combined_citytracts$Measure)
# 
# combined_citytracts$GeographicLevel[combined_citytracts$GeographicLevel == "city"] <- "City"
# combined_citytracts$GeographicLevel[combined_citytracts$GeographicLevel == "tract"] <- "Census Tract"
# 
# 
# # save final combined into an RDS file
# saveRDS(combined_citytracts, "data/combined_citytracts.rds")


# save combined metrics (cities) into rds file
saveRDS(combined_metrics_df_v2, "data/combined_city_metrics_v2.rds")

# save combined metrics (cities) into rds file
 saveRDS(tracts_metrics_df, "data/tracts_metrics.rds")

# save combined metrics (cities) into rds file
# saveRDS(tracts_metrics_df_v2, "data/tracts_metrics_v2.rds")




# joining chdb tract data with zip codes
#chdb_tract_geo_v2 <- chdb_tract_geo %>% 
  #left_join(tract_zip, by = c("stcotr_fips" = "tract"))
