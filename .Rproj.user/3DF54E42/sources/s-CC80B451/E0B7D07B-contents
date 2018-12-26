library(tidyverse)

tps <- read.csv("data/hvbp_tps.csv")
clinical_care <- read.csv("data/hvbp_clinical.csv")
pat_experience <- read.csv("data/hvbp_patient_experience.csv")
safety <- read.csv("data/hvbp_safety.csv")
efficiency <- read.csv("data/hvbp_efficiency.csv")
hospitals <- read.csv("data/hospital_info.csv")

# clean datasets so that ratings are single digits or NA
clinical <- clinical_care %>% 
  apply(2, function(y) gsub(" out of 10", "", y)) %>% 
  apply(2, function(y) gsub(" out of 9", "", y)) %>% 
  apply(2, function(y) gsub("Not Available", NA, y)) %>% 
  as.data.frame()

pat_exp <- pat_experience %>% 
  apply(2, function(y) gsub(" out of 10", "", y)) %>% 
  apply(2, function(y) gsub(" out of 9", "", y)) %>% 
  apply(2, function(y) gsub("Not Available", NA, y)) %>% 
  as.data.frame()

safe <- safety %>%
  apply(2, function(y) gsub(" out of 10", "", y)) %>% 
  apply(2, function(y) gsub(" out of 9", "", y)) %>% 
  apply(2, function(y) gsub("Not Available", NA, y)) %>% 
  as.data.frame()

eff <- efficiency %>% 
  apply(2, function(y) gsub(" out of 10", "", y)) %>% 
  apply(2, function(y) gsub(" out of 9", "", y)) %>% 
  apply(2, function(y) gsub("Not Available", NA, y)) %>% 
  as.data.frame()

# subset the hospital dataset to only include provider.id, type, ownership
hospital_df <- hospitals[,c("Provider.ID", "Hospital.Type", "Hospital.Ownership")] %>% 
  rename("Provider.Number" = "Provider.ID")

# change type of provider.number in datasets to be integers for merging
clinical$Provider.Number <- as.integer(as.character(clinical$Provider.Number))
eff <- eff %>% 
  rename("Provider.Number" = "Provider_Number")
eff$Provider.Number <- as.integer(as.character(eff$Provider.Number))
safe$Provider.Number <- as.integer(as.character(safe$Provider.Number))
pat_exp$Provider.Number <- as.integer(as.character(pat_exp$Provider.Number))

# left join datasets with hospital dataset
tps <- tps %>% 
  left_join(hospital_df, by = "Provider.Number")
clinical <- clinical %>%
  left_join(hospital_df, by = "Provider.Number")
eff <- eff %>%
  left_join(hospital_df, by = "Provider.Number")
safe <- safe %>% 
  left_join(hospital_df, by = "Provider.Number")
pat_exp <- pat_exp %>% 
  left_join(hospital_df, by = "Provider.Number")

# let's take a look at counts of ownership
ggplot(tps, aes(x=factor(Hospital.Ownership))) +
  geom_bar(stat = "count") +
  labs(x="Hospital Ownership", y="Count", title="Count of Hospitals by Ownership") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))

tps$Total.Performance.Score <- as.numeric(as.character(tps$Total.Performance.Score))

ownership_scores <- tps %>% 
  group_by(Hospital.Ownership) %>% 
  summarize(Mean.Score = mean(Total.Performance.Score, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(Hospital.Ownership, Mean.Score)

ggplot(ownership_scores, aes(x=reorder(Hospital.Ownership, -Mean.Score), y=Mean.Score)) +
  geom_bar(stat = "identity") +
  labs(x="Hospital Ownership", y="Mean Performance Score",
       title="Mean Performance Score by Hospital Ownership") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))
