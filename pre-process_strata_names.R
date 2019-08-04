library(readr)
library(dplyr)

source("functions/to_alphanumeric_lowercase.R")
strata_clusters <- read.csv("input/Strata_clusters_population.csv", stringsAsFactors=F, check.names=F)

strata_clusters$District <- to_alphanumeric_lowercase(strata_clusters$District)

strata_clusters <- strata_clusters %>% mutate(Stratum = paste0(District,popgroup))
write.csv(strata_clusters,"input_modified/Strata_clusters_population.csv", row.names=F)

results <- c("script_Host","script_IDP_out_of_camp","script_Returnee")
h_samples <- read.csv(sprintf("%s/sampling_frame_%s.csv","input",results[1]), stringsAsFactors = F)
h_samples$popgroup <- "host"
idp_samples <- read.csv(sprintf("%s/sampling_frame_%s.csv","input",results[2]), stringsAsFactors = F)
idp_samples$popgroup <- "idp"
r_samples <- read.csv(sprintf("%s/sampling_frame_%s.csv","input",results[3]), stringsAsFactors = F)
r_samples$popgroup <- "returnee"
names(r_samples) <- sub("Location_name","Location.Name",names(r_samples))
combined_sample <- rbind(h_samples[,c("Governorate", "strata","psu","Location.Name", "popgroup", "pop")],
                         idp_samples[,c("Governorate", "strata", "psu","Location.Name", "popgroup", "pop")],
                         r_samples[,c("Governorate", "strata", "psu","Location.Name", "popgroup", "pop")])

names(combined_sample)[2] <- "district"
combined_sample$district <- to_alphanumeric_lowercase(combined_sample$district)
combined_sample <- combined_sample %>% mutate(stratum = paste0(district,popgroup))
write.csv(combined_sample,"input_modified/Strata_clusters_population.csv", row.names=F)

# questions <- read.csv("c:/Users/REACH-IRQ-GIS/Documents/201905 MCNA KoBo/survey.csv", stringsAsFactors=F, check.names=F)
# questions$type <- tolower(questions$type)
# questions$name <- tolower(questions$name)
# questions$relevant <- tolower(questions$relevant)
# questions$constraint <- tolower(questions$constraint)
# questions$calculation <- tolower(questions$calculation)
# questions$choice_filter <- tolower(questions$choice_filter)
# write.csv(questions, "c:/Users/REACH-IRQ-GIS/Documents/201905 MCNA KoBo/survey_tolower.csv", row.names=F)
# 
# choices <- read.csv("c:/Users/REACH-IRQ-GIS/Documents/201905 MCNA KoBo/choices.csv", stringsAsFactors=F, check.names=F)
# choices$list_name <- tolower(choices$list_name)
# choices$name <- tolower(choices$name)
# choices$Filter <- tolower(choices$Filter)
# write.csv(choices, "c:/Users/REACH-IRQ-GIS/Documents/201905 MCNA KoBo/choices_tolower.csv", row.names=F)
# 
districts <- read.csv("c:/Users/REACH-IRQ-GIS/Documents/2019 MCNA/201905 MCNA KoBo/districts.csv", stringsAsFactors=F, check.names=F)
districts$list_name <- tolower(districts$list_name)
districts$name <- to_alphanumeric_lowercase(districts$label)
districts$filter <- to_alphanumeric_lowercase(districts$filter)
write.csv(districts, "c:/Users/REACH-IRQ-GIS/Documents/2019 MCNA/201905 MCNA KoBo/districts_tolower.csv", row.names=F)
