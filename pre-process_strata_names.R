library(readr)
library(dplyr)

source("functions/to_alphanumeric_lowercase.R")
strata_clusters <- read.csv("input/Strata_clusters_population.csv", stringsAsFactors=F, check.names=F)

strata_clusters$District <- to_alphanumeric_lowercase(strata_clusters$District)

strata_clusters <- strata_clusters %>% mutate(Stratum = paste0(District,popgroup))
write.csv(strata_clusters,"input_modified/Strata_clusters_population.csv", row.names=F)


choices <- read.csv("input/kobo_choices.csv", stringsAsFactors = FALSE)
choices %>% 
  mutate(name = ifelse(list_name=="District_MCNA", to_alphanumeric_lowercase(label), name)) %>% 
  write.csv("input_modified/kobo_choices2.csv", row.names=F, quote=F,na="")
