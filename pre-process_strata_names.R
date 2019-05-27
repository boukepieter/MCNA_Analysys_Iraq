source("functions/to_alphanumeric_lowercase.R")
strata_clusters <- read.csv("input/Strata_clusters_population.csv", stringsAsFactors=F, check.names=F)
strata_clusters$District <- to_alphanumeric_lowercase(strata_clusters$District)
strata_clusters$Stratum <- paste0()
library(dplyr)
strata_clusters <- strata_clusters %>% mutate(Stratum = paste0(District,popgroup))
write.csv(strata_clusters,"input/Strata_clusters_population.csv", row.names=F)

library(readr)
choices <- read_csv("input/kobo_choices.csv")
choices %>% 
  #filter(list_name=="District_MCNA") %>% 
  mutate(name = ifelse(list_name=="District_MCNA", to_alphanumeric_lowercase(label), name)) %>% 
  write.csv("input/kobo_choices2.csv", row.names=F, quote=F,na="")
