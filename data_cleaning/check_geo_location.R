setwd("data_cleaning")
source("setup.R")
source("cleaning_functions.R")
dir <- "raw_data/20190630"
kobo.xlsx.to.csv(dir, "(FINAL) Iraq MCNA Version VII", anonymise=T)

data <- read.csv(sprintf("%s/parent_cleaned.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data <- data %>% mutate(population_group = ifelse(calc_idp == 1, "idp", ifelse(calc_returnee == 1, "returnee", 
                                                                               ifelse(calc_host == 1, "host", NA)))) 

# filter on date
submission_date_to_check <- c("2019-06-28")
filtered_data <- data %>% filter(date_assessment == submission_date_to_check) 
sprintf("fraction of data after date %s: %f%%", submission_date_to_check, nrow(filtered_data) / nrow(data) * 100)

# clusters
result <- points.inside.cluster(data = filtered_data, samplepoints, sample_areas, dir, write_to_file = F)
print(result)

row <- 2
filtered_data %>% filter(cluster_location_id == result[row,1] & population_group == result[row,3]) %>% dplyr::select(enumerator_num)

# check separate cluster
clusters <- result$cluster
result <- points.inside.cluster.separated(data = filtered_data, samplepoints = samplepoints, 
                                          sample_areas = sample_areas, dir = dir, cluster = "cluster_location_id_0364",
                                          pop_group = "idp", alternative_clusters = clusters)

row <- 6
filtered_data %>% filter(X_uuid == result[row,1]) %>% dplyr::select(ngo,enumerator_num)

# add to cleaning log
uuid <- filtered_data %>% filter(cluster_location_id == result[3,1] & population_group == result[3,3]) %>% dplyr::select(X_uuid)
log <- log.cleaning.change(uuid = uuid$X_uuid, old.value = result[3,1],
                           new.value = result$alternative_cluster[3], question.name = "cluster_location_id",
                           issue = "wrong cluster selected, the right one is checked with the gps points", 
                           dir = dir)
execute.cleaning.changes(dir)