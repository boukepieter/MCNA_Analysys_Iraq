setwd("data_cleaning")
source("setup.R")
source("cleaning_functions.R")
dir <- "raw_data/20190702"
kobo.xlsx.to.csv(dir, "(FINAL) Iraq MCNA Version VII", anonymise=F)

data <- read.csv(sprintf("%s/parent.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data <- data %>% mutate(population_group = ifelse(calc_idp == 1, "idp", ifelse(calc_returnee == 1, "returnee", 
                                                                               ifelse(calc_host == 1, "host", NA)))) 
data %>% dplyr::select(ngo) %>% table()
# filter on date
submission_date_to_check <- c("2019-06-30")
filtered_data <- data %>% filter(date_assessment >= submission_date_to_check) 
sprintf("fraction of data after date %s: %f%%", submission_date_to_check, nrow(filtered_data) / nrow(data) * 100)

# clusters
result <- points.inside.cluster(data = filtered_data, samplepoints, sample_areas, dir, write_to_file = T)
print(result)

row <- 8
filtered_data %>% filter(cluster_location_id == result[row,1] & population_group == result[row,3]) %>% dplyr::select(enumerator_num)

# add to cleaning log
uuid <- filtered_data %>% filter(cluster_location_id == result[row,1] & population_group == result[row,3]) %>% dplyr::select(X_uuid)
log <- log.cleaning.change(uuid = uuid$X_uuid, action = "f", question.name = "calc_host",
                           #old.value = result$cluster[row], new.value = result$alternative_cluster[row], 
                           issue = "Host is interviewed in location that is not in the sample, keeping as reserve for potential use if there are not enough surveys elsewhere in this district (Adhamia).", 
                           dir = dir)

# check separate cluster
clusters <- result$cluster
result_separate <- points.inside.cluster.separated(data = filtered_data, samplepoints = samplepoints, 
                                          sample_areas = sample_areas, dir = dir, cluster = "cluster_location_id_0597",
                                          pop_group = "idp", alternative_clusters = clusters)
print(result_separate)

row <- 1
filtered_data %>% filter(X_uuid == result_separate[row,1]) %>% dplyr::select(ngo,enumerator_num)

# add to cleaning log
log <- log.cleaning.change(uuid = result$uuid[row], action = "c", old.value = "cluster_location_id_0553",
                           new.value = result$alternative_cluster[6], question.name = "cluster_location_id",
                           issue = "wrong cluster selected, the right one is checked with the gps points", 
                           dir = dir)
execute.cleaning.changes(dir)
anonymise.cleaned.data(dir)

data %>% dplyr::select(cluster_location_id, population_group, date_assessment) %>%  table
data %>% filter(endsWith(cluster_location_id, "0708")) %>% dplyr::select(ngo) %>% table

data %>% dplyr::filter(X_uuid %in% result_separate$uuid) %>% dplyr::select(cluster_location_id, population_group, date_assessment)
