setwd("data_cleaning")
source("setup.R")
source("cleaning_functions.R")
set.up.cleaning.dir("raw_data")
dir <- "raw_data/20190819"
#kobo.xlsx.to.csv(dir, "(FINAL) Iraq MCNA Version VII", anonymise=F)

data <- read.csv(sprintf("%s/parent.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data <- data %>% mutate(population_group = ifelse(calc_idp == 1, "idp", ifelse(calc_returnee == 1, "returnee", 
                                                                               ifelse(calc_host == 1, "host", NA))))
#ignore_date <- c("2019-08-07")
#data <- data %>% filter(as.Date(date_assessment) < as.Date(ignore_date)) 

## Overview of dates
dates <- read.csv(sprintf("%s/surveys_cleaned_old.csv", dir), stringsAsFactors = F)
dates2 <- data %>% dplyr::select(date_assessment) %>% table
dates2 - c(dates$Freq, rep(0,length(dates2) - nrow(dates)))
write.csv(dates2, sprintf("%s/surveys_cleaned.csv", dir), row.names = F)

## Overview of partners
execute.cleaning.changes(dir, filename = "parent.csv")
data <- read.csv(sprintf("%s/parent_cleaned.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data <- data %>% mutate(population_group = ifelse(calc_idp == 1, "idp", ifelse(calc_returnee == 1, "returnee", 
                                                                               ifelse(calc_host == 1, "host", NA))))
#data <- data %>% filter(as.Date(date_assessment) < as.Date(ignore_date)) 

overview <- data %>% dplyr::select(governorate_mcna, ngo) %>% table()
colnames(overview) <- partners$V2[match(colnames(overview), partners$V1)]
overview

col <- 17
row <- 9
name <- colnames(data %>% dplyr::select(governorate_mcna, ngo) %>% table())[col]
uuid <- data %>% filter(ngo == name) %>% 
  dplyr::select(X_uuid, cluster_location_id)
log <- log.cleaning.change.extended(data, partners, psu, uuid$X_uuid, action = "c",  
                                    question.name="ngo", new.value="mcna20", issue="ZOA selected ngo is other and then 8, but in this place there is only ZOA doing data collection so it should be them.",
                                    dir)
## check on referral
data %>% filter(ref_docs == "yes") %>% dplyr::select(X_uuid, date_assessment, referral_contact, ngo)

## start geo cleaning
# filter on date
check <- read.csv(sprintf("%s/cleaning_logbook.csv", dir), stringsAsFactors = F)
submission_date_to_check <- c("2019-08-20")
filtered_data <- data %>% filter(date_assessment == submission_date_to_check) 
sprintf("fraction of data after date %s: %f%%", submission_date_to_check, nrow(filtered_data) / nrow(data) * 100)

# clusters
result <- points.inside.cluster(data = filtered_data, samplepoints, sample_areas, dir, write_to_file = F)
print(result[,-ncol(result)])

check_if_cleaned(result, check, psu, partners)
which(result$inside_alternative_cluster)
row <- 2
uuid <- filtered_data %>% filter(cluster_location_id == result[row,1] & population_group == result[row,3]) %>% 
  dplyr::select(X_uuid)
uuid$X_uuid %in% check$uuid
result$cluster[row]

# add to cleaning log
log <- log.cleaning.change.extended(data, partners, psu, uuid$X_uuid, action = "f",  
                                    question.name="calc_host", issue="Host interviewed in district without host sample.",
                                    dir = dir)
log <- log.cleaning.change.extended(data, partners, psu, uuid$X_uuid, action = "f",  
                                    question.name="cluster_location_id", issue="Returnee interviewed in IDP location, keeping for potential later use 581 Returnee cluster closest by.",
                                    dir = dir)
log <- log.cleaning.change.extended(data, partners, psu, uuid$X_uuid, action = "c",  
                                    question.name="cluster_location_id", issue="Wrong cluster selected, right cluster checked with gps coordinates and other locations assessed that day.",
                                    dir = dir, new.value = result$alternative_cluster[row])#"cluster_location_id_0198")###
log <- log.cleaning.change.extended(data, partners, psu, uuid$X_uuid, action = "c",  
                                    question.name="cluster_location_id", issue="Wrong cluster selected, right cluster checked with gps coordinates.",
                                    dir = dir, new.value = "cluster_location_id_0060")###


log <- log.cleaning.change.extended(data, partners, psu, uuid$X_uuid, action = "f",  
                                    question.name="cluster_location_id", issue="Interview gps coordinates on the big road, seems to be on the way back. Closer to other clusters but that might be because of the 'delay'.",
                                    dir = dir)
# check separate cluster
clusters <- result$cluster
result_separate <- points.inside.cluster.separated(data = filtered_data, samplepoints = samplepoints, 
                                                   sample_areas = sample_areas, dir = dir, cluster = result$cluster[row],
                                                   pop_group = result$pop_group[row], alternative_clusters = clusters, 
                                                   buffer=1000)
print(result_separate)
which(!result_separate$inside_buffer) %in% which(uuid$X_uuid %in% check$uuid)

row <- 1
row <- c(3:6)
row <- which(!result_separate$inside_buffer)
filtered_data %>% filter(X_uuid == result_separate[row,1]) %>% dplyr::select(ngo,enumerator_num)
filtered_data %>% filter(X_uuid == result_separate[row,1]) %>% dplyr::select(X_gpslocation_latitude, X_gpslocation_longitude)

# add to cleaning log
log <- log.cleaning.change.extended(data, partners, psu, result_separate$uuid[row], action = "f",  
                                    question.name="calc_host", issue="Host interviewed in district without host sample.",
                                    dir = dir)
log <- log.cleaning.change.extended(data, partners, psu, result_separate$uuid[row], action = "f",  
                                    question.name="cluster_location_id", issue="Returnee interviewed in IDP location in district without Returnee sample, keeping for potential later use.",
                                    dir = dir)
log <- log.cleaning.change.extended(data, partners, psu, result_separate$uuid[row], action = "c",  
                                    question.name="cluster_location_id", issue="Wrong cluster selected, checked with gps points and other clusters interviewed that day.",
                                    dir = dir, new.value = result_separate$alternative_cluster[row])
log <- log.cleaning.change.extended(data, partners, psu, result_separate$uuid[row], action = "c",  
                                    question.name="cluster_location_id", issue="Wrong cluster selected, checked with gps points and other clusters interviewed that day.",
                                    dir = dir, new.value = "cluster_location_id_0595")#

log <- log.cleaning.change.extended(data, partners, psu, result_separate$uuid[row], action = "f",  
                                    question.name="cluster_location_id", issue="Interview held just outside of cluster location buffer. But many interviews in same location.",
                                    dir = dir)#, new.value = "cluster_location_id_0621")#result_separate$alternative_cluster[row])#
execute.cleaning.changes(dir, "parent")
anonymise.cleaned.data(dir, name = "parent")

### surveys done
data_old <- read.csv(sprintf("%s/parent_cleaned_old.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data_all <- read.csv(sprintf("%s/parent_cleaned.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data_all <- data_all %>% mutate(population_group = ifelse(calc_idp == 1, "idp", ifelse(calc_returnee == 1, "returnee", 
                                                                                       ifelse(calc_host == 1, "host", NA)))) 
data_new <- data_all %>% filter(!X_uuid %in% data_old$X_uuid)# & as.Date(date_assessment) < as.Date(ignore_date))

surveys_done <- data_new %>% dplyr::select(cluster_location_id, population_group) %>%  table
new_survey_locations <- cbind(as.data.frame.matrix(surveys_done),
                              psu[match(rownames(surveys_done), psu$new_ID),c("district", "name")])
total_surveys_done <- data_all %>%  dplyr::select(cluster_location_id, population_group) %>%  table
cbind(new_survey_locations, 
      as.data.frame.matrix(total_surveys_done)[match(rownames(surveys_done), rownames(total_surveys_done)),])

## map of finished
log <- read.csv(sprintf("%s/cleaning_logbook.csv",dir), stringsAsFactors = F)
data1 <- read.csv(sprintf("%s/parent_cleaned_anonymised.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data2 <- read.csv(sprintf("%s/parent2_cleaned_anonymised.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data2$X_validation_status <- NA
data <- rbind(data1[,-which(!names(data1) %in% names(data2))],data2)
data <- rbind(data1,data2)
data <- data %>% mutate(population_group = ifelse(calc_idp == 1, "idp", ifelse(calc_returnee == 1, "returnee", 
                                                                               ifelse(calc_host == 1, "host", NA))))
cluster_lookup_table <- read.csv("../input/combined_sample_ids.csv", 
                                 stringsAsFactors=F, check.names=F)
data <- data %>% 
  mutate(district = cluster_lookup_table$district[match(cluster_location_id,cluster_lookup_table$new_ID)])

write.csv(data, sprintf("%s/parent_merged.csv", dir), row.names = F, fileEncoding = "UTF-8")

strata <- readOGR("c:/Users/REACH-IRQ-GIS/Documents/2019 MCNA/201904 MCNA sampling script/MCNA_sampling/input/irq_admbnda_adm2_cso_20190603.shp",
                  "irq_admbnda_adm2_cso_20190603", stringsAsFactors = F)
govs <- readOGR("c:/Users/REACH-IRQ-GIS/Documents/2019 MCNA/201904 MCNA sampling script/MCNA_sampling/input/irq_admbnda_adm1_cso_20190603.shp",
                "irq_admbnda_adm1_cso_20190603", stringsAsFactors = F)
loc_overview <- read.csv("raw_data/locations_overview.csv", stringsAsFactors = F)
map.finished.districts(data, log, loc_overview, strata, govs, dir)

# diff in partners / govs
overview <- data_new %>% dplyr::select(governorate_mcna, ngo) %>% table()
colnames(overview) <- partners$V2[match(colnames(overview), partners$V1)]
overview

data_all %>% dplyr::filter(endsWith(cluster_location_id, "0215")) %>% dplyr::select(ngo, date_assessment) %>% table

data %>% dplyr::filter(X_uuid %in% result_separate$uuid) %>% dplyr::select(cluster_location_id, population_group, date_assessment)

psu <- read.csv("combined_sample_ids.csv", stringsAsFactors = F)
data$district <- psu$district[match(data$cluster_location_id, psu$new_ID)]
table <- data %>% dplyr::select(district, population_group) %>% table
write.csv(as.data.frame.matrix(table), "surveys_in_district.csv")


data %>% filter(endsWith(cluster_location_id, "0639")) %>% 
  dplyr::select(cluster_location_id, ngo, population_group)
tab <- data %>% filter(! ngo %in% c("mcna02", "mcna16")) %>% 
  group_by(date_assessment) %>% summarize(n_distinct(ngo), n())
mean((tab[,3] / tab[,2])$'n()')

# feedback for flagged cleaning
log <- read.csv(sprintf("%s/cleaning_logbook.csv",dir), stringsAsFactors = F)
data <- read.csv(sprintf("%s/parent_cleaned.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data2 <- read.csv(sprintf("%s/parent2_cleaned.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data2$X_validation_status <- NA
data <- rbind(data[,-which(!names(data) %in% names(data2))],data2)
data <- data %>% mutate(population_group = ifelse(calc_idp == 1, "idp", ifelse(calc_returnee == 1, "returnee", 
                                                                               ifelse(calc_host == 1, "host", NA))))
row <- c(6762)
uuid <- log[row,"uuid"]
coords <- data[which(data$X_uuid %in% uuid),c("X_gpslocation_longitude", "X_gpslocation_latitude")]
spdf <- SpatialPointsDataFrame(coords, data.frame(uuid=uuid), proj4string = WGS84)
writeOGR(spdf, dsn = "raw_data/shapes", layer = sprintf("%s_%s_%s",format(Sys.time(), format="%H%M"), log$location_id[row][1], 
                                                        data$population_group[which(data$X_uuid== uuid[1])]),
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

#TEMP
phones <- c("354402104522644",
            "354503090523163",
            "354601081223791",
            "354814100414297",
            "356022061520304",
            "356073092205423",
            "356379069781845",
            "359705055249792",
            "359926090741748",
            "860133045897152",
            "867115024512202")
shirqat <- filtered_data[which(filtered_data$deviceid %in% phones),]
uuid <- shirqat$X_uuid
log <- log.cleaning.change.extended(data, partners, psu, uuid, action = "d",  
                                    question.name="cluster_location_id", issue="Test interviews, confirmed with area coordinator",
                                    dir = dir)#, new.value = "cluster_location_id_0621")#result_separate$alternative_cluster[row])#
