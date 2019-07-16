setwd("data_cleaning")
source("setup.R")
source("cleaning_functions.R")
set.up.cleaning.dir("raw_data")
dir <- "raw_data/20190714"
kobo.xlsx.to.csv(dir, "(FINAL) Iraq MCNA Version VII", anonymise=F)

data <- read.csv(sprintf("%s/parent.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data <- data %>% mutate(population_group = ifelse(calc_idp == 1, "idp", ifelse(calc_returnee == 1, "returnee", 
                                                                               ifelse(calc_host == 1, "host", NA))))
ignore_date <- c("2019-07-19")
data <- data %>% filter(as.Date(date_assessment) < as.Date(ignore_date)) 

## Overview of dates
dates <- read.csv(sprintf("%s/surveys_cleaned_old.csv", dir), stringsAsFactors = F)
dates2 <- data %>% dplyr::select(date_assessment) %>% table
dates2 - c(dates$Freq, rep(0,length(dates2) - nrow(dates)))
write.csv(dates2, sprintf("%s/surveys_cleaned.csv", dir), row.names = F)

## Overview of partners
execute.cleaning.changes(dir)
data <- read.csv(sprintf("%s/parent_cleaned.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data <- data %>% mutate(population_group = ifelse(calc_idp == 1, "idp", ifelse(calc_returnee == 1, "returnee", 
                                                                               ifelse(calc_host == 1, "host", NA))))
data <- data %>% filter(as.Date(date_assessment) < as.Date(ignore_date)) 

overview <- data %>% dplyr::select(governorate_mcna, ngo) %>% table()
colnames(overview) <- partners$V2[match(colnames(overview), partners$V1)]
overview

col <- 9
row <- 3
name <- colnames(data %>% dplyr::select(governorate_mcna, ngo) %>% table())[col]
uuid <- data %>% filter(ngo == name) %>% 
  dplyr::select(X_uuid, cluster_location_id)
log.cleaning.change.extended(data, partners, psu, uuid$X_uuid, action = "c",  
                                         question.name="ngo", new.value="mcna17", issue="The REACH Iraq team in Kirkuk selected twice the wrong NGO. They selected Save the Children instead of REACH Iraq. Other points at the same location had already REACH Iraq selected",
                                         dir)
## check on referral
data %>% filter(ref_docs == "yes") %>% dplyr::select(date_assessment, referral_contact, ngo)

## start geo cleaning
# filter on date
submission_date_to_check <- c("2019-07-13")
filtered_data <- data %>% filter(date_assessment == submission_date_to_check) 
sprintf("fraction of data after date %s: %f%%", submission_date_to_check, nrow(filtered_data) / nrow(data) * 100)

# clusters
result <- points.inside.cluster(data = filtered_data, samplepoints, sample_areas, dir, write_to_file = T)
print(result[,-ncol(result)])

check <- read.csv(sprintf("%s/cleaning_logbook.csv", dir), stringsAsFactors = F)
which(!result$inside_alternative_cluster)
row <- 1
uuid <- filtered_data %>% filter(cluster_location_id == result[row,1] & population_group == result[row,3]) %>% 
  dplyr::select(X_uuid)
uuid$X_uuid %in% check$uuid
psu$district[match(result$cluster[row], psu$new_ID)]
ngo <- filtered_data %>% filter(cluster_location_id == result[row,1] & population_group == result[row,3]) %>% 
  dplyr::select(ngo)
partners$V2[match(ngo$ngo, partners$V1)]
filtered_data %>% filter(cluster_location_id == result[row,1] & population_group == result[row,3]) %>% 
  dplyr::select(X_gpslocation_latitude, X_gpslocation_longitude)

# add to cleaning log
log <- log.cleaning.change.extended(data, partners, psu, uuid$X_uuid, action = "f",  
                             question.name="calc_host", issue="Host interviewed in district without host sample.",
                             dir = dir)
log <- log.cleaning.change.extended(data, partners, psu, uuid$X_uuid, action = "f",  
                                    question.name="calc_returnee", issue="IDP interviewed in Returnee location, keeping for potential later use.",
                                    dir = dir)
log <- log.cleaning.change.extended(data, partners, psu, uuid$X_uuid, action = "c",  
                                    question.name="cluster_location_id", issue="Wrong cluster selected, right cluster checked with gps coordinates and other locations assessed that day.",
                                    dir = dir, new.value = result$alternative_cluster[row])#"cluster_location_id_0198")###
log <- log.cleaning.change.extended(data, partners, psu, uuid$X_uuid, action = "c",  
                                    question.name="cluster_location_id", issue="Wrong cluster selected, right cluster checked with gps coordinates and other locations assessed that day.",
                                    dir = dir, new.value = "cluster_location_id_0671")###


log <- log.cleaning.change.extended(data, partners, psu, uuid$X_uuid, action = "f",  
                                    question.name="cluster_location_id", issue="All interviews done exactly at the same spot (12!), check with partner why.",
                                    dir = dir)
# check separate cluster
clusters <- result$cluster
result_separate <- points.inside.cluster.separated(data = filtered_data, samplepoints = samplepoints, 
                                          sample_areas = sample_areas, dir = dir, cluster = result$cluster[row],
                                          pop_group = result$pop_group[row], alternative_clusters = clusters, 
                                          buffer=1000)
print(result_separate)

row <- 2
row <- which(!result_separate$inside_buffer)
filtered_data %>% filter(X_uuid == result_separate[row,1]) %>% dplyr::select(ngo,enumerator_num)
filtered_data %>% filter(X_uuid == result_separate[row,1]) %>% dplyr::select(X_gpslocation_latitude, X_gpslocation_longitude)

# add to cleaning log
log <- log.cleaning.change.extended(data, partners, psu, result_separate$uuid[row], action = "c",  
                             question.name="cluster_location_id", issue="Wrong cluster selected, checked with gps points and other clusters interviewed that day.",
                             dir = dir, new.value = "cluster_location_id_0621")#result_separate$alternative_cluster[row])#
log <- log.cleaning.change.extended(data, partners, psu, result_separate$uuid[row], action = "f",  
                                    question.name="cluster_location_id", issue="Interview held outside (1 km) buffer of location boundary. Many interviews in this locations so normal that some are a bit further away.",
                                    dir = dir)#, new.value = "cluster_location_id_0621")#result_separate$alternative_cluster[row])#
execute.cleaning.changes(dir)
anonymise.cleaned.data(dir)

### surveys done
data_old <- read.csv(sprintf("%s/parent_cleaned_old.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data_all <- read.csv(sprintf("%s/parent_cleaned.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data_all <- data_all %>% mutate(population_group = ifelse(calc_idp == 1, "idp", ifelse(calc_returnee == 1, "returnee", 
                                                                               ifelse(calc_host == 1, "host", NA)))) 
data_new <- data_all %>% filter(!X_uuid %in% data_old$X_uuid & as.Date(date_assessment) < as.Date(ignore_date))

surveys_done <- data_new %>% dplyr::select(cluster_location_id, population_group) %>%  table
new_survey_locations <- cbind(as.data.frame.matrix(surveys_done),
                              psu[match(rownames(surveys_done), psu$new_ID),c("district", "name")])
total_surveys_done <- data_all %>%  dplyr::select(cluster_location_id, population_group) %>%  table
cbind(new_survey_locations, 
      as.data.frame.matrix(total_surveys_done)[match(rownames(surveys_done), rownames(total_surveys_done)),])

## map of finished
log <- read.csv(sprintf("%s/cleaning_logbook.csv",dir), stringsAsFactors = F)
data <- read.csv(sprintf("%s/parent_cleaned.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data <- data %>% mutate(population_group = ifelse(calc_idp == 1, "idp", ifelse(calc_returnee == 1, "returnee", 
                                                                               ifelse(calc_host == 1, "host", NA))))
table(log$question.name)
uuid <- log %>% dplyr::filter(question.name %in% c("calc_host", "calc_idp", "calc_returnee", "cluster_location_id", "host_calc") 
                              & action == "flag") %>% dplyr::select(uuid)
data_filtered <- data %>% filter(! X_uuid %in% uuid$uuid)
loc_overview <- read.csv("raw_data/locations_overview.csv", stringsAsFactors = F)
for (i in 1:nrow(loc_overview)) {
  n <- data_filtered %>% filter(endsWith(data_filtered$cluster_location_id, substr(loc_overview$label[i],1,4)) 
                                & population_group == tolower(loc_overview$pop_group[i]))
  loc_overview$surveys_done[i] <- nrow(n)
  loc_overview$complete[i] <- min(loc_overview$surveys_done[i] / loc_overview$survey_buffer[i], 1)
}
write.csv(loc_overview, "raw_data/locations_overview.csv", row.names = F)
summarized <- loc_overview %>% group_by(strata) %>% summarise(mean(complete))
strata <- readOGR("c:/Users/REACH-IRQ-GIS/Documents/2019 MCNA/201904 MCNA sampling script/MCNA_sampling/input/
                  irq_admbnda_adm2_cso_20190603.shp","irq_admbnda_adm2_cso_20190603")
strata$complete <- summarized$`mean(complete)`[match(as.character(strata$ADM2_EN), summarized$strata)]
library(leaflet)
library(htmlwidgets)
pal <- colorNumeric("RdYlBu", replace(strata$complete,is.infinite(strata$complete),0),reverse=F)
centers <- data.frame(gCentroid(strata, byid = TRUE))
centers$lbl <- strata$ADM2_EN
m <- leaflet(strata) %>% 
  addTiles() %>%
  setView(44, 36, zoom = 10) %>% 
  addPolygons(color="black",fillColor=~pal(strata$complete),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              fillOpacity=0.75,
              popupOptions=popupOptions(maxWidth=300,closeOnClick=T),
              popup = sprintf("%s %%",round(strata$complete * 100,1))) %>%
  addLabelOnlyMarkers(data = centers,
                      lng = ~x, lat = ~y, label = ~lbl,
                      labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE))#%>% 
  addLegend(position="topright",colors=c("red","blue"),
            labels=c("Camp location","Out of camp location"))

saveWidget(m,"map.html")

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


data %>% filter(endsWith(cluster_location_id, "0671")) %>% dplyr::select(ngo, population_group)
