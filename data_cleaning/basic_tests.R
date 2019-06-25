#devtools::install_github("boukepieter/cleaninginspectoR", build_opts=c())
library(cleaninginspectoR)

library(xlsx)
source("functions.R")
dir <- "raw_data/20190620_test"
kobo.xlsx.to.csv(dir, anonymise=F)

data <- read.csv(sprintf("%s/parent.csv",dir), stringsAsFactors = F)
child <- read.csv(sprintf("%s/child.csv",dir), stringsAsFactors = F)

#unique uuid's
find_duplicates_uuid(data)

#interview speed
as.POSIXct(data$end,format="%Y-%m-%dT%H:%M:%OS") - as.POSIXct(data$start,format="%Y-%m-%dT%H:%M:%OS")

#shortest path (# of NA's)
apply(data,1,FUN=function(x){length(which(is.na(x)))})

#pop group
data %>% mutate(total_pop_cat=sum(calc_idp,calc_returnee,calc_host,na.rm=T)) %>% 
  select(total_pop_cat) ## ISSUE, TO SOLVE
data <- data %>% mutate(population_group = ifelse(calc_idp == 1, "idp", ifelse(calc_returnee == 1, "returnee", 
                                                                     ifelse(calc_host == 1, "host", NA)))) 

#right locations
library(sf)
load("sample_points/sample_points_idp.RData")
load("sample_points/sample_points_r.RData")
load("sample_points/sample_points_host.RData")
samplepoints <- list(host = sample_points_host, idp = sample_points_idp, r = sample_points_r)

submission_date_to_check <- c("2019-06-13")
filtered_data <- data %>% filter(date_assessment == submission_date_to_check) 
clusters_at_date <- filtered_data %>% select(cluster_location_id) %>% table %>% names
pop_groups_at_date <- filtered_data %>% select(population_group) %>% table %>% names

cluster_no_to_check <- 1
interview_locations <- filtered_data %>% 
  filter(cluster_location_id == clusters_at_date[cluster_no_to_check]) %>% 
  select(c(X_gpslocation_longitude,X_gpslocation_latitude))
cluster <- strsplit(names(clusters_at_date)[cluster_no_to_check],"_")[[1]][4]
cluster_points <- sample_points_idp %>% as("sf") %>% filter(startsWith(label, cluster))


#outliers check in integer values
data$protection.birth_cert_missing_amount > data$household_roster.num_hh_member
data$protection.id_card_missing_amount > data$household_roster.num_hh_member
data$household_roster.num_family_member > data$household_roster.num_hh_member
data$household_roster.num_hh_member > 25
data$wash.refill_times > 10
data$wash.people_share_tank > 20

data$household_roster.num_family_member
apply(data$X_uuid,c(1,2),FUN=function(x){child$X_uuid})