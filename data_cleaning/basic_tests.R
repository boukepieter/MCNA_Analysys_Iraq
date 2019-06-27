#devtools::install_github("boukepieter/cleaninginspectoR", build_opts=c())
library(cleaninginspectoR)
library(plotKML)
library(rgeos)
library(xlsx)
library(raster)
library(sf)
Sys.setlocale("LC_ALL","Arabic")

setwd("data_cleaning")
source("cleaning_functions.R")
WGS84 <- crs("+init=epsg:4326")
dir <- "raw_data/20190627"
kobo.xlsx.to.csv(dir, "(FINAL) Iraq MCNA Version VII", anonymise=F)

data <- read.csv(sprintf("%s/parent.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
child <- read.csv(sprintf("%s/child.csv",dir), stringsAsFactors = F)

#unique uuid's
find_duplicates_uuid(data)

inspect_all(data, uuid.column.name = "X_uuid")

#interview speed
times <- as.POSIXct(data$end,format="%Y-%m-%dT%H:%M:%OS") - as.POSIXct(data$start,format="%Y-%m-%dT%H:%M:%OS")
median(times)

#shortest path (# of NA's)
NAs <- apply(data,1,FUN=function(x){length(which(is.na(x)))})
overview_times <- data.frame(enumerator=data$enumerator_num, time=times, family_size=data$num_family_member,
                             NAs=NAs)
write.csv(overview_times, sprintf("%s/overview.csv",dir), row.names = F)

#pop group
data %>% mutate(total_pop_cat=sum(calc_idp,calc_returnee,calc_host,na.rm=T)) %>% 
  dplyr::select(total_pop_cat) ## ISSUE, TO SOLVE
data <- data %>% mutate(population_group = ifelse(calc_idp == 1, "idp", ifelse(calc_returnee == 1, "returnee", 
                                                                     ifelse(calc_host == 1, "host", NA)))) 
#date check
submission_date_to_check <- c("2019-06-27")
filtered_data <- data %>% filter(date_assessment == submission_date_to_check) 
sprintf("fraction of data on date %s: %f%%", submission_date_to_check, nrow(filtered_data) / nrow(data) * 100)

#right locations
points.inside.cluster(data = data, samplepoints_file = "sample_points/samplepoints.RData",
                      sampleareas_file = "sample_points/sample_areas.RData")

log <- log.cleaning.change(uuid = data_on_cluster$X_uuid, old.value = data_on_cluster$cluster_location_id,
                          new.value = clusters_at_date[2], question.name = "cluster_location_id",
                          issue = "wrong cluster selected, the right one is checked with the gps points", 
                          dir = dir)
execute.cleaning.changes(dir)

kml_open(file.name=sprintf("maps/%s",clusters_at_date[cluster_no_to_check]),
         folder.name=clusters_at_date[cluster_no_to_check], kml_open=F)
plotKML(cluster_area)
plotKML(cluster_points)
plotKML(interview_locations_geo, colour="cluster")



#TODO(visualize cluster_area + cluster_points + interview_locations)

#outliers check in integer values
data$birth_cert_missing_amount_a1 + data$birth_cert_missing_amount_u1 > data$num_hh_member
data$id_card_missing_amount > data$num_hh_member
data$num_family_member > data$num_hh_member
data$num_hh_member > 25
data$refill_times > 10
data$people_share_tank > 20

data$num_family_member
apply(data$X_uuid,c(1,2),FUN=function(x){child$X_uuid})
