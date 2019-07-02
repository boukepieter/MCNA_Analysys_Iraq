setwd("data_cleaning")
source("setup.R")
source("cleaning_functions.R")
dir <- "raw_data/20190630"

data <- read.csv(sprintf("%s/parent_cleaned_anonymised.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data <- data %>% mutate(population_group = ifelse(calc_idp == 1, "idp", ifelse(calc_returnee == 1, "returnee", 
                                                                               ifelse(calc_host == 1, "host", NA)))) 
child <- read.csv(sprintf("%s/child.csv",dir), stringsAsFactors = F)

#unique uuid's
find_duplicates_uuid(data)

inspect_all(data, uuid.column.name = "X_uuid")

## interview speed
times <- as.POSIXct(data$end,format="%Y-%m-%dT%H:%M:%OS") - as.POSIXct(data$start,format="%Y-%m-%dT%H:%M:%OS")
median(times)

# shortest path (# of NA's)
NAs <- apply(data,1,FUN=function(x){length(which(is.na(x)))})
overview_times <- data.frame(enumerator=data$enumerator_num, time=times, family_size=data$num_family_member,
                             NAs=NAs, ngo=data$ngo)
mean_per_enum <- overview_times %>% group_by(enumerator) %>% summarize(mean(time))
write.csv(overview_times, sprintf("%s/overview.csv",dir), row.names = F)


## translating other...
data <- read.csv(sprintf("%s/parent_cleaned_anonymised.csv", dir), stringsAsFactors = F, encoding = "UTF-8")
result <- translate.others.arabic(data, ignore.cols = c("inc_other", "restriction_other"))
write.csv(result, sprintf("%s/translations.csv", dir), row.names = F, fileEncoding = "UTF-8")

## household individual data
loop <- read.csv(sprintf("%s/child.csv", dir), stringsAsFactors = F, encoding = "UTF-8")
loop_without_error <- loop %>% filter(relationship != "error")

#	Check whether the household size number given and the individual roster fits, 
# and if there is discrepancy between the household survey and individual loop. 
result <- general.checks(data, loop_without_error)
summary <- summarize.result(result)
summary
round(summary / nrow(data) * 100)

table(data$ngo)
summary.of.partner(data, loop_without_error, "mcna01")

og <- log.cleaning.change(uuid = result$uuid[which(result$no_single_spouse == FALSE)], 
                          action = "c", old.value = "cluster_location_id_0553",
                          new.value = result$alternative_cluster[6], question.name = "cluster_location_id",
                          issue = "wrong cluster selected, the right one is checked with the gps points", 
                          dir = dir)
execute.cleaning.changes(dir)

loop %>% filter(X_submission__uuid %in% result$uuid[which(result$no_single_spouse == FALSE)] & relationship == "spouse") %>% 
  dplyr::select(marital_status)
