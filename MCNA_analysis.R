# setup

library(xlsx)
library(plyr) # rbind.fill
library(dplyr)
library(koboquest) # manage kobo questionnairs
library(kobostandards) # check inputs for inconsistencies
library(xlsformfill) # generate fake data for kobo
library(surveyweights) # calculate weights from samplingframes
library(hypegrammaR) # simple stats 4 complex samples
library(composr) # horziontal operations
source("postprocessing_functions.R")
source("functions/to_alphanumeric_lowercase.R")
source("functions/analysisplan_factory.R")
source("functions/recoding.R")


#' load input files & make everything match:
source("load_inputs.R",local = T)
#' creates objects:
#' 
#'    response
#'    analysisplan
#'    choices
#'    questions
#'    cluster_lookup_table
#'    idp_in_camp
#'    loop
#'    loop_in_camp
#'    samplingframe
#'    samplingframe_in_camp

source("match_inputs.R", local = T)
#' matching all inputs:
#' 1. combine in and out of camp data for each, HH and loops 
#' 2. put together questionnaire
#' 3. prepare sampling frames:
#'     3.1 prepare columns in out of camp cluster level sampling frame
#'     3.2 aggregate out-of-camp to stratum level
#'     3.3.make strata id for in-camp sampling frame
#'     3.4.combine the stratum sampling frames
#'     3.5.add strata ids to the dataset
#'     3.6. throw error if any don't match


# any further problems with the sampling frame matching?

strata_samplingframe_issues <- as.data.frame(response[which(!response$strata %in% samplingframe_strata$stratum), c("X_uuid", "strata")])
if(nrow(strata_samplingframe_issues)!=0){
  print(strata_samplingframe_issues)
  warning("something's not right with the strata id matching!")
}

cluster_samplingframe_issues <- as.data.frame(response[which(!response$cluster_id[which(response$population_group != "idp_in_camp")] %in% samplingframe$cluster_strata_ID), c("X_uuid", "strata")])
if(nrow(cluster_samplingframe_issues)!=0){
  print(cluster_samplingframe_issues)
  warning("something's not right with the cluster id matching!")
}



### IGNORING CLUSTER LEVEL WEIGHTING FOR NOW
#### it's been under debate..



# remove records not in cluster samplingframe:

# nrow_before<- nrow(response)
# response<-response %>% filter((cluster_id %in% samplingframe$cluster_strata_ID) | population_group=="idp_in_camp")

# if any disappeared, give a warning:
# if(nrow(response)!=nrow_before){
#   warning(paste("lost ",nrow_before-nrow(response), " records; their cluster id is not in the cluster sampling frame"))
# }

# clusters_weight_fun <- map_to_weighting(sampling.frame= samplingframe,
#                                         sampling.frame.population.column = "pop",
#                                         sampling.frame.stratum.column = "cluster_strata_ID",
#                                         data.stratum.column = "cluster_id",
#                                         data = response[response$population_group!="idp_in_camp",])


# only in camp idps have cluster weight of 1:


# cluster_weight_fun<-function(df){
#   weights<-rep(NA,nrow(df))
#   in_camp<-df$population_group=="idp_in_camp"
#   weights[!in_camp]<-clusters_weight_fun_out_of_camp(df[!in_camp,])
#   weights[in_camp]<-1
#   weights
#   }

strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe_strata,
                                      sampling.frame.population.column = "population",
                                      sampling.frame.stratum.column = "stratum",
                                      data.stratum.column = "strata",
                                      data = response)

# weight_fun <- combine_weighting_functions(strata_weight_fun, clusters_weight_fun)
weight_fun<-strata_weight_fun


response$weights<-weight_fun(response)

# new version of weights for SDC analysis
if (F){
  library(purrr)
  samplingframe_strata$samp_amount[match(names(samp_amount), samplingframe_strata$stratum)] <- response$strata %>% table
  samplingframe_strata$weights2 <- samplingframe_strata$population / samplingframe_strata$samp_amount
  samplingframe_strata[which(samplingframe_strata$weights2 < 2),]
  response <- response %>% mutate(weights2 = samplingframe_strata$weights2[match(strata, samplingframe_strata$stratum)]) 
  
  response %>% rowwise() %>% mutate(test = weights2 / weights) %>% select(test) %>% table
  samplingframe %>% filter(endsWith(samplingframe$stratum, "returnee")) %>% select(pop) %>% nrow
}

# for speedy speed we can not recalculate weights on every run):
# weight_fun<-function(df){
#   df$weights
# }


response_with_composites <- recoding_preliminary(response, loop)
#table(response_with_composites[, c("g51a")][which(response_with_composites$district == "erbil" & response_with_composites$population_group == "idp_out_camp")], useNA="always")
#which(response_with_composites$district == "al.hatra")

# Correcting for random sampled districts
simple_random_records <- response_with_composites$strata %in% simple_random_strata
response_with_composites$cluster_id[simple_random_records]<-
  paste("simple random unique cluster id - ",1:length(which(simple_random_records)))

dap_name <- "preliminary"
analysisplan <- read.csv(sprintf("input/dap_%s.csv",dap_name), stringsAsFactors = F)
#analysisplan <- analysisplan[-which(analysisplan$ignore),]
analysisplan <- analysisplan[which(startsWith(analysisplan$dependent.variable, "f_hhh") 
                                   #  | startsWith(analysisplan$dependent.variable, "s7") 
                                   #  | startsWith(analysisplan$dependent.variable, "s21") 
                                   #  | startsWith(analysisplan$dependent.variable, "s22")
),]
#analysisplan <- analysisplan_nationwide(analysisplan)
#analysisplan <- analysisplan_pop_group_aggregated(analysisplan)
result <- from_analysisplan_map_to_output(response_with_composites, analysisplan = analysisplan,
                                          weighting = weight_fun,
                                          cluster_variable_name = "cluster_id",
                                          questionnaire = questionnaire, confidence_level = 0.9)

name <- "preliminary_fhhh_nationwide_aggregated"
saveRDS(result,paste(sprintf("output/result_%s.RDS", name)))
#summary[which(summary$dependent.var == "g51a"),]

lookup_in_camp<-load_samplingframe("./input/sampling_frame_in_camp.csv")
names(lookup_in_camp)[which(names(lookup_in_camp) == "camp")] <- "name"
names(lookup_in_camp)[which(names(lookup_in_camp) == "camp.long.name")] <- "english"
names(lookup_in_camp)[which(names(lookup_in_camp) == "governorate")] <- "filter"

summary <- bind_rows(lapply(result[[1]], function(x){x$summary.statistic}))
write.csv(summary, sprintf("output/raw_results_%s.csv", name), row.names=F)
summary <- read.csv(sprintf("output/raw_results_%s.csv", name), stringsAsFactors = F)
summary <- correct.zeroes(summary)
summary <- summary %>% filter(dependent.var.value %in% c(NA,1))
write.csv(summary, sprintf("output/raw_results_%s_filtered.csv", name), row.names=F)
if(all(is.na(summary$independent.var.value))){summary$independent.var.value <- "all"}
groups <- unique(summary$independent.var.value)
groups <- groups[!is.na(groups)]
for (i in 1:length(groups)) {
  df <- pretty.output(summary, groups[i], analysisplan, cluster_lookup_table, lookup_table, severity = name == "severity", camp = F)
  write.csv(df, sprintf("output/summary_sorted_%s_%s.csv", name, groups[i]), row.names = F)
  if(i == 1){
    write.xlsx(df, file=sprintf("output/summary_sorted_%s.xlsx", name), sheetName=groups[i], row.names=FALSE)
  } else {
    write.xlsx(df, file=sprintf("output/summary_sorted_%s.xlsx", name), sheetName=groups[i], append=TRUE, row.names=FALSE)
  }
}

# formap <- df[-c(1:4),]
# formap$msni <- as.numeric(formap$msni)


# Extra step for pin calculation
for (i in 1:length(groups)) {
  group_pin <- severity_for_pin(sprintf("output/summary_sorted_%s_%s.csv", name, groups[i]), analysisplan = analysisplan)
  write.csv(group_pin, sprintf("output/pin_%s_%s.csv", name, groups[i]), row.names = F)
  if(i == 1){
    write.xlsx(group_pin, file=sprintf("output/pin_%s.xlsx", name), sheetName=groups[i], row.names=FALSE)
  } else {
    write.xlsx(group_pin, file=sprintf("output/pin_%s.xlsx", name), sheetName=groups[i], append=TRUE, row.names=FALSE)
  }
}



response_with_composites %>% filter(district=="al.baaj") %>% 
  select(names(response_with_composites)[which(startsWith(names(response_with_composites), "s8_"))])
### NA's
summary_statistics_get_na_messages<-function(results){
  x<-results$results
  lapply(x,function(x){
    
    attributes(x$summary.statistic)$hg_summary_statistic_fail_message
    x}) 
}
summary_statistics_get_na_messages(result)
attributes(result[[1]][[1]]$summary.statistic)

### Severity calculation
all <- read.csv("output/summary_sorted_all.csv", stringsAsFactors = F)
f_hhh <- read.csv("output/summary_sorted_female.csv", stringsAsFactors = F)
results <- list(all = all, f_hhh = f_hhh)
severity_table <- read.csv("input/severity_thresholds.csv", stringsAsFactors = F)
severity_table <- severity_table[-18,]
districts <- unique(c(all$district, f_hhh$district))[-1]
df <- data.frame(sector = rep(severity_table$sector, each=2), indicator_code = rep(severity_table$indicator_code, each=2), 
                 indicator = rep(severity_table$indicator, each=2),
                 stringsAsFactors = F)
for (j in 1:length(districts)) {
  df[,districts[j]] <- NA
  for (i in 1:nrow(severity_table)){
    if (severity_table$resultset[i] == "camp"){next}
    value <- as.numeric(results[[severity_table$resultset[i]]][results[[severity_table$resultset[i]]]$district == districts[j],severity_table$indicator_code[i]])
    df[(i-1) * 2 + 1, districts[j]] <- value
    if (severity_table$lower_limit[i] == 0) {
      df[(i-1) * 2 + 2, districts[j]] <- ifelse(value < severity_table$Minimal..1.[i], 1,
                                                ifelse(value < severity_table$Stress..2.[i], 2,
                                                       ifelse(value < severity_table$Severe..3.[i], 3,
                                                              ifelse(value < severity_table$Extreme..5.[i], 4, 5))))
    } else if (severity_table$lower_limit[i] == 1) {
      df[(i-1) * 2 + 2, districts[j]] <- ifelse(value >= severity_table$Minimal..1.[i], 1,
                                                ifelse(value >= severity_table$Stress..2.[i], 2,
                                                       ifelse(value >= severity_table$Severe..3.[i], 3,
                                                              ifelse(value >= severity_table$Extreme..5.[i], 4, 5))))
    }
  }
}
write.csv(df, "output/severity_output.csv", row.names = F)
