# setup

library(plyr) # rbind.fill
library(dplyr)
library(koboquest) # manage kobo questionnairs
library(kobostandards) # check inputs for inconsistencies
library(xlsformfill) # generate fake data for kobo
library(surveyweights) # calculate weights from samplingframes
library(hypegrammaR) # simple stats 4 complex samples
library(composr) # horziontal operations
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

# for speedy speed we can not recalculate weights on every run):
# weight_fun<-function(df){
#   df$weights
# }


response_with_composites <- recoding_O(response, loop)
table(response_with_composites[, c("disabled_hhh")], useNA="always")
which(response_with_composites$district == "al.hatra")

analysisplan <- read.csv("input/dap_adanal.csv", stringsAsFactors = F)
analysisplan <- analysisplan[-which(analysisplan$ignore),]
result <- from_analysisplan_map_to_output(response_with_composites, analysisplan = analysisplan,
                                          weighting = weight_fun,
                                          cluster_variable_name = "cluster_id",
                                          questionnaire = questionnaire, confidence_level = 0.9)


saveRDS(result,paste("result_adanal.RDS"))
#summary[which(summary$dependent.var == "g51a"),]

source("postprocessing_functions.R")
summary <- bind_rows(lapply(result[[1]], function(x){x$summary.statistic}))
write.csv(summary, "output/raw_results_adanal.csv", row.names=F)
summary <- read.csv("output/raw_results_adanal.csv", stringsAsFactors = F)
summary <- correct.zeroes(summary)
summary <- summary %>% filter(dependent.var.value %in% c(NA,1))
write.csv(summary, "output/raw_results_adanal_filtered.csv", row.names=F)
if(all(is.na(summary$independent.var.value))){summary$independent.var.value <- "all"}
groups <- unique(summary$independent.var.value)
groups <- groups[!is.na(groups)]
for (i in 1:length(groups)) {
  df <- pretty.output(summary, groups[i], analysisplan, cluster_lookup_table, lookup_table)
  write.csv(df, sprintf("output/summary_sorted_adanal_%s.csv", groups[i]), row.names = F)
}

browseURL("output")

### NA's
summary_statistics_get_na_messages<-function(results){
  x<-results$results
  lapply(x,function(x){
    
    attributes(x$summary.statistic)$hg_summary_statistic_fail_message
    x}) 
}
summary_statistics_get_na_messages(result)
attributes(result[[1]][[50]]$summary.statistic)

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
