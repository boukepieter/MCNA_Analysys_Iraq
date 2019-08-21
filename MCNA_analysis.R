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




response_with_composites <- recoding_mcna(response, loop)
table(response_with_composites[which(response_with_composites$district == "erbil"), c("population_group", "g54")])

result <- from_analysisplan_map_to_output(response_with_composites, analysisplan = analysisplan,
                                          weighting = weight_fun,
                                          cluster_variable_name = "cluster_id",
                                          questionnaire = questionnaire, confidence_level = 0.9)


# saveRDS(result,paste("result.RDS"))


summary <- bind_rows(lapply(result[[1]], function(x){x$summary.statistic}))
write.csv(summary, "output/raw_results.csv")
summary <- summary %>% filter(dependent.var.value %in% c(NA,1))
summary$moe <- summary$max - summary$min
summary$research.question <- analysisplan$research.question[match(summary$dependent.var, analysisplan$dependent.variable)]
summary$research.question <- analysisplan$research.question[match(summary$dependent.var, analysisplan$dependent.variable)]
write.csv(summary[,c("repeat.var.value", "dependent.var", "dependent.var.value","research.question", "independent.var.value", "numbers", "min", "max", "moe")],
          "output/resultsssss.csv", row.names = F)

lookup_table <- read.csv("input/lookup_table_names.csv", stringsAsFactors = F)
group <- "host"
df <- pretty.output(summary, group, analysisplan, cluster_lookup_table, lookup_table)
write.csv(df, sprintf("output/summary_sorted_%s.csv", group), row.names = F)


browseURL("output")
