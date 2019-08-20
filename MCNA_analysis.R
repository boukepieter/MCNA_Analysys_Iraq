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
r <- recoding_mcna(response_w_clusterids, loop)
ls() %>% paste0(collapse="\n") %>% cat


# this line is dangerous. If we end up with missing strata, they're silently removed.
# could we instead kick out more specifically the impossible district/population group combos?

if(any(!(r$strata %in% samplingframe_strata$stratum))){
  print(unique(r$strata[!(r$strata %in% samplingframe_strata$stratum)]))
  stop("STRATA MISSING FROM SAMPLING FRAME")
}
  
r <- r %>% filter(strata %in% samplingframe_strata$stratum)
r$cluster_id <- paste(r$cluster_location_id, r$population_group, sep = "_")

r$cluster_id %>% table %>% as.data.frame %>% filter(Freq)
r$cluster_id %>% table %>% as.data.frame %>% filter(Freq==1)

if(any(!(r$cluster_id %in% samplingframe$cluster_strata_ID))){
  print(unique(r$cluster_id[!(r$cluster_id %in% samplingframe$cluster_strata_ID)]))
  stop("STRATA MISSING FROM SAMPLING FRAME")
}
r <- r %>% filter(cluster_id %in% samplingframe$cluster_strata_ID)



clusters_weight_fun <- map_to_weighting(sampling.frame
                                        = samplingframe,
                                                        sampling.frame.population.column = "pop",
                                                        sampling.frame.stratum.column = "cluster_strata_ID",
                                                        data.stratum.column = "cluster_id",
                                                        data = r)



strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe_strata,
                 sampling.frame.population.column = "population",
                 sampling.frame.stratum.column = "stratum",
                 data.stratum.column = "strata",
                 data = r)

weight_fun <- combine_weighting_functions(strata_weight_fun, clusters_weight_fun)

r$weights<-weight_fun(r)

result <- from_analysisplan_map_to_output(r, analysisplan = analysisplan,
                                          weighting = weight_fun,
                                          cluster_variable_name = "cluster_id",
                                          questionnaire = questionnaire, confidence_level = 0.9)


# saveRDS(result,paste("result.RDS"))


summary <- bind_rows(lapply(result[[1]], function(x){x$summary.statistic}))
# summary <- summary %>% filter(dependent.var.value %in% c(NA,1))
summary$moe <- summary$max - summary$min
summary$research.question <- analysisplan$research.question[match(summary$dependent.var, analysisplan$dependent.variable)]
write.csv(summary[,c("repeat.var.value", "dependent.var", "dependent.var.value","research.question", "independent.var.value", "numbers", "min", "max", "moe")],
          "output/result_0_90_g54.csv", row.names = F)
browseURL("output")
# -----------------------------------------------------------------------------------------------------------------------------------------------
# For Martin - unitil here, in the summary are only NA's in dev version in master G68 - G63 have numbers (others are not recoded yet in the data)

kobostandards::check_input(data = r, analysisplan = analysisplan) %>% filter(grepl("analysisplan",affected_files))

result_labeled <- result$results %>% lapply(map_to_labeled,questionnaire)
result_labeled <- result
result_labeled$results <- result$results %>% lapply(map_to_labeled,questionnaire)

# exporting only small part of results for speed during testing:
subset_of_results<- rep(FALSE,length(result$results))
subset_of_results[500:700]<-TRUE
some_results<-hypegrammaR:::results_subset(result,logical = subset_of_results)

# not sure if this function should be "user facing" or have some wrappers (@Bouke thoughts?)
# essentially it handles all the looping over different column values as hierarchies.
# then each result is visualised by a function passed here that decides how to render each individual result
# see ?hypegrammaR:::map_to_generic_hierarchical_html

result_labeled$analysisplan$dependent.var

hypegrammaR:::map_to_generic_hierarchical_html(result_labeled,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir = "./output",
                                               filename = "summary_by_dependent_var_then_by_repeat_var.html"
                                               )
browseURL("summary_by_dependent_var_then_by_repeat_var.html")


# not sure this is working correctly.. next on agenda (:
# big_table <- hypegrammaR:::map_to_datamerge(results$results, questionnaire = questionnaire, rows = "repeat.var.value")

