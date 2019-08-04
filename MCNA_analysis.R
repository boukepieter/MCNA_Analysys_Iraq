# setup
library(dplyr)
library(koboquest) # manage kobo questionnairs
library(kobostandards) # check inputs for inconsistencies
library(xlsformfill) # generate fake data for kobo
library(hypegrammaR) # simple stats 4 complex samples
library(composr) # horziontal operations

source("functions/to_alphanumeric_lowercase.R")
source("functions/analysisplan_factory.R")
source("functions/recoding.R")
#source("./pre-process_strata_names.R")

# load questionnaire inputs
questions <- read.csv("input/kobo_questions.csv", 
                      stringsAsFactors=F, check.names=F)

choices <- read.csv("input/kobo_choices.csv", 
                    stringsAsFactors=F, check.names=F)


# generate data
# response <- xlsform_fill(questions,choices,1000)

# response_filtered <- response %>% 
#   filter(!is.na(type_hh))

response <- read.csv("input/parent_cleaned_anonymised.csv",
                     stringsAsFactors = F, check.names = F)
response <- response %>% mutate(type_hh = ifelse(calc_idp == 1, "idp", ifelse(calc_returnee == 1, "returnee", 
                                                                               ifelse(calc_host == 1, "host", NA))))
# add cluster ids

cluster_lookup_table <- read.csv("input/combined_sample_ids.csv", 
                         stringsAsFactors=F, check.names=F)

response_w_clusterids <- response %>% 
  mutate(district = cluster_lookup_table$district[match(cluster_location_id,cluster_lookup_table$new_ID)])
response_w_clusterids <- response_w_clusterids %>% 
  mutate(strata = paste0(district,type_hh))


# horizontal operations / recoding

r <- recoding_mcna(response_w_clusterids)

# r <- r %>% mutate(score_livelihoods = hh_with_debt_value+hh_unemployed+hh_unable_basic_needs)

# vertical operations / aggregation

### .. should/can this move up to dataset generation? 
# names(r)<-to_alphanumeric_lowercase(names(r))


# make analysisplan including all questions as dependent variable by HH type, repeated for each governorate:
questionnaire <- load_questionnaire(r,questions,choices)

# r1 <- response_w_clusterids[,-c(518:523)]
# analysisplan_old <- make_analysisplan_all_vars(r1,
#                                          questionnaire,
#                                          independent.variable = "type_hh"
#                                          #repeat.for.variable = "governorate_mcna"
#                                          )
analysisplan <- read.csv("input/dap.csv", stringsAsFactors = F)
### .. should/can this move up to loading inputs?

samplingframe <- load_samplingframe("./input_modified/Strata_clusters_population.csv")
samplingframe$cluster_ID <- cluster_lookup_table$new_ID[match(samplingframe$psu, cluster_lookup_table$psu)]
samplingframe$cluster_strata_ID <- paste(samplingframe$cluster_ID, samplingframe$popgroup, sep = "_")
samplingframe_strata <- samplingframe %>% 
  group_by(stratum) %>% 
  summarize(population = sum(pop))

samplingframe_strata<-as.data.frame(samplingframe_strata)

# this line is dangerous. If we end up with missing strata, they're silently removed.
# could we instead kick out more specifically the impossible district/population group combos?
# r_test <- r_test %>% filter(strata %in% samplingframe_strata$stratum)
r$cluster_id <- paste(r$cluster_location_id, r$type_hh, sep = "_")
# r_test <- r_test %>% filter(cluster_id %in% samplingframe$cluster_strata_ID)
r_test <- r %>% 
  filter(strata %in% c("al.mosulidp","al.mosulreturnee","al.mosulhost"))

library(surveyweights)
clusters_weight_fun <- map_to_weighting(sampling.frame = samplingframe,
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


result <- from_analysisplan_map_to_output(r_test, analysisplan = analysisplan,
                                          weighting = weight_fun,
                                          cluster_variable_name = "cluster_id",
                                          questionnaire)


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

