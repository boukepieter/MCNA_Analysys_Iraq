# setup

library(dplyr)
library(koboquest) # manage kobo questionnairs
library(kobostandards) # check inputs for inconsistencies
library(xlsformfill) # generate fake data for kobo
library(hypegrammaR) # simple stats 4 complex samples
library(composr) # horziontal operations

source("functions/to_alphanumeric_lowercase.R")
source("functions/analysisplan_factory.R")
#source("./pre-process_strata_names.R")

# load questionnaire inputs
questions <- read.csv("input/kobo_questions.csv", 
                      stringsAsFactors=F, check.names=F)

choices <- read.csv("input/kobo_choices.csv", 
                    stringsAsFactors=F, check.names=F)


# generate data
response <- xlsform_fill(questions,choices,1000)

response_filtered <- response %>% 
  filter(!is.na(type_hh))

# add cluster ids

cluster_lookup_table <- read.csv("input/combined_sample_ids.csv", 
                         stringsAsFactors=F, check.names=F)

response_filtered_w_clusterids <- response_filtered %>% 
  mutate(strata = paste0(lookup_table$district[match(cluster_location_id,cluster_lookup_table$new_ID)],type_hh))


# horizontal operations / recoding

r <- response_filtered_w_clusterids %>%
  new_recoding(source=how_much_debt, target=hh_with_debt_value) %>% 
  recode_to(0.25,where.num.larger.equal = 505000,otherwise.to=0) %>% 

  new_recoding(target=hh_unemployed) %>% 
  recode_to(0 ,where=!(is.na(response_filtered$work) | is.na(response_filtered$actively_seek_work))) %>% 
  recode_to(0.5,where=(work == "no") & (actively_seek_work == "yes")) %>% 

  new_recoding(source=reasons_for_debt, target=hh_unable_basic_needs) %>% 
  recode_to(0.25, where.selected.any = c("health","food","education","basic_hh_expenditure"), otherwise.to=0) %>% 
  
  end_recoding
  
r <- r %>% mutate(score_livelihoods = hh_with_debt_value+hh_unemployed+hh_unable_basic_needs)

# vertical operations / aggregation

### .. should/can this move up to dataset generation? 
names(r)<-to_alphanumeric_lowercase(names(r))
### .. should/can this move up to loading inputs?
questionnaire <- load_questionnaire(r,questions,choices)


# make analysisplan including all questions as dependent variable by HH type, repeated for each governorate:

analysisplan<-make_analysisplan_all_vars(r,
                                         questionnaire
                                         ,independent.variable = "type_hh",
                                         repeat.for.variable = "governorate_mcna"
                                         )



### .. should/can this move up to loading inputs?

samplingframe <- load_samplingframe("./input/Strata_clusters_population.csv")

samplingframe_strata <- samplingframe %>% 
  group_by(stratum) %>% 
  summarize(population = sum(population))

samplingframe_strata<-as.data.frame(samplingframe_strata)

# this line is dangerous. If we end up with missing strata, they're silently removed.
# could we instead kick out more specifically the impossible district/population group combos?
r <- r %>% 
  filter(strata %in% samplingframe_strata$stratum)

strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe_strata,
                 sampling.frame.population.column = "population",
                 sampling.frame.stratum.column = "stratum",
                 data.stratum.column = "strata")



r$cluster_id <- paste(r$cluster_location_id,r$type_hh,sep = "_")

result <- from_analysisplan_map_to_output(r, analysisplan = analysisplan,
                                          weighting = strata_weight_fun,
                                          cluster_variable_name = "cluster_id",
                                          questionnaire)


result_labeled <- result$results %>% lapply(map_to_labeled,questionnaire)

# exporting only small part of results for speed during testing:
subset_of_results<- rep(FALSE,length(results$results))
subset_of_results[500:700]<-TRUE
some_results<-hypegrammaR:::results_subset(results,logical = subset_of_results)

# not sure if this function should be "user facing" or have some wrappers (@Bouke thoughts?)
# essentially it handles all the looping over different column values as hierarchies.
# then each result is visualised by a function passed here that decides how to render each individual result
# see ?hypegrammaR:::map_to_generic_hierarchical_html
hypegrammaR:::map_to_generic_hierarchical_html(some_results,
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

