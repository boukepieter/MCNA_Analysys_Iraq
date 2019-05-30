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
# get & format inputs

questions <- read.csv("input/kobo_questions.csv", 
                      stringsAsFactors=F, check.names=F)
choices <- read.csv("input/kobo_choices.csv", 
                    stringsAsFactors=F, check.names=F)
response <- xlsform_fill(questions,choices,1000)

# horizontal operations
lookup_table <- read.csv("input/combined_sample_ids.csv", 
                         stringsAsFactors=F, check.names=F)
response_filtered <- response %>% 
  filter(!is.na(type_hh)) %>% 
  mutate(strata = paste0(lookup_table$district[match(cluster_location_id,lookup_table$new_ID)],type_hh))


r <- response_filtered %>%
  new_recoding(source=how_much_debt, target=hh_with_debt_value) %>% 
  recode_to(0.25,where.num.larger.equal = 505000,otherwise.to=0) %>% 

  new_recoding(target=hh_unemployed) %>% 
  recode_to(0 ,where=!(is.na(response_filtered$work) | is.na(response_filtered$actively_seek_work))) %>% 
  recode_to(0.5,where=(work == "no") & (actively_seek_work == "yes")) %>% 

  new_recoding(source=reasons_for_debt, target=hh_unable_basic_needs) %>% 
  recode_to(0.25, where.selected.any = c("health","food","education","basic_hh_expenditure"), otherwise.to=0) %>% 
  
  end_recoding %>% 
  mutate(score_livelihoods = hh_with_debt_value+hh_unemployed+hh_unable_basic_needs)


# Prepare analysis

names(r)<-to_alphanumeric_lowercase(names(r))
questionnaire <- load_questionnaire(r,questions,choices)


# make analysisplan

analysisplan<-make_analysisplan_all_vars(r,
                                         questionnaire
                                         ,independent.variable = "type_hh",
                                         repeat.for.variable = "governorate_mcna"
                                         )



# vertical operations:

samplingframe <- load_samplingframe("./input/Strata_clusters_population.csv")

samplingframe_strata <- samplingframe %>% 
  group_by(stratum) %>% 
  summarize(sum(population))

names(samplingframe_strata)[2] <- "population"
samplingframe_strata<-as.data.frame(samplingframe_strata)


r <- r %>% 
  filter(strata %in% samplingframe_strata$stratum)

strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe_strata,
                 sampling.frame.population.column = "population",
                 sampling.frame.stratum.column = "stratum",
                 data.stratum.column = "strata")



# samplingframe$cluster_id<-paste(samplingframe$stratum, )
# cluster_weight_fun<- map_to_weighting(sampling.frame = samplingframe_strata,
#                                       sampling.frame.population.column = "population",
#                                       sampling.frame.stratum.column = "stratum",
#                                       data.stratum.column = "strata")


r$cluster_id <- paste(r$cluster_location_id,r$type_hh,sep = "_")

result <- from_analysisplan_map_to_output(r, analysisplan = analysisplan,
                                          weighting = strata_weight_fun,
                                          cluster_variable_name = "cluster_id",
                                          questionnaire)


result_labeled <- result$results %>% lapply(map_to_labeled,questionnaire)



?map_to_visualisation
vis <- 
res %>% map_to_template(questionnaire, "./output", type="summary",filename="summary.html")
res %>% map_to_template(questionnaire, "./output", type="full",filename="full.html")

rmarkdown::render(input = 'msna_test.Rmd')

result$results[[800]]$parameters$case <- "CASE_group_difference_categorical_categorical"
  
  

result$results[[800]] %>% map_to_labeled(questionnaire) %>% map_to_visualisation

big_table <- hypegrammaR:::map_to_datamerge(result, questionnaire = questionnaire,rows = "repeat.var.value")

# results$results<-lapply(results$results, function(x){class(x)<-c("hg_result",class(x));
# x})
# 
# 
# print.hg_result <- function(x){
#   x %>% map_to_table() %>% (knitr(kable))
#   
# }
# 
# results$results[[1]]

