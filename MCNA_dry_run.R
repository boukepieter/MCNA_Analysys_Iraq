# setup

library(dplyr)
library(koboquest) # manage kobo questionnairs
library(kobostandards) # check inputs for inconsistencies
library(xlsformfill) # generate fake data for kobo
library(hypegrammaR) # simple stats 4 complex samples
library(composr) # horziontal operations

source("functions/to_alphanumeric_lowercase.R")
source("functions/analysisplan_factory.R")

# get & format inputs

# questions <- read.csv("c:/Users/REACH-IRQ-GIS/Documents/201905 MCNA KoBo/survey.csv", stringsAsFactors=F, check.names=F)
# questions$type <- tolower(questions$type)
# questions$name <- tolower(questions$name)
# questions$relevant <- tolower(questions$relevant)
# questions$constraint <- tolower(questions$constraint)
# questions$calculation <- tolower(questions$calculation)
# questions$choice_filter <- tolower(questions$choice_filter)
# write.csv(questions, "c:/Users/REACH-IRQ-GIS/Documents/201905 MCNA KoBo/survey_tolower.csv", row.names=F)
# 
# choices <- read.csv("c:/Users/REACH-IRQ-GIS/Documents/201905 MCNA KoBo/choices.csv", stringsAsFactors=F, check.names=F)
# choices$list_name <- tolower(choices$list_name)
# choices$name <- tolower(choices$name)
# choices$Filter <- tolower(choices$Filter)
# write.csv(choices, "c:/Users/REACH-IRQ-GIS/Documents/201905 MCNA KoBo/choices_tolower.csv", row.names=F)
# 
# districts <- read.csv("c:/Users/REACH-IRQ-GIS/Documents/201905 MCNA KoBo/districts.csv", stringsAsFactors=F, check.names=F)
# districts$list_name <- tolower(districts$list_name)
# districts$name <- to_alphanumeric_lowercase(districts$label)
# districts$filter <- to_alphanumeric_lowercase(districts$filter)
# write.csv(districts, "c:/Users/REACH-IRQ-GIS/Documents/201905 MCNA KoBo/districts_tolower.csv", row.names=F)

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


samplingframe$cluster_id<-paste(samplingframe$stratum, )
cluster_weight_fun<- map_to_weighting(sampling.frame = samplingframe,
                                      sampling.frame.population.column = "population",
                                      sampling.frame.stratum.column = "stratum",
                                      data.stratum.column = "strata")



results<-from_analysisplan_map_to_output(data = r,
                                         analysisplan = analysisplan,
                                         weighting = weight_fun,#function(x){rep(1,nrow(x))},
                                         questionnaire = questionnaire)

results <- readRDS("temp.RDS")

library(knitr)
detach("package:knitr")
library(hypegrammaR)
map_to_labeled(results,questionnaire)
?map_to_visualisation
vis <- 
results %>% map_to_template(questionnaire, "./output", type="summary",filename="summary.html")
results %>% map_to_template(questionnaire, "./output", type="full",filename="full.html")

rmarkdown::render(input = 'msna_test.Rmd')
