# setup

library(dplyr)
library(koboquest) # manage kobo questionnairs
library(kobostandards) # check inputs for inconsistencies
library(xlsformfill) # generate fake data for kobo
library(hypegrammaR) # simple stats 4 complex samples
library(composr) # horziontal operations

source("functions/to_alphanumeric_lowercase.R")
source("functions/analysisplan_factory.R")
source("./pre-process_strata_names.R")
# get & format inputs

questions <- read.csv("input/kobo_questions.csv", stringsAsFactors=F, check.names=F)
questions$name <- tolower(questions$name)
questions$relevant <- tolower(questions$relevant)
questions$constraint <- tolower(questions$constraint)
questions$calculation <- tolower(questions$calculation)
questions$choice_filter <- tolower(questions$choice_filter)
write.csv(questions,"input_modified/questions.csv")

choices <- read.csv("input_modified/kobo_choices2.csv", stringsAsFactors=F, check.names=F)
names(choices) <- tolower(names(choices))
choices$name <- tolower(choices$name)
choices$filter <- tolower(choices$filter)
write.csv(choices,"input_modified/choices.csv")


response <- xlsform_fill(questions,choices,5000)

# horizontal operations

response <- response %>% 
  filter(!is.na(type_hh)) %>% 
  mutate(strata = paste0(district_mcna,type_hh))


r <- response %>%
  new_recoding(source=how_much_debt, target=hh_with_debt_value) %>% 
  recode_to(0.25,where.num.larger.equal = 505000,otherwise.to=0) %>% 

  new_recoding(target=hh_unemployed) %>% 
  recode_to(0 ,where=!(is.na(response$work) | is.na(response$actively_seek_work))) %>% 
  recode_to(0.5,where=(work == "no") & (actively_seek_work == "yes")) %>% 

  new_recoding(source=reasons_for_debt, target=hh_unable_basic_needs) %>% 
  recode_to(0.25, where.selected.any = c("health","food","education","basic_hh_expenditure"), otherwise.to=0) %>% 
  
  end_recoding %>% 
  mutate(score_livelihoods = hh_with_debt_value+hh_unemployed+hh_unable_basic_needs)

# Prepare analysis

names(r)<-to_alphanumeric_lowercase(names(r))
questionnaire <- load_questionnaire(r,questions,choices)


# make analysisplan

analysisplan_gov<-make_analysisplan_all_vars(r,
                                         questionnaire
                                         ,independent.variable = "type_hh",
                                         repeat.for.variable = "governorate_mcna"
                                         )



# vertical operations:

samplingframe <- load_samplingframe("./input/Strata_clusters_population.csv")

samplingframe <- samplingframe %>% 
  group_by(stratum) %>% 
  summarize(sum(population))
names(samplingframe)[2] <- "population"
samplingframe<-as.data.frame(samplingframe)

r <- r %>% 
  filter(strata %in% samplingframe$stratum)

weight_fun <- map_to_weighting(sampling.frame = samplingframe,
                 sampling.frame.population.column = "population",
                 sampling.frame.stratum.column = "stratum",
                 data.stratum.column = "strata")

results<-from_analysisplan_map_to_output(data = r,
                                         analysisplan = analysisplan_gov,
                                         weighting = weight_fun,
                                         questionnaire = questionnaire)


datamerge <- results$results %>% hypegrammaR:::map_to_datamerge(rows = "repeat.var.value",
                                                                values = c("numbers"),
                                                                ignore = c("se","min","max"))
  


miniresults %>% map_to_template(type = "summary",dir = "./",filename = "test.html")
saveRDS(questionnaire, "output/questionnaire.RDS")
