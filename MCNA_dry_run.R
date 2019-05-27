

library(dplyr)
library(koboquest) # manage kobo questionnairs
library(kobostandards) # check inputs for inconsistencies
library(xlsformfill) # generate fake data for kobo
library(hypegrammaR) # simple stats 4 complex samples
library(composr) # horziontal operations

source("functions/to_alphanumeric_lowercase.R")

questions <- read.csv("input/kobo_questions.csv", stringsAsFactors=F, check.names=F)

questions$name <- tolower(questions$name)
questions$relevant <- tolower(questions$relevant)
questions$constraint <- tolower(questions$constraint)
questions$calculation <- tolower(questions$calculation)
questions$choice_filter <- tolower(questions$choice_filter)



choices <- read.csv("input/kobo_choices.csv", stringsAsFactors=F, check.names=F)

names(choices) <- tolower(names(choices))
choices$name <- tolower(choices$name)
choices$filter <- tolower(choices$filter)


response <- xlsform_fill(questions,choices,10)



response %>%
  new_recoding(source=how_much_debt,
               target=hh_with_debt_value)





names(response)<-to_alphanumeric_lowercase(names(response))
questionnaire <- load_questionnaire(response,questions,choices)

debugonce(map_to_datatypes)
analysisplan_generic<-make_analysisplan_all_vars(response,
                                                 questionnaire = questionnaire)


results<-from_analysisplan_map_to_output(data = response,
                                analysisplan = analysisplan_generic,weighting = function(x){rep(1,length(x))})




charts <- results$results %>% lapply(map_to_labeled,questionnaire) %>% lapply(map_to_visualisation)


charts[[80]]$ggplot



