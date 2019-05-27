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


response <- xlsform_fill(questions,choices,100)

# horizontal operations

r <- response %>% new_recoding(source=how_much_debt, target=hh_with_debt_value) %>% 
  recode_to(TRUE,where.num.larger.equal = 505000,otherwise.to=FALSE) %>% 
  end_recoding()

r <- r %>%  new_recoding(target=hh_unemployed) %>% 
  recode_to(FALSE,where=!(is.na(response$work) | is.na(response$actively_seek_work))) %>% 
  recode_to(TRUE,where=(work == "no") & (actively_seek_work == "yes")) %>% 
  end_recoding()

r <- r %>% new_recoding(source=reasons_for_debt, target=hh_unable_basic_needs) %>% 
  recode_to(TRUE, where.selected.any = c("health","food","education","basic_hh_expenditure"), otherwise.to=FALSE) %>% 
  end_recoding()



# Prepare analysis

names(response)<-to_alphanumeric_lowercase(names(response))
questionnaire <- load_questionnaire(response,questions,choices)


# make analysisplan

analysisplan<-make_analysisplan_all_vars(response,
                                         questionnaire,
                                         independent.variable = "type_hh")


# vertical operations:

results<-from_analysisplan_map_to_output(data = response,
                                         analysisplan = analysisplan,
                                         weighting = function(x){rep(1,nrow(x))},
                                         questionnaire = questionnaire)



results %>% map_to_template(questionnaire, "./output","test.html")

