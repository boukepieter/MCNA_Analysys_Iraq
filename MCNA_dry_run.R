

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

# debugonce(xlsformfill:::remove_skipped_values)
response <- xlsform_fill(questions,choices,100)
warnings()
response[,1:10]

r <- response %>% new_recoding(source=how_much_debt, target=hh_with_debt_value) %>% 
  recode_to(TRUE,where.num.larger.equal = 505000,otherwise.to=FALSE) %>% 
  end_recoding()

r <- r %>%  new_recoding(target=hh_unemployed) %>% 
  recode_to(FALSE,where=!(is.na(response$work) | is.na(response$actively_seek_work))) %>% 
  recode_to(TRUE,where=(work == "no") & (actively_seek_work == "yes")) %>% 
  end_recoding()

r %>% new_recoding(source=reasons_for_debt, target=hh_unable_basic_needs) %>% 
  recode_to(TRUE, where.selected.any = c("health","food","education","basic_hh_expenditure"), otherwise.to=FALSE)


