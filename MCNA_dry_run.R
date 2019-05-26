

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
response <- xlsform_fill(questions,choices,10)
warnings()
response[,1:10]

response %>% new_recoding(source=how_much_debt, target=hh_with_debt_value)
>>>>>>> 0e3a00c6d7cfde6b3725a64cdf7bee600e6240bc
