setwd("data_cleaning")
source("setup.R")
source("cleaning_functions.R")
dir <- "raw_data/20190714"
ignore_date <- c("2019-07-19")

data <- read.csv(sprintf("%s/parent_cleaned_anonymised.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data <- data %>% mutate(population_group = ifelse(calc_idp == 1, "idp", ifelse(calc_returnee == 1, "returnee", 
                                                                               ifelse(calc_host == 1, "host", NA)))) 
data_old <- read.csv(sprintf("%s/parent_cleaned_old.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data_new <- data %>% filter((!X_uuid %in% data_old$X_uuid) & (as.Date(date_assessment) < as.Date(ignore_date)))

child <- read.csv(sprintf("%s/child.csv",dir), stringsAsFactors = F)
psu <- read.csv("raw_data/combined_sample_ids.csv", stringsAsFactors = F)

#unique uuid's
find_duplicates_uuid(data)

# outliers

inspect_all(data, uuid.column.name = "X_uuid")
outliers <- find_outliers(data)
outliers$value_USD <- round(outliers$value / 1200)
outliers[,c("ngo", "enumerator_num")] <- data[outliers$index, c('ngo', "enumerator_num")]
outliers$ngo <- partners$V2[match(outliers$ngo, partners$V1)]
outliers <- outliers %>% filter(variable != "enumerator_num")
print(outliers[,c(7,8,1,3,2,6,5)])

uuid <- data$X_uuid[outliers$index]
variables <- unique(outliers$variable)
for (i in 1:length(variables)){
  question <- variables[i]
  
  log.cleaning.change.extended(data, partners, psu, uuid[which(outliers$variable == question)], action = "f",  
                             question.name=question, 
                             issue="outlier", dir=dir)
}

## interview speed
times <- difftime(as.POSIXct(data$end,format="%Y-%m-%dT%H:%M:%OS"), 
                  as.POSIXct(data$start,format="%Y-%m-%dT%H:%M:%OS"), units='mins')
median(times)
hist(as.numeric(times), breaks=c(min(times),0,10,20,30,40,50,60,70,80,90,120,max(times)),
     xlim=c(-30,150), labels=T, freq=T)

# shortest path (# of NA's)
NAs <- apply(data,1,FUN=function(x){length(which(is.na(x)))})
overview_times <- data.frame(enumerator=data$enumerator_num, time=times, family_size=data$num_family_member,
                             NAs=NAs, ngo=data$ngo)
mean_per_enum <- overview_times %>% group_by(enumerator) %>% summarize(mean(time))
write.csv(overview_times, sprintf("%s/overview.csv",dir), row.names = F)


## translating other...
data <- read.csv(sprintf("%s/parent_cleaned_anonymised.csv", dir), stringsAsFactors = F, encoding = "UTF-8")
result <- translate.others.arabic(data, ignore.cols = c("inc_other", "restriction_other"))
write.csv(result, sprintf("%s/translations.csv", dir), row.names = F, fileEncoding = "UTF-8")

## household individual data
loop <- read.csv(sprintf("%s/child.csv", dir), stringsAsFactors = F, encoding = "UTF-8")

loop %>% filter(relationship == "error") %>% dplyr::select(X_submission__uuid, X_index)
loop_without_error <- loop %>% filter(relationship != "error")

#	Check whether the household size number given and the individual roster fits, 
# and if there is discrepancy between the household survey and individual loop. 
result <- general.checks(data_new, loop_without_error)
summary <- summarize.result(result)
summary
round(summary / nrow(data_new) * 100)

#table(data$ngo)
#summary.of.partner(data, loop_without_error, "mcna01")
uuid <- result$uuid[which(!result$loop_is_family_size)]
log <- log.cleaning.change.extended(data_new, partners, psu, uuid, action = "f",  
                                    question.name="num_family_member", 
                                    issue="Number of responses in the loop is not equal to family size (after deleting error lines)",
                                    dir = dir)
uuid <- result$uuid[which(!result$one_hoh_in_loop)]
log <- log.cleaning.change.extended(data_new, partners, psu, uuid, action = "f",  
                                    question.name="ind_level", 
                                    issue="There is not 1 Head of Household in the individual loop. There is either non or more than 1.",
                                    dir = dir)
uuid <- result$uuid[which(!result$hoh_info_same_as_respondent)]
log <- log.cleaning.change.extended(data_new, partners, psu, uuid, action = "f",  
                                    question.name="hhh", 
                                    issue="The respondent is head of household but doesn't have same age and gender as head of household in the loop.",
                                    dir = dir)


## displacement length check
returns <- data_new %>% filter(population_group == "returnee")
flag <- (as.POSIXct(returns$return_date_returnee, format="%Y-%m-%d") - 
           as.difftime(returns$return_durationdisplace * 30.5, units = "days")) <
  as.POSIXct("2014-01-01", format="%Y-%m-%d")
returns[which(flag), c("return_date_returnee", "return_durationdisplace")]
uuid <- returns$X_uuid[which(flag)]
log <- log.cleaning.change.extended(data_new, partners, psu, uuid, action = "f",  
                             question.name="return_durationdisplace", 
                             issue="According to their duration of displacement they have been displaced from before 1-1-2014",
                             dir = dir)

## first displacement to arrival at location check (26 weeks)
idp_first_place <- data_new %>% filter(idp_first_place == "yes")
flag <- difftime(as.POSIXct(idp_first_place$arrival_date_idp, format="%Y-%m-%d"), 
  as.POSIXct(idp_first_place$displace_date_idp, format="%Y-%m-%d"), units = "weeks") > 26
idp_first_place[which(flag), c("displace_date_idp", "arrival_date_idp")]
uuid <- idp_first_place$X_uuid[which(flag)]
log <- log.cleaning.change.extended(data_new, partners, psu, uuid, action = "f",  
                                    question.name="arrival_date_idp", 
                                    issue="The difference between displace date and arrival date while it being first place of displacement is more than 6 months.",
                                    dir = dir)

## family size > household size
flag <- data_new$num_hh_member - data_new$num_family_member < 0
data_new[which(flag), c("num_hh_member", "num_family_member")]
uuid <- data_new$X_uuid[which(flag)]
log <- log.cleaning.change.extended(data_new, partners, psu, uuid, action = "f",  
                                    question.name="num_family_member", 
                                    issue="Number of family members should always be lower or equal to household size, but for these it is not.",
                                    dir = dir)

## Employment as primary source of livelihood but 0 income in previous 30 days from employment
flag <- data_new$primary_livelihood.employment == 1 & data_new$inc_employment < 1000
data_new[which(flag), c("primary_livelihood.employment", "inc_employment")]
uuid <- data_new$X_uuid[which(flag)]
log <- log.cleaning.change.extended(data_new, partners, psu, uuid, action = "f",  
                                    question.name="inc_employment", 
                                    issue="Employment is selected as primary livelihood source but income from employment is given as 0.",
                                    dir = dir)

## last 30 days debt to be lower than total debt
flag <- data_new$inc_debt > data_new$how_much_debt
data_new[which(flag), c("inc_debt", "how_much_debt")]
uuid <- data_new$X_uuid[which(flag)]
log <- log.cleaning.change.extended(data_new, partners, psu, uuid, action = "f",  
                                    question.name="how_much_debt", 
                                    issue="Total reported debt is lower than the debt taken in the last 30 days.",
                                    dir = dir)

## Marriage and divorce certificates not being applicable while family has married or divorced people
married_a18 <- unique(child$X_submission__uuid[which(child$age >= 18 & child$marital_status == "married")])
married_u18 <- unique(child$X_submission__uuid[which(child$age < 18 & child$marital_status == "married")])
divorced_a18 <- unique(child$X_submission__uuid[which(child$age >= 18 & child$marital_status == "divorced")])
divorced_u18 <- unique(child$X_submission__uuid[which(child$age < 18 & child$marital_status == "divorced")])
flag_married_a18 <- data_new$X_uuid %in% married_a18 & data_new$marriage_cert_a18 == "no_one"
flag_married_u18 <- data_new$X_uuid %in% married_u18 & data_new$marriage_cert_u18 == "no_one"
flag_divorced_a18 <- data_new$X_uuid %in% divorced_a18 & data_new$divorce_cert_a18 == "no_one"
flag_divorced_u18 <- data_new$X_uuid %in% divorced_u18 & data_new$divorce_cert_u18 == "no_one"
uuid <- data_new$X_uuid[which(flag_married_a18)]
log <- log.cleaning.change.extended(data_new, partners, psu, uuid, action = "f",  
                                    question.name="marriage_cert_a18", 
                                    issue="Reported that marriage certificate above 18 is not applicable but in the loop there are people above 18 married.",
                                    dir = dir)
uuid <- data_new$X_uuid[which(flag_married_u18)]
log <- log.cleaning.change.extended(data_new, partners, psu, uuid, action = "f",  
                                    question.name="marriage_cert_u18", 
                                    issue="Reported that marriage certificate under 18 is not applicable but in the loop there are people under 18 married.",
                                    dir = dir)
uuid <- data_new$X_uuid[which(flag_divorced_a18)]
log <- log.cleaning.change.extended(data_new, partners, psu, uuid, action = "f",  
                                    question.name="divorce_cert_a18", 
                                    issue="Reported that divorce certificate above 18 is not applicable but in the loop there are people above 18 divorced.",
                                    dir = dir)
uuid <- data_new$X_uuid[which(flag_divorced_u18)]
log <- log.cleaning.change.extended(data_new, partners, psu, uuid, action = "f",  
                                    question.name="divorce_cert_u18", 
                                    issue="Reported that divorce certificate under 18 is not applicable but in the loop there are people under 18 divorced.",
                                    dir = dir)

## Children without birth certificate or id card but the amount without being 0
flag_birth <- data_new$birth_cert_u18 == "no" & (data_new$birth_cert_missing_amount_a1 +
  data_new$birth_cert_missing_amount_u1) == 0
flag_id <- data_new$id_card_u18 == "no" & data_new$id_card_missing_amount == 0
uuid <- data_new$X_uuid[which(flag_birth)]
log <- log.cleaning.change.extended(data_new, partners, psu, uuid, action = "f",  
                                    question.name="birth_cert_u18", 
                                    issue="Children missing birth certificate reported but the amount of children missing it is 0.",
                                    dir = dir)
uuid <- data_new$X_uuid[which(flag_id)]
log <- log.cleaning.change.extended(data_new, partners, psu, uuid, action = "f",  
                                    question.name="id_card_missing_amount", 
                                    issue="Children missing id card reported but the amount of children missing it is 0.",
                                    dir = dir)

## average amount of 

