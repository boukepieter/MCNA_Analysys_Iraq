setwd("data_cleaning")
source("setup.R")
source("cleaning_functions.R")
dir <- "raw_data/20190818"
# ignore_date <- c("2019-07-21")

data <- read.csv(sprintf("%s/parent_cleaned_anonymised.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data <- data %>% mutate(population_group = ifelse(calc_idp == 1, "idp", ifelse(calc_returnee == 1, "returnee", 
                                                                               ifelse(calc_host == 1, "host", NA)))) 
data_old <- read.csv(sprintf("%s/parent_cleaned_old.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data_new <- data %>% filter((!X_uuid %in% data_old$X_uuid))# & (as.Date(date_assessment) < as.Date(ignore_date)))

child <- read.csv(sprintf("%s/child.csv",dir), stringsAsFactors = F)

#unique uuid's
find_duplicates_uuid(data)

# outliers
#inspect_all(data, uuid.column.name = "X_uuid")
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
data <- read.csv(sprintf("%s/parent_other.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
dir <- "raw_data/20190807"
data_old <- read.csv(sprintf("%s/parent_cleaned_old.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data_new <- data %>% filter((!X_uuid %in% data_old$X_uuid))# & (as.Date(date_assessment) < as.Date(ignore_date)))
result <- translate.others.arabic(data_new, ignore.cols = c("inc_other", "restriction_other"))
dir <- "raw_data/20190819"
write.csv(result, sprintf("%s/translations.csv", dir), row.names = F, fileEncoding = "UTF-8")
uq <- unique(result$question.name)
for (i in 1:length(uq)){
  uuid <- result$uuid[which(result$question.name == uq[i])]
  log <- log.cleaning.change.extended(data, partners, psu, uuid = uuid, action = "f",  
                                      question.name=uq[i], 
                                      issue="Other text, to be recoded",
                                      dir = dir)
}

## household individual data
loop <- read.csv(sprintf("%s/child.csv", dir), stringsAsFactors = F, encoding = "UTF-8")
#loop$X_submission__uuid <- data$X_uuid[loop$X_parent_index]

loop %>% filter(relationship == "error") %>% dplyr::select(X_submission__uuid, X_index)
loop_without_error <- loop %>% filter(relationship != "error")

#	Check whether the household size number given and the individual roster fits, 
# and if there is discrepancy between the household survey and individual loop. 
result <- general.checks(data_new, loop_without_error)
summary <- summarize.result(result)
summary
round(summary / nrow(data_new) * 100)

#tank capacity
uuid <- data$X_uuid[which(data$tank_capacity == 999)]
log <- log.cleaning.change.extended(data, partners, psu, uuid, action = "c",  
                                    question.name="tank_capacity", 
                                    issue="999 is do not know value and is recoded to NA",
                                    new.value = NA,
                                    dir = dir)

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

## recoding flagged issues
dir <- "raw_data/20190820"
log <- read.csv(sprintf("%s/cleaning_logbook.csv",dir), stringsAsFactors = F)
data <- read.csv(sprintf("%s/parent_cleaned.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data2 <- read.csv(sprintf("%s/parent2_cleaned.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
data2$X_validation_status <- NA
#data <- rbind(data[,-which(!names(data) %in% names(data2))],data2)
names(data)[1] <- "start"
data <- rbind.fill(data2, data)
data <- data %>% mutate(population_group = ifelse(calc_idp == 1, "idp", ifelse(calc_returnee == 1, "returnee", 
                                                                               ifelse(calc_host == 1, "host", NA))))
loop <- read.csv(sprintf("%s/child_cleaned.csv", dir), stringsAsFactors = F, encoding = "UTF-8")
loop2 <- read.csv(sprintf("%s/child2.csv", dir), stringsAsFactors = F, encoding = "UTF-8")
loop2$X_submission__uuid <- data2$X_uuid[loop2$X_parent_index]
#write.csv(loop2, sprintf("%s/child2.csv", dir), row.names = F)
loop3 <- rbind(loop[,-c(48,50,51)],loop2)
loop <- loop3
write.csv(loop, sprintf("%s/loop_merged.csv", dir), row.names = F, fileEncoding = "UTF-8")
#loop %>% filter(relationship == "error") %>% dplyr::select(X_submission__uuid, X_index)
# uuid <- data$X_uuid[which(data$calc_noteligible == 1)]
# log <- log.cleaning.change.extended(data, partners, psu, uuid, action = "d",  
#                              question.name="calc_noteligible", 
#                              issue="HH is not IDP, not Returnee, not Host so not eligible and has to be deleted from the dataset", dir=dir)
# errors <- loop %>% filter(relationship == "error")
# index <- errors$X_index
# log <- log.cleaning.change.loop(data, loop, partners, psu, index, action="f", dir = dir,
#                          question.name = "relationship", issue = "error in loop, line needs to be deleted")

entries <- which(log$feedback == "Surrounding villages couldn't be reached because of ongoing security operations. So all done in town and recoded to the 3 clusters present in the town.")
log$action[entries[which(data$population_group[which(data$X_uuid %in% log$uuid[entries])] == "idp")]] <- "flag"

data$population_group[which(data$cluster_location_id %in% c("cluster_location_id_0481","cluster_location_id_0482"))]
entries <- which(log$issue == "Children missing birth certificate reported but the amount of children missing it is 0.")
for (i in 1:length(entries)){
  log$question.name[entries[i]] <- "birth_cert_missing_amount_a1"
  log$action[entries[i]] <- "change"
  log$old.value[entries[i]] <- 0
  log$new.value[entries[i]] <- 999
}
entries <- which(log$issue == "Children missing id card reported but the amount of children missing it is 0.")
for (i in 1:length(entries)){
  log$action[entries[i]] <- "change"
  log$new.value[entries[i]] <- 999
}
entries <- which(log$issue == "Employment is selected as primary livelihood source but income from employment is given as 0.")
for (i in 1:length(entries)){
  log$action[entries[i]] <- "change"
  log$new.value[entries[i]] <- 999
}
entries <- which(log$issue == "Number of family members should always be lower or equal to household size, but for these it is not.")
for (i in 1:length(entries)){
  if (log$uuid[entries[i]] %in% data$X_uuid){
    log$question.name[entries[i]] <- "num_hh_member"
    log$action[entries[i]] <- "change"
    log$new.value[entries[i]] <- as.numeric(log$old.value[entries[i]])
    log$old.value[entries[i]] <- data$num_hh_member[which(data$X_uuid == log$uuid[entries[i]])]
  } else {
    log <- log[-entries[i],]
    entries <- entries -1
  }
}
entries <- which(log$issue == "Number of responses in the loop is not equal to family size (after deleting error lines)")
for (i in 1:length(entries)){
  uuid <- log$uuid[entries[i]]
  if (uuid %in% data$X_uuid){
    log$action[entries[i]] <- "change"
    log$new.value[entries[i]] <- length(which(loop_without_error$X_submission__uuid == uuid))
  }else {
    log <- log[-entries[i],]
    entries <- entries -1
  }
}
entries <- which(log$issue == "Reported that divorce certificate above 18 is not applicable but in the loop there are people above 18 divorced.")
for (i in 1:length(entries)){
  if (log$uuid[entries[i]] %in% data$X_uuid){
    log$action[entries[i]] <- "change"
    log$new.value[entries[i]] <- "non_valid"
  }else {
    log <- log[-entries[i],]
    entries <- entries -1
  }
}
entries <- which(log$issue == "Reported that marriage certificate above 18 is not applicable but in the loop there are people above 18 married.")
for (i in 1:length(entries)){
  if (log$uuid[entries[i]] %in% data$X_uuid){
    log$action[entries[i]] <- "change"
    log$new.value[entries[i]] <- "non_valid"
  }else {
    log <- log[-entries[i],]
    entries <- entries -1
  }
}
entries <- which(log$issue == "Reported that marriage certificate under 18 is not applicable but in the loop there are people under 18 married.")
for (i in 1:length(entries)){
  if (log$uuid[entries[i]] %in% data$X_uuid){
    log$action[entries[i]] <- "change"
    log$new.value[entries[i]] <- "non_valid"
  }else {
    log <- log[-entries[i],]
    entries <- entries -1
  }
}
entries <- which(log$issue == "Total reported debt is lower than the debt taken in the last 30 days.")
for (i in 1:length(entries)){
  if (log$uuid[entries[i]] %in% data$X_uuid){
    log$action[entries[i]] <- "change"
    log$new.value[entries[i]] <- data$inc_debt[which(data$X_uuid == log$uuid[entries[i]])]
  }else {
    log <- log[-entries[i],]
    entries <- entries -1
  }
}


change_hhh <- function(data, log, loop, entries){
  for (i in 1:length(entries)){
    print(i)
    data_subset <- data[which(data$X_uuid == log$uuid[entries[i]]),]
    is_hhh <- data_subset$hhh == "yes"
    loop_subset <- loop[which(loop$X_submission__uuid == log$uuid[entries[i]]),]
    number_hhh <- length(which(loop_subset$relationship == "head"))
    same_gender <- data_subset$gender_respondent == loop_subset$sex[which(loop_subset$relationship == "head")]
    
    if (nrow(loop_subset) < 1) {
      test[i] <- 0
      log$feedback[entries[i]] <- "no loop for this interview exists so also no hhh"
    } else if (is_hhh & number_hhh == 1 & ! same_gender[1]) {
      test[i] <- 1
      log$survey[entries[i]] <- "loop"
      log$uuid[entries[i]] <- paste(log$uuid[entries[i]], which(loop_subset$relationship == "head"), sep="|")
      log$question.name[entries[i]] <- "sex"
      log$action[entries[i]] <- "change"
      log$feedback[entries[i]] <- "gender in loop changed to gender given by respondent"
      log$old.value[entries[i]] <- loop_subset$sex[which(loop_subset$relationship == "head")]
      log$new.value[entries[i]] <- data_subset$gender_respondent
    } else if (is_hhh & number_hhh == 1 & same_gender[1]) {
      test[i] <- 1.5
      log$feedback[entries[i]] <- "keep, only age of hhh in loop is different from respondent, no large implications."
    } else if (is_hhh & number_hhh > 1) {
      if (length(which(same_gender)) == 1) {
        test[i] <- 2.1
        log$feedback[entries[i]] <- "keep, multiple hhh in loop, the ones with a different gender as respondent will be recoded to non hhh."
        to_be_changed <- which(loop_subset$relationship == "head")[-which(same_gender)[1]]
        for (j in to_be_changed){
          rownr <- nrow(log) + 1
          log[rownr,] <- log[entries[i],]
          log$survey[rownr] <- "loop"
          log$uuid[rownr] <- paste(log$uuid[entries[i]], j, sep="|")
          log$question.name[rownr] <- "relationship"
          log$action[rownr] <- "change"
          log$feedback[rownr] <- "relationship in loop changed to NA, so that only 1 hhh in the loop remains (based on same gender as respondent)"
          log$old.value[rownr] <- "head"
          log$new.value[rownr] <- NA
        }
        
      } else if (length(which(same_gender)) > 1) {
        test[i] <- 2.2
        log$feedback[entries[i]] <- "keep, multiple hhh in loop, all but the first one with a different gender as respondent will be recoded to non hhh."
        to_be_changed <- which(loop_subset$relationship == "head")[-which(same_gender)[1]]
        for (j in to_be_changed){
          rownr <- nrow(log) + 1
          log[rownr,] <- log[entries[i],]
          log$survey[rownr] <- "loop"
          log$uuid[rownr] <- paste(log$uuid[entries[i]], j, sep="|")
          log$question.name[rownr] <- "relationship"
          log$action[rownr] <- "change"
          log$feedback[rownr] <- "relationship in loop changed to NA, so that only 1 hhh in the loop remains (based on same gender as respondent)"
          log$old.value[rownr] <- "head"
          log$new.value[rownr] <- NA
        }
      } else if (length(which(same_gender)) < 1) {
        test[i] <- 2.3
        log$feedback[entries[i]] <- "keep, multiple hhh in loop, all but the first one will be recoded to non hhh."
        to_be_changed1 <- which(loop_subset$relationship == "head")[1]
        to_be_changed2 <- which(loop_subset$relationship == "head")[-1]
        for (j in to_be_changed1){
          rownr <- nrow(log) + 1
          log[rownr,] <- log[entries[i],]
          log$survey[rownr] <- "loop"
          log$uuid[rownr] <- paste(log$uuid[entries[i]], j, sep="|")
          log$question.name[rownr] <- "sex"
          log$action[rownr] <- "change"
          log$feedback[rownr] <- "hhh gender based on gender of respondent"
          log$old.value[rownr] <- loop_subset$sex[j]
          log$new.value[rownr] <- data_subset$gender_respondent
        }
        for (j in to_be_changed2){
          rownr <- nrow(log) + 1
          log[rownr,] <- log[entries[i],]
          log$survey[rownr] <- "loop"
          log$uuid[rownr] <- paste(log$uuid[entries[i]], j, sep="|")
          log$question.name[rownr] <- "relationship"
          log$action[rownr] <- "change"
          log$feedback[rownr] <- "keeping only first hhh in the loop with same gender as respondent"
          log$old.value[rownr] <- "head"
          log$new.value[rownr] <- NA
        }
      } 
      
    } else if (! is_hhh & number_hhh > 1) {
      test[i] <- 3
      log$feedback[entries[i]] <- "keeping only the first hhh in the loop."
      to_be_changed <- which(loop_subset$relationship == "head")[-1]
      for (j in to_be_changed){
        rownr <- nrow(log) + 1
        log[rownr,] <- log[entries[i],]
        log$survey[rownr] <- "loop"
        log$uuid[rownr] <- paste(log$uuid[entries[i]], j, sep="|")
        log$question.name[rownr] <- "relationship"
        log$action[rownr] <- "change"
        log$feedback[rownr] <- "keeping only first hhh in the loop"
        log$old.value[rownr] <- "head"
        log$new.value[rownr] <- NA
      }
    } else if (! is_hhh & number_hhh < 1) {
      test[i] <- 4
      loop_male <- loop_subset[which(loop_subset$sex == "male"),]
      if(nrow(loop_male) < 1) {loop_male <- loop_subset}
      tobechanged <- which(loop_subset$X_index == loop_male$X_index[which(loop_male$age == max(loop_male$age, na.rm=T))[1]])
      log$survey[entries[i]] <- "loop"
      log$uuid[entries[i]] <- paste(log$uuid[entries[i]], tobechanged, sep="|")
      log$question.name[entries[i]] <- "relationship"
      log$action[entries[i]] <- "change"
      log$feedback[entries[i]] <- "no hhh in loop so oldest male assumed to be the hhh"
      log$old.value[entries[i]] <- loop_subset$relationship[tobechanged]
      log$new.value[entries[i]] <- "head"
    } else if (is_hhh & number_hhh < 1) {
      test[i] <- 6
      same_sex <- loop_subset[which(loop_subset$sex == data_subset$gender_respondent),]
      if (nrow(same_sex) > 0) {
        tobechanged <- which(loop_subset$X_index == same_sex$X_index[which.min(abs(same_sex$age - data_subset$age_respondent))])
        log$survey[entries[i]] <- "loop"
        log$uuid[entries[i]] <- paste(log$uuid[entries[i]], tobechanged, sep="|")
        log$question.name[entries[i]] <- "relationship"
        log$action[entries[i]] <- "change"
        log$feedback[entries[i]] <- "person in loop with same gender and closest age to respondent is assumed being the hhh"
        log$old.value[entries[i]] <- loop_subset$relationship[tobechanged]
        log$new.value[entries[i]] <- "head"
      } else {
        tobechanged <- which.max(loop_subset$age)
        log$survey[entries[i]] <- "loop"
        log$uuid[entries[i]] <- paste(log$uuid[entries[i]], tobechanged, sep="|")
        log$question.name[entries[i]] <- "relationship"
        log$action[entries[i]] <- "change"
        log$feedback[entries[i]] <- "person in loop with same gender and closest age to respondent is assumed being the hhh"
        log$old.value[entries[i]] <- loop_subset$relationship[tobechanged]
        log$new.value[entries[i]] <- "head"
      }
    } else {
      test[i] <- NA
    }
  }
  return(list(log, test))
}
entries1 <- which(log$issue %in% c("There is not 1 Head of Household in the individual loop. There is either non or more than 1.",
                                   "The respondent is head of household but doesn't have same age and gender as head of household in the loop."))
entries <- entries1[which(log$uuid[entries1] %in% data$X_uuid)]
test <- numeric()
l <- change_hhh(data, log, loop, entries)
log <- l[[1]]

table(l[[2]], useNA = "always")
write.csv(log, sprintf("%s/cleaning_logbook.csv",dir), row.names = F)
