# SDC analysis

#Load required libraries
library(readxl)      #for excel, csv sheets manipulation
library(sdcMicro)    #sdcMicro package with functions for the SDC process 
library(tidyverse)   #optional #for data cleaning

#Import data
file <- "c:/Users/REACH-IRQ-GIS/Documents/2019 MCNA/SEND TO AWG/awg_irq_dataset_mcna_vii_september2019_public+new_weights.xlsx"
data <- read_excel(file, sheet = "HH data - representative")

#Select key variables                   
selectedKeyVars <- c( 'population_group','governorate_mcna',
                      'district', 'hhh', 'age_respondent',
                      'gender_respondent', 'governorate_origin',
                      'district_origin', 'num_hh_member',
                      'inc_employment', 'tot_income', 'camp')

#select weights
weightVars <- c('weights')

#Convert variables to factors
cols =  selectedKeyVars

data[,cols] <- lapply(data[,cols], factor)

#Convert sub file to a dataframe
subVars <- c(selectedKeyVars, weightVars)
fileRes<-data[,subVars]
fileRes <- as.data.frame(fileRes)
objSDC <- createSdcObj(dat = fileRes, 
                       keyVars = selectedKeyVars, weightVar = weightVars)
freq(objSDC, type = 'Fk') %>% summary

#print the risk
print(objSDC, "risk")
objSDC@risk$individual

#Generate an internal (extensive) report
report(objSDC, filename = "index_1",internal = T, verbose = TRUE) 


## changes to mitigate disclosure risk
data_clean <- data <- read_excel(file, sheet = "HH data - representative")

# mitigation 1; remove age_respondent and gender_respondent
incomes <- names(data_clean)[which(startsWith(names(data_clean), "inc_"))]
data_clean <- data_clean[, -which(names(data_clean) %in% c("age_respondent", "gender_respondent", "tot_income",
                                                           "district_origin", incomes, "hhh", "will_to_response"))]
selectedKeyVars <- c('population_group',	'governorate_mcna',
                     'district', 'governorate_origin',
                     'num_hh_member', "camp")

# mitigation 2; governorate_origin and district_origin options less than 10 to be recoded to 'other'
data_clean$governorate_origin[which(data_clean$governorate_origin %in% 
                                      c("al.basrah", "al.sulaymaniyah", "duhok"))] <- "other"

# tab <- table(data_clean$district_origin)
# before <- names(tab)[which( tab < 50)]
# data_clean$district_origin[which(data_clean$district_origin %in% before)] <- "other"

# mitigation 3; num_hh_member capped on 25
# data_clean$num_hh_member[which(data_clean$num_hh_member > 15)] <- 15
data_clean <- data_clean %>% mutate(num_hh_member = case_when(
  num_hh_member <= 3 ~ 1, 
  num_hh_member <= 6 ~ 2,
  num_hh_member <= 10 ~ 3,
  num_hh_member <= 15 ~ 4,
  num_hh_member > 15 ~ 5))

# mitigation 4; round inc_employment
# data_clean$inc_employment <- round(as.numeric(data_clean$inc_employment) / 100000) * 100000
# data_clean$inc_employment[which(data_clean$inc_employment > 1000000)] <- 
#   round(as.numeric(data_clean$inc_employment[which(data_clean$inc_employment > 1000000)]) / 1000000) * 1000000
# data_clean$inc_employment[which(data_clean$inc_employment > 2000000)] <- 2000000

# 
# data_clean$tot_income <- round(data_clean$tot_income / 100000) * 100000
# data_clean$tot_income[which(data_clean$tot_income > 1000000)] <- 
#   round(as.numeric(data_clean$tot_income[which(data_clean$tot_income > 1000000)]) / 1000000) * 1000000
# data_clean$tot_income[which(data_clean$tot_income > 2000000)] <- 2000000

# new report
cols =  selectedKeyVars

data_clean[,cols] <- lapply(data_clean[,cols], factor)

#Convert sub file to a dataframe
subVars <- c(selectedKeyVars, weightVars)
fileRes<-data_clean[,subVars]
fileRes <- as.data.frame(fileRes)
objSDC <- createSdcObj(dat = fileRes, 
                       keyVars = selectedKeyVars, weightVar = weightVars)

#print the risk
print(objSDC, "risk")

#Generate an internal (extensive) report
report(objSDC, filename = "index_1",internal = T, verbose = TRUE) 

write.csv(data_clean, "temp.csv", row.names = F)
