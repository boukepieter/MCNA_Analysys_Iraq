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
                      'district', 'hhh',
                      'governorate_origin', 'district_origin',
                      'num_hh_member', 'inc_employment','tot_income')

#select weights
weightVars <- c('weights2')

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

# recode
objSDC  <-  groupAndRename(obj = objSDC, var = c("governorate_origin"),
                          before = c("al.basrah", "al.sulaymaniyah"),
                          after = c("other", "other"))
tab <- table(data$district_origin)
before <- names(tab)[which( tab < 10)]
objSDC  <-  groupAndRename(obj = objSDC, var = c("district_origin"),
                          before = before, after = rep.int("other", length(before)))

objSDC <- calcRisks(objSDC)
#Generate an internal (extensive) report
report(objSDC, filename = "index_1",internal = T, verbose = TRUE) 
