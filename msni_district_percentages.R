#Import Data Analysis Plan
dap_msni <- read.csv("Input/dap_msni_district.csv")
msni_district <- response_with_composites
#msni_district[, c("lsg_ed", "lsg_el", "lsg_fs", 
#                             "lsg_health", "lsg_protection", 
#                             "lsg_snfi", "lsg_wash", "capacity_gap", 
#                             "vulnerability")] <- ifelse(msni_district[, c("lsg_ed", "lsg_el", "lsg_fs", 
#                                             "lsg_health", "lsg_protection", 
#                                             "lsg_snfi", "lsg_wash", "capacity_gap", 
#                                             "vulnerability")] >= 3, 1, 
#                                             ifelse(msni_district[, c("lsg_ed", "lsg_el", "lsg_fs", 
#                                                                      "lsg_health", "lsg_protection", 
#                                                                      "lsg_snfi", "lsg_wash", "capacity_gap", 
#                                                                      "vulnerability")]<3, 0, NA))




weight_fun_msni<-function(df){
     df$weights
}
msni_district$ones <- "ones"

subset_msni <- msni_district[, c("lsg_ed", "lsg_el", "lsg_fs", 
                  "lsg_health", "lsg_protection", 
                  "lsg_snfi", "lsg_wash", "capacity_gap", 
                  "vulnerability", "district", "ones")]


list_of_results <- from_analysisplan_map_to_output(subset_msni, analysisplan = dap_msni,
                                          weighting = weight_fun_msni,
                                          questionnaire = questionnaire, confidence_level = 0.9)


source("functions/function_summstats.R")
#SUMMARY STATS LIST
summary.stats.list <- list_of_results$results %>% 
  lapply(function(x){map_to_labeled(result = x, questionnaire = questionnaire)})

#SUMMARY STATS LIST EXPORT
summary.stats.list %>% 
  resultlist_summary_statistics_as_one_table %>% 
  map_to_file(".summary_stats.csv")


#SUMMARY STATS LIST FORMATTED WITH p-VALUES
summary.stats.list %>%
  lapply((map_to_labeled),questionnaire) %>%
  lapply(result_format_numbers) %>% 
  lapply(add_p_value_to_summary_table) %>% 
  resultlist_summary_statistics_as_one_table %>%
  
#  write.csv('Output/summary_stats_formatted_with_pvalues.csv')
#browseURL("summary_stats_formatted_with_pvalues.csv")


#SUMMARY STATS LIST FORMATTED
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
summarystats <- summary.stats.list %>% 
  lapply((map_to_labeled),questionnaire) %>% 
  resultlist_summary_statistics_as_one_table 

names(summarystats)
summarystats <- summarystats[, c("dependent.var", "repeat.var.value", "numbers", "min", "max")]

reshape_summstats <- function(summs) {
  summs <- as.data.frame(t(summs))
  names(summs) <- as.matrix(summs[1, ])
  summs <- summs[-1, ]
  summs[] <- lapply(summs, function(x) type.convert(as.character(x)))

  return(summs)
}
#namesvar<-as.vector(sapply( summs[1,], paste0, collapse=""))
#names(summs) <- ifelse(namesvar=="NA", names(summs),
#                       paste(names(summs[1,]), namesvar, sep = "_"))
#summs<-summs[-1,]
summarystats <- reshape_summstats(summarystats)


write.xlsx(summarystats,'Output/meanscore_district.xlsx')

