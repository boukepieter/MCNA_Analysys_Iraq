
# questionnaire
questions <- read.csv("input/kobo_questions.csv", 
                      stringsAsFactors=F, check.names=F)


choices <- read.csv("input/kobo_choices.csv", 
                    stringsAsFactors=F, check.names=F)


# data
response <- read.csv("input/parent_merged.csv",
                     stringsAsFactors = F, check.names = F)
loop <- read.csv("input/loop_merged.csv", stringsAsFactors = F)


idp_in_camp <- read.csv("input/idp_in_camp.csv",
                        stringsAsFactors = F, check.names = F)
loop_in_camp <- read.csv("input/loop_in_camp.csv", stringsAsFactors = F)




# sampling

cluster_lookup_table <- read.csv("input/combined_sample_ids.csv", 
                                 stringsAsFactors=F, check.names=F)


samplingframe <- load_samplingframe("./input_modified/Strata_clusters_population.csv")

samplingframe_in_camp<-load_samplingframe("./input/sampling_frame_in_camp.csv")
# analysis definition
analysisplan <- read.csv("input/dap.csv", stringsAsFactors = F)






