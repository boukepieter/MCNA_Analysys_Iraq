
# questionnaire
questions <- read.csv("input/kobo_questions.csv", 
                      stringsAsFactors=F, check.names=F)


choices <- read.csv("input/kobo_choices.csv", 
                    stringsAsFactors=F, check.names=F)

# sampling

cluster_lookup_table <- read.csv("input/combined_sample_ids.csv", 
                                 stringsAsFactors=F, check.names=F)
lookup_table <- read.csv("input/lookup_table_names.csv", stringsAsFactors = F)


samplingframe <- load_samplingframe("./input/strata_population.csv")

samplingframe_in_camp<-load_samplingframe("./input/sampling_frame_in_camp.csv")


# data
response <- read.csv("input/parent_merged.csv",
                     stringsAsFactors = F, check.names = F)
loop <- read.csv("input/loop_merged.csv", stringsAsFactors = F)


idp_in_camp <- read.csv("input/idp_in_camp.csv",
                        stringsAsFactors = F, check.names = F)
idp_in_camp$district <- to_alphanumeric_lowercase(samplingframe_in_camp$district.new[match(idp_in_camp$camp, samplingframe_in_camp$camp)])
loop_in_camp <- read.csv("input/loop_in_camp_v2.csv", stringsAsFactors = F)


# simple random strata
simple_random_strata<-matrix(c("Al-Baaj","idp_out_camp",
                               "Al-Hamdaniya","idp_out_camp",
                               "Al-Kaim","idp_out_camp",
                               "Al-Ramadi","idp_out_camp",
                               "Baladruz","idp_out_camp",
                               "Beygee","idp_out_camp",
                               "Daquq","idp_out_camp",
                               "Dibis","idp_out_camp",
                               "Al-Mussyab","returnee",
                               "Al-Rutba","returnee",
                               "Al-Shikhan","returnee",
                               "Dibis","returnee",
                               "Samarra","returnee",
                               "Al-Adhamiya","host",
                               "Al-Basrah","host",
                               "Al-Kahla","host",
                               "Al-Najaf","host",
                               "Al-Nasiriya","host",
                               "Al-Shirqat","host",
                               "Erbil","host"),ncol = 2,byrow = T)

simple_random_strata[,1]<-to_alphanumeric_lowercase(simple_random_strata[,1])
simple_random_strata<-paste0(simple_random_strata[,1],simple_random_strata[,2])




