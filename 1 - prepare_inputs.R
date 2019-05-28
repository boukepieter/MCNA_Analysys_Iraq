questions <- read.csv("input/kobo_questions.csv", stringsAsFactors=F, check.names=F)
questions$name <- tolower(questions$name)
questions$relevant <- tolower(questions$relevant)
questions$constraint <- tolower(questions$constraint)
questions$calculation <- tolower(questions$calculation)
questions$choice_filter <- tolower(questions$choice_filter)

choices <- read.csv("input/kobo_choices2.csv", stringsAsFactors=F, check.names=F)
names(choices) <- tolower(names(choices))
choices$name <- tolower(choices$name)
choices$filter <- tolower(choices$filter)



samplingframe <- load_samplingframe("./input/Strata_clusters_population.csv")
samplingframe <- samplingframe %>% 
  group_by(stratum) %>% 
  summarize(sum(population))
names(samplingframe)[2] <- "population"
samplingframe<-as.data.frame(samplingframe)



r <- r %>% 
  filter(strata %in% samplingframe$stratum)

weight_fun <- map_to_weighting(sampling.frame = samplingframe,
                               sampling.frame.population.column = "population",
                               sampling.frame.stratum.column = "stratum",
                               data.stratum.column = "strata")