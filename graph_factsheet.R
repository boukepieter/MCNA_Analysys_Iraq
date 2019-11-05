devtools::install_github("caldwellst/Setviz", build_vignettes = TRUE, force = TRUE)
library(Setviz)
browseVignettes("Setviz")

r <- response_with_composites[,c("lsg_wash", "lsg_snfi", "lsg_ed", "lsg_el",
                                     "lsg_health", "lsg_protection", "lsg_fs", "weights")]


r <- r %>% dplyr::rename(
  "WASH" = "lsg_wash",
  "Shelter" = "lsg_snfi",
  "Education" =	"lsg_ed",
  "Livelihoods" =	"lsg_el",
  "Health" =	"lsg_health",
  "Protection" =	"lsg_protection",
  "Food_Security" =	"lsg_fs")


varnames <- as.vector(names(r[,c("WASH", "Shelter", "Education", "Livelihoods",
                                 "Health", "Protection", "Food_Security")]))
r$WASH <- ifelse(r$WASH >2, 1,0)
r$Shelter <- ifelse(r$Shelter >2, 1,0)
r$Education <- ifelse(r$Education >2, 1,0)
r$Livelihoods <- ifelse(r$Livelihoods >2, 1,0)
r$Health <- ifelse(r$Health >2, 1,0)
r$Protection <- ifelse(r$Protection >2, 1,0)
r$`Food_Security` <- ifelse(r$`Food_Security` >2, 1,0)

Setviz::plot_set_percentages(r, varnames, weight_variable = "weights",
                     weighting_function = NULL, nintersects = 9, exclude_unique = T, 
                     label = "% in need per combination of sectors")

