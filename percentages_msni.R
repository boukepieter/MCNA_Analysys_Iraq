library(questionr)

tables_prop_msni <- function(r, pillar=p, pop=pop, w){
  r <- as.data.frame(prop.table(wtd.table(r[, c(pillar)]
                                          [which(r$population_group == pop)], 
                                          weights=r[, c(w)]
                                          [which(r$population_group == pop)], na.show=F, na.rm=T)))
  return(r)
}



tables_prop_all <- function(r, pillar=p, w){
  r <- as.data.frame(prop.table(wtd.table(r[, c(pillar)], 
                                          weights=r[, c(w)], na.show=F, na.rm=T)))
  return(r)
}

#CREATE TABLES WITH PERCENTAGES FOR EACH SUB-PILLAR FOR TOTAL POP AND 
#DISAGGREGATED BY POPULATION GROUP
percent_msni <- function(response_with_composites){

tables_prop_msni <- function(r, pillar=p, pop=pop, w){
  r <- as.data.frame(prop.table(wtd.table(r[, c(pillar)]
                                          [which(r$population_group == pop)], 
                                          weights=r[, c(w)]
                                          [which(r$population_group == pop)], na.show=F, na.rm=T)))
  return(r)
}



tables_prop_all <- function(r, pillar=p, w){
r <- as.data.frame(prop.table(wtd.table(r[, c(pillar)], 
     weights=r[, c(w)], na.show=F, na.rm=T)))
  return(r)
}

#MSNI
#msni_all <- tables_prop_all(response_with_composites, pillar="msni", "weights")
#names(msni_all) <- c("score", "msni_all")
#msni_out_camp <- tables_prop_msni(response_with_composites, pillar="msni", "idp_out_camp", "weights")
#names(msni_out_camp) <- c("score", "msni_out_camp")
#msni_in_camp <- tables_prop_msni(response_with_composites, pillar="msni", "idp_in_camp", "weights")
#names(msni_in_camp) <- c("score", "msni_in_camp")
#msni_returnee <- tables_prop_msni(response_with_composites, pillar="msni", "returnee", "weights")
#names(msni_returnee) <- c("score", "msni_returnee")
#msni_host <- tables_prop_msni(response_with_composites, pillar="msni", "host", "weights")
#names(msni_host) <- c("score", "msni_host")
#msni<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(msni_all, msni_out_camp, msni_in_camp, 
#                                                             msni_returnee, msni_host))

#IMPACT
impact_all <- tables_prop_all(response_with_composites, pillar="impact", "weights")
names(impact_all) <- c("score", "impact_all")
impact_out_camp <- tables_prop_msni(response_with_composites, pillar="impact", "idp_out_camp", "weights")
names(impact_out_camp) <- c("score", "impact_out_camp")
impact_in_camp <- tables_prop_msni(response_with_composites, pillar="impact", "idp_in_camp", "weights")
names(impact_in_camp) <- c("score", "impact_in_camp")
impact_returnee <- tables_prop_msni(response_with_composites, pillar="impact", "returnee", "weights")
names(impact_returnee) <- c("score", "impact_returnee")
impact_host <- tables_prop_msni(response_with_composites, pillar="impact", "host", "weights")
names(impact_host) <- c("score", "impact_host")
impact<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(impact_all, impact_out_camp, impact_in_camp, 
                                                               impact_returnee, impact_host))

#LSG_WASH
wash_all <- tables_prop_all(response_with_composites, pillar="lsg_wash", "weights")
names(wash_all) <- c("score", "wash_all")
wash_out_camp <- tables_prop_msni(response_with_composites, pillar="lsg_wash", "idp_out_camp", "weights")
names(wash_out_camp) <- c("score", "wash_out_camp")
wash_in_camp <- tables_prop_msni(response_with_composites, pillar="lsg_wash", "idp_in_camp", "weights")
names(wash_in_camp) <- c("score", "wash_in_camp")
wash_returnee <- tables_prop_msni(response_with_composites, pillar="lsg_wash", "returnee", "weights")
names(wash_returnee) <- c("score", "wash_returnee")
wash_host <- tables_prop_msni(response_with_composites, pillar="lsg_wash", "host", "weights")
names(wash_host) <- c("score", "wash_host")
wash<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(wash_all, wash_out_camp, wash_in_camp, 
                                                             wash_returnee, wash_host))

#CAPACITY GAP
capacity_all <- tables_prop_all(response_with_composites, pillar="capacity_gap", "weights")
names(capacity_all) <- c("score", "capacity_all")
capacity_out_camp <- tables_prop_msni(response_with_composites, pillar="capacity_gap", "idp_out_camp", "weights")
names(capacity_out_camp) <- c("score", "capacity_out_camp")
capacity_in_camp <- tables_prop_msni(response_with_composites, pillar="capacity_gap", "idp_in_camp", "weights")
names(capacity_in_camp) <- c("score", "capacity_in_camp")
capacity_returnee <- tables_prop_msni(response_with_composites, pillar="capacity_gap", "returnee", "weights")
names(capacity_returnee) <- c("score", "capacity_returnee")
capacity_host <- tables_prop_msni(response_with_composites, pillar="capacity_gap", "host", "weights")
names(capacity_host) <- c("score", "capacity_host")
capacity_gap<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(capacity_all, capacity_out_camp, capacity_in_camp, 
                                                                     capacity_returnee, capacity_host))

#EL
el_all <- tables_prop_all(response_with_composites, pillar="lsg_el", "weights")
names(el_all) <- c("score", "el_all")
el_out_camp <- tables_prop_msni(response_with_composites, pillar="lsg_el", "idp_out_camp", "weights")
names(el_out_camp) <- c("score", "el_out_camp")
el_in_camp <- tables_prop_msni(response_with_composites, pillar="lsg_el", "idp_in_camp", "weights")
names(el_in_camp) <- c("score", "el_in_camp")
el_returnee <- tables_prop_msni(response_with_composites, pillar="lsg_el", "returnee", "weights")
names(el_returnee) <- c("score", "el_returnee")
el_host <- tables_prop_msni(response_with_composites, pillar="lsg_el", "host", "weights")
names(el_host) <- c("score", "el_host")
el<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(el_all, el_out_camp, el_in_camp, 
                                                           el_returnee, el_host))

#PROTECTION
protection_all <- tables_prop_all(response_with_composites, pillar="lsg_protection", "weights")
names(protection_all) <- c("score", "protection_all")
protection_out_camp <- tables_prop_msni(response_with_composites, pillar="lsg_protection", "idp_out_camp", "weights")
names(protection_out_camp) <- c("score", "protection_out_camp")
protection_in_camp <- tables_prop_msni(response_with_composites, pillar="lsg_protection", "idp_in_camp", "weights")
names(protection_in_camp) <- c("score", "protection_in_camp")
protection_returnee <- tables_prop_msni(response_with_composites, pillar="lsg_protection", "returnee", "weights")
names(protection_returnee) <- c("score", "protection_returnee")
protection_host <- tables_prop_msni(response_with_composites, pillar="lsg_protection", "host", "weights")
names(protection_host) <- c("score", "protection_host")
protection<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(protection_all, protection_out_camp, protection_in_camp, 
                                                                   protection_returnee, protection_host))

#HEALTH
health_all <- tables_prop_all(response_with_composites, pillar="lsg_health", "weights")
names(health_all) <- c("score", "health_all")
health_out_camp <- tables_prop_msni(response_with_composites, pillar="lsg_health", "idp_out_camp", "weights")
names(health_out_camp) <- c("score", "health_out_camp")
health_in_camp <- tables_prop_msni(response_with_composites, pillar="lsg_health", "idp_in_camp", "weights")
names(health_in_camp) <- c("score", "health_in_camp")
health_returnee <- tables_prop_msni(response_with_composites, pillar="lsg_health", "returnee", "weights")
names(health_returnee) <- c("score", "health_returnee")
health_host <- tables_prop_msni(response_with_composites, pillar="lsg_health", "host", "weights")
names(health_host) <- c("score", "health_host")
health<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(health_all, health_out_camp, health_in_camp, 
                                                               health_returnee, health_host))

#EDUCATION
education_all <- tables_prop_all(response_with_composites, pillar="lsg_ed", "weights")
names(education_all) <- c("score", "education_all")
education_out_camp <- tables_prop_msni(response_with_composites, pillar="lsg_ed", "idp_out_camp", "weights")
names(education_out_camp) <- c("score", "education_out_camp")
education_in_camp <- tables_prop_msni(response_with_composites, pillar="lsg_ed", "idp_in_camp", "weights")
names(education_in_camp) <- c("score", "education_in_camp")
education_returnee <- tables_prop_msni(response_with_composites, pillar="lsg_ed", "returnee", "weights")
names(education_returnee) <- c("score", "education_returnee")
education_host <- tables_prop_msni(response_with_composites, pillar="lsg_ed", "host", "weights")
names(education_host) <- c("score", "education_host")
education<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(education_all, education_out_camp, education_in_camp, 
                                                                  education_returnee, education_host))

#NFI
nfi_all <- tables_prop_all(response_with_composites, pillar="lsg_snfi", "weights")
names(nfi_all) <- c("score", "nfi_all")
nfi_out_camp <- tables_prop_msni(response_with_composites, pillar="lsg_snfi", "idp_out_camp", "weights")
names(nfi_out_camp) <- c("score", "nfi_out_camp")
nfi_in_camp <- tables_prop_msni(response_with_composites, pillar="lsg_snfi", "idp_in_camp", "weights")
names(nfi_in_camp) <- c("score", "nfi_in_camp")
nfi_returnee <- tables_prop_msni(response_with_composites, pillar="lsg_snfi", "returnee", "weights")
names(nfi_returnee) <- c("score", "nfi_returnee")
nfi_host <- tables_prop_msni(response_with_composites, pillar="lsg_snfi", "host", "weights")
names(nfi_host) <- c("score", "nfi_host")
nfi<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(nfi_all, nfi_out_camp, nfi_in_camp, 
                                                            nfi_returnee, nfi_host))

#FS
fs_all <- tables_prop_all(response_with_composites, pillar="lsg_fs", "weights")
names(fs_all) <- c("score", "fs_all")
fs_out_camp <- tables_prop_msni(response_with_composites, pillar="lsg_fs", "idp_out_camp", "weights")
names(fs_out_camp) <- c("score", "fs_out_camp")
fs_in_camp <- tables_prop_msni(response_with_composites, pillar="lsg_fs", "idp_in_camp", "weights")
names(fs_in_camp) <- c("score", "fs_in_camp")
fs_returnee <- tables_prop_msni(response_with_composites, pillar="lsg_fs", "returnee", "weights")
names(fs_returnee) <- c("score", "fs_returnee")
fs_host <- tables_prop_msni(response_with_composites, pillar="lsg_fs", "host", "weights")
names(fs_host) <- c("score", "fs_host")
fs<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(fs_all, fs_out_camp, fs_in_camp, 
                                                            fs_returnee, fs_host))

#VULNERABILITY
vul_all <- tables_prop_all(response_with_composites, pillar="vulnerability", "weights")
names(vul_all) <- c("score", "vul_all")
vul_out_camp <- tables_prop_msni(response_with_composites, pillar="vulnerability", "idp_out_camp", "weights")
names(vul_out_camp) <- c("score", "vul_out_camp")
vul_in_camp <- tables_prop_msni(response_with_composites, pillar="vulnerability", "idp_in_camp", "weights")
names(vul_in_camp) <- c("score", "vul_in_camp")
vul_returnee <- tables_prop_msni(response_with_composites, pillar="vulnerability", "returnee", "weights")
names(vul_returnee) <- c("score", "vul_returnee")
vul_host <- tables_prop_msni(response_with_composites, pillar="vulnerability", "host", "weights")
names(vul_host) <- c("score", "vul_host")
vul<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(vul_all, vul_out_camp, vul_in_camp, 
                                                            vul_returnee, vul_host))

percent_msni<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(fs, nfi, wash, el, protection, 
                                                                     health, capacity_gap, impact, education, vul))


return(percent_msni)
}
score_pop_group <- percent_msni(response_with_composites)
write.xlsx(score_pop_group, "Output/MSNI/score_pop_group.xlsx") 


#% OF HHs WITH AT LEAST ONE LSG SEVERITY SCORE OF AT LEAST 3
least_3_all <- function(r){
r <-as.data.frame(r[c("population_group", "weights", "lsg_health", "lsg_protection", 
                                                 "lsg_snfi", "lsg_fs", "lsg_el", "lsg_wash", "lsg_ed")])
r$least_3 <- ifelse(r$lsg_health > 2, 1,0)
r$least_3 <- ifelse(r$lsg_protection > 2, 1,r$least_3)
r$least_3 <- ifelse(r$lsg_snfi > 2, 1,r$least_3)
r$least_3 <- ifelse(r$lsg_fs > 2, 1,r$least_3)
r$least_3 <- ifelse(r$lsg_el > 2, 1,r$least_3)
r$least_3 <- ifelse(r$lsg_wash > 2, 1,r$least_3)
r$least_3 <- ifelse(r$lsg_ed > 2, 1,r$least_3)

least_3_all <- tables_prop_all(r, pillar="least_3", "weights")
names(least_3_all) <- c("score", "least_3_all")
return(least_3_all)
}
least_3_all <- least_3_all(response_with_composites)

###############################################################
###############################################################

#% OF HHs WITH LSG SEVERITY SCORE OF AT LEAST 3 BY POP GROUP AND BY NUMBER OF SECTORS
nr_sectors_least_3 <- function(r){
r$lsg_health <- ifelse(r$lsg_health > 2, 1,0)
r$lsg_protection <- ifelse(r$lsg_protection > 2, 1,0)
r$lsg_snfi <- ifelse(r$lsg_snfi > 2, 1,0)
r$lsg_fs <- ifelse(r$lsg_fs > 2, 1,0)
r$lsg_el <- ifelse(r$lsg_el > 2, 1,0)
r$lsg_ed <- ifelse(r$lsg_ed > 2, 1,0)
r$lsg_wash <- ifelse(r$lsg_wash > 2, 1,0)
r$nr_sectors_least_3 <- rowSums(r[,c("lsg_health", "lsg_protection", "lsg_snfi", "lsg_fs", "lsg_el",
                                              "lsg_wash", "lsg_ed")])
nr_sectors_least_3_host <- tables_prop_msni(r, pillar="nr_sectors_least_3", "host", "weights")
names(nr_sectors_least_3_host) <- c("score", "nr_sectors_least_3_host")
nr_sectors_least_3_returnee <- tables_prop_msni(r, pillar="nr_sectors_least_3", "returnee", "weights")
names(nr_sectors_least_3_returnee) <- c("score", "nr_sectors_least_3_returnee")  
nr_sectors_least_3_idp_out_camp <- tables_prop_msni(r, pillar="nr_sectors_least_3", "idp_out_camp", "weights")
names(nr_sectors_least_3_idp_out_camp) <- c("score", "nr_sectors_least_3_idp_out_camp")  
nr_sectors_least_3_idp_in_camp <- tables_prop_msni(r, pillar="nr_sectors_least_3", "idp_in_camp", "weights")
names(nr_sectors_least_3_idp_in_camp) <- c("score", "nr_sectors_least_3_idp_in_camp")  
nr_sectors_least_3<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(nr_sectors_least_3_host, nr_sectors_least_3_returnee,
                                                            nr_sectors_least_3_idp_out_camp, nr_sectors_least_3_idp_in_camp))
return(nr_sectors_least_3)
}
nr_sectors_least_3 <- nr_sectors_least_3(response_with_composites)

#% of HH with sectoral LSG severity score of at least 3, per population group
#RETURNEE
least_3_pillar_popgroup <- function(r){
  r$lsg_health <- ifelse(r$lsg_health > 2, 1,0)
  r$lsg_protection <- ifelse(r$lsg_protection > 2, 1,0)
  r$lsg_snfi <- ifelse(r$lsg_snfi > 2, 1,0)
  r$lsg_fs <- ifelse(r$lsg_fs > 2, 1,0)
  r$lsg_el <- ifelse(r$lsg_el > 2, 1,0)
  r$lsg_wash <- ifelse(r$lsg_wash > 2, 1,0) 
  r$lsg_ed <- ifelse(r$lsg_ed > 2, 1,0) 
  r$vulnerability <- ifelse(r$vulnerability > 2, 1,0) 
  

#Returnees
least_3_returnees_snfi <- tables_prop_msni(r, pillar="lsg_snfi", "returnee", "weights")
names(least_3_returnees_snfi) <- c("score", "least_3_returnees_snfi")
least_3_returnees_fs <- tables_prop_msni(r, pillar="lsg_fs", "returnee", "weights")
names(least_3_returnees_fs) <- c("score", "least_3_returnees_fs")
least_3_returnees_el <- tables_prop_msni(r, pillar="lsg_el", "returnee", "weights")
names(least_3_returnees_el) <- c("score", "least_3_returnees_el")
least_3_returnees_wash <- tables_prop_msni(r, pillar="lsg_wash", "returnee", "weights")
names(least_3_returnees_wash) <- c("score", "least_3_returnees_wash")
least_3_returnees_health <- tables_prop_msni(r, pillar="lsg_health", "returnee", "weights")
names(least_3_returnees_health) <- c("score", "least_3_returnees_health")
least_3_returnees_protection <- tables_prop_msni(r, pillar="lsg_protection", "returnee", "weights")
names(least_3_returnees_protection) <- c("score", "least_3_returnees_protection")
least_3_returnees_ed <- tables_prop_msni(r, pillar="lsg_ed", "returnee", "weights")
names(least_3_returnees_ed) <- c("score", "least_3_returnees_ed")
least_3_returnees_vul <- tables_prop_msni(r, pillar="vulnerability", "returnee", "weights")
names(least_3_returnees_vul) <- c("score", "least_3_returnees_vul")
least_3_returnees<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(least_3_returnees_snfi, least_3_returnees_fs,
                                                                           least_3_returnees_el, least_3_returnees_wash, 
                                                                           least_3_returnees_health, least_3_returnees_protection, 
                                                                          least_3_returnees_ed, least_3_returnees_vul))

#IDP out Camp
least_3_outcamp_snfi <- tables_prop_msni(r, pillar="lsg_snfi", "idp_out_camp", "weights")
names(least_3_outcamp_snfi) <- c("score", "least_3_out_snfi")
least_3_outcamp_fs <- tables_prop_msni(r, pillar="lsg_fs", "idp_out_camp", "weights")
names(least_3_outcamp_fs) <- c("score", "least_3_out_fs")
least_3_outcamp_el <- tables_prop_msni(r, pillar="lsg_el", "idp_out_camp", "weights")
names(least_3_outcamp_el) <- c("score", "least_3_out_el")
least_3_outcamp_wash <- tables_prop_msni(r, pillar="lsg_wash", "idp_out_camp", "weights")
names(least_3_outcamp_wash) <- c("score", "least_3_out_wash")
least_3_outcamp_health <- tables_prop_msni(r, pillar="lsg_health", "idp_out_camp", "weights")
names(least_3_outcamp_health) <- c("score", "least_3_out_health")
least_3_outcamp_protection <- tables_prop_msni(r, pillar="lsg_protection", "idp_out_camp", "weights")
names(least_3_outcamp_protection) <- c("score", "least_3_out_protection")
least_3_outcamp_ed <- tables_prop_msni(r, pillar="lsg_ed", "idp_out_camp", "weights")
names(least_3_outcamp_ed) <- c("score", "least_3_outcamp_ed")
least_3_outcamp_vul <- tables_prop_msni(r, pillar="vulnerability", "idp_out_camp", "weights")
names(least_3_outcamp_vul) <- c("score", "least_3_outcamp_vul")
least_3_outcamp<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(least_3_outcamp_snfi, least_3_outcamp_fs,
                                                                          least_3_outcamp_el, least_3_outcamp_wash, 
                                                                          least_3_outcamp_health, least_3_outcamp_protection, 
                                                                        least_3_outcamp_ed, least_3_outcamp_vul))

#IDP Out of camp Camp
least_3_incamp_snfi <- tables_prop_msni(r, pillar="lsg_snfi", "idp_in_camp", "weights")
names(least_3_incamp_snfi) <- c("score", "least_3_in_snfi")
least_3_incamp_fs <- tables_prop_msni(r, pillar="lsg_fs", "idp_in_camp", "weights")
names(least_3_incamp_fs) <- c("score", "least_3_in_fs")
least_3_incamp_el <- tables_prop_msni(r, pillar="lsg_el", "idp_in_camp", "weights")
names(least_3_incamp_el) <- c("score", "least_3_in_el")
least_3_incamp_wash <- tables_prop_msni(r, pillar="lsg_wash", "idp_in_camp", "weights")
names(least_3_incamp_wash) <- c("score", "least_3_in_wash")
least_3_incamp_health <- tables_prop_msni(r, pillar="lsg_health", "idp_in_camp", "weights")
names(least_3_incamp_health) <- c("score", "least_3_in_health")
least_3_incamp_protection <- tables_prop_msni(r, pillar="lsg_protection", "idp_in_camp", "weights")
names(least_3_incamp_protection) <- c("score", "least_3_in_protection")
least_3_incamp_ed <- tables_prop_msni(r, pillar="lsg_ed", "idp_in_camp", "weights")
names(least_3_incamp_ed) <- c("score", "least_3_incamp_ed")
least_3_incamp_vul <- tables_prop_msni(r, pillar="vulnerability", "idp_in_camp", "weights")
names(least_3_incamp_vul) <- c("score", "least_3_incamp_vul")
least_3_incamp<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(least_3_incamp_snfi, least_3_incamp_fs,
                                                                       least_3_incamp_el, least_3_incamp_wash, 
                                                                       least_3_incamp_health, least_3_incamp_protection, 
                                                                       least_3_incamp_ed, least_3_incamp_vul))

#Host
least_3_host_snfi <- tables_prop_msni(r, pillar="lsg_snfi", "host", "weights")
names(least_3_host_snfi) <- c("score", "least_3_host_snfi")
least_3_host_fs <- tables_prop_msni(r, pillar="lsg_fs", "host", "weights")
names(least_3_host_fs) <- c("score", "least_3_host_fs")
least_3_host_el <- tables_prop_msni(r, pillar="lsg_el", "host", "weights")
names(least_3_host_el) <- c("score", "least_3_host_el")
least_3_host_wash <- tables_prop_msni(r, pillar="lsg_wash", "host", "weights")
names(least_3_host_wash) <- c("score", "least_3_host_wash")
least_3_host_health <- tables_prop_msni(r, pillar="lsg_health", "host", "weights")
names(least_3_host_health) <- c("score", "least_3_host_health")
least_3_host_protection <- tables_prop_msni(r, pillar="lsg_protection", "host", "weights")
names(least_3_host_protection) <- c("score", "least_3_host_protection")
least_3_host_ed <- tables_prop_msni(r, pillar="lsg_ed", "host", "weights")
names(least_3_host_ed) <- c("score", "least_3_host_ed")
least_3_host_vul <- tables_prop_msni(r, pillar="vulnerability", "host", "weights")
names(least_3_host_vul) <- c("score", "least_3_host_vul")
least_3_host<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(least_3_host_snfi, least_3_host_fs,
                                                                       least_3_host_el, least_3_host_wash, 
                                                                       least_3_host_health, least_3_host_protection, 
                                                                     least_3_host_ed, least_3_host_vul))

least_3_all_pop_groups<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(least_3_host, least_3_returnees, 
                                                                               least_3_incamp, least_3_outcamp))
return(least_3_all_pop_groups)
}
least_3_all_pop_groups <- least_3_pillar_popgroup(response_with_composites)
write.csv(least_3_all_pop_groups, "Output/MSNI/least_3_all_pop_groups.csv")

#Most common combinations of scores > 3 in two 
combination_pillars_least_3 <- function(r){
  
r$wash_shelter_combination <- ifelse(r$lsg_wash >2 & r$lsg_snfi > 2, 1,0)
r$education_shelter_combination <- ifelse(r$lsg_ed >2 & r$lsg_snfi > 2, 1,0)
r$livelihoods_shelter_combination <- ifelse(r$lsg_el >2 & r$lsg_snfi > 2, 1,0)
r$health_shelter_combination <- ifelse(r$lsg_health >2 & r$lsg_snfi > 2, 1,0)
r$education_wash_combination <- ifelse(r$lsg_wash >2 & r$lsg_ed > 2, 1,0)
r$education_protection_combination <- ifelse(r$lsg_protection >2 & r$lsg_ed > 2, 1,0)
r$health_wash_combination <- ifelse(r$lsg_health >2 & r$lsg_wash > 2, 1,0)
r$health_education_combination <- ifelse(r$lsg_health >2 & r$lsg_ed > 2, 1,0)
r$livelihoods_wash_combination <- ifelse(r$lsg_el >2 & r$lsg_wash > 2, 1,0)
r$protection_shelter_combination <- ifelse(r$lsg_snfi >2 & r$lsg_protection > 2, 1,0)
r$fs_livelihoods_combination <- ifelse(r$lsg_fs >2 & r$lsg_el > 2, 1,0)
r$health_livelihoods_combination <- ifelse(r$lsg_health >2 & r$lsg_el > 2, 1,0)
r$protection_livelihoods_combination <- ifelse(r$lsg_protection >2 & r$lsg_el > 2, 1,0)
r$education_livelihoods_combination <- ifelse(r$lsg_ed >2 & r$lsg_el > 2, 1,0)


wash_shelter_combination <- tables_prop_all(r, pillar="wash_shelter_combination", "weights")
names(wash_shelter_combination) <- c("score", "wash_shelter_combination")
education_shelter_combination <- tables_prop_all(r, pillar="education_shelter_combination", "weights")
names(education_shelter_combination) <- c("score", "education_shelter_combination")
livelihoods_shelter_combination <- tables_prop_all(r, pillar="livelihoods_shelter_combination", "weights")
names(livelihoods_shelter_combination) <- c("score", "livelihoods_shelter_combination")
health_shelter_combination <- tables_prop_all(r, pillar="health_shelter_combination", "weights")
names(health_shelter_combination) <- c("score", "health_shelter_combination")
education_wash_combination <- tables_prop_all(r, pillar="education_wash_combination", "weights")
names(education_wash_combination) <- c("score", "education_wash_combination")
education_protection_combination <- tables_prop_all(r, pillar="education_protection_combination", "weights")
names(education_protection_combination) <- c("score", "education_protection_combination")
health_wash_combination <- tables_prop_all(r, pillar="health_wash_combination", "weights")
names(health_wash_combination) <- c("score", "health_wash_combination")
health_education_combination <- tables_prop_all(r, pillar="health_education_combination", "weights")
names(health_education_combination) <- c("score", "health_education_combination")
livelihoods_wash_combination <- tables_prop_all(r, pillar="livelihoods_wash_combination", "weights")
names(livelihoods_wash_combination) <- c("score", "livelihoods_wash_combination")
protection_shelter_combination <- tables_prop_all(r, pillar="protection_shelter_combination", "weights")
names(protection_shelter_combination) <- c("score", "protection_shelter_combination")
fs_livelihoods_combination <- tables_prop_all(r, pillar="fs_livelihoods_combination", "weights")
names(fs_livelihoods_combination) <- c("score", "fs_livelihoods_combination")
health_livelihoods_combination <- tables_prop_all(r, pillar="health_livelihoods_combination", "weights")
names(health_livelihoods_combination) <- c("score", "health_livelihoods_combination")
protection_livelihoods_combination <- tables_prop_all(r, pillar="protection_livelihoods_combination", "weights")
names(protection_livelihoods_combination) <- c("score", "protection_livelihoods_combination")
education_livelihoods_combination <- tables_prop_all(r, pillar="education_livelihoods_combination", "weights")
names(education_livelihoods_combination) <- c("score", "education_livelihoods_combination")

combination_pillars_least_3<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(wash_shelter_combination, education_shelter_combination,
                                                                                    livelihoods_shelter_combination, health_shelter_combination,
                                                                                    education_wash_combination, education_protection_combination,
                                                                                    health_wash_combination, health_education_combination,
                                                                                    livelihoods_wash_combination, protection_shelter_combination,
                                                                                    fs_livelihoods_combination, health_livelihoods_combination,
                                                                                    protection_livelihoods_combination, education_livelihoods_combination))

return(combination_pillars_least_3)
}
combination_pillars_least_3 <- combination_pillars_least_3(response_with_composites)



#CALCULATE FOUR PERCENTAGES FOR COMBINATIONS OF LSG AND CG SCORES
#Calculate the percentage of HHs with at least one LSG severity score of 3 or a CG score of at least 3
lsg_cg_combinations <- function(r){
  
r$lsg_or_cg <- ifelse(response_with_composites$lsg_ed >2 | r$lsg_el > 2 | 
                                               r$lsg_fs > 2 | r$lsg_health > 2 | 
                                               r$lsg_protection > 2 | r$lsg_snfi > 2 |
                                               r$lsg_wash > 2 | r$capacity_gap > 2, 1, 0)

lsg_or_cg <- tables_prop_all(r, pillar="lsg_or_cg", "weights")
names(lsg_or_cg) <- c("score", "1_lsg_or_cg")


#Calculate % of HHs with at least one LSG severity score of at least 3 but a GC score lower than 3

  r$lsg_but_no_cg <- ifelse(response_with_composites$lsg_ed >2 | r$lsg_el > 2 | 
                          r$lsg_fs > 2 | r$lsg_health > 2 | 
                          r$lsg_protection > 2 | r$lsg_snfi > 2 |
                          r$lsg_wash > 2 & r$capacity_gap < 3, 1, 0)
  
  lsg_but_no_cg <- tables_prop_all(r, pillar="lsg_but_no_cg", "weights")
  names(lsg_but_no_cg) <- c("score", "2_lsg_but_no_cg")
  


#Calculate % of HHs with at least one LSG severity score of at least 3 and a GC score higher than 3
  
  r$lsg_and_cg <- ifelse(response_with_composites$lsg_ed >2 | r$lsg_el > 2 | 
                              r$lsg_fs > 2 | r$lsg_health > 2 | 
                              r$lsg_protection > 2 | r$lsg_snfi > 2 |
                              r$lsg_wash > 2 & r$capacity_gap > 2, 1, 0)
  
  lsg_and_cg <- tables_prop_all(r, pillar="lsg_and_cg", "weights")
  names(lsg_and_cg) <- c("score", "3_lsg_and_cg")
  

#Calculate % of HHs with all LSG severity scores lower than 3 but a GC score higher than 3
  
  r$lsg_all_lower_but_cg <- ifelse(response_with_composites$lsg_ed < 3 & r$lsg_el < 3 & 
                           r$lsg_fs < 3 & r$lsg_health < 3 & 
                           r$lsg_protection < 3 & r$lsg_snfi < 3 &
                           r$lsg_wash < 3 & r$capacity_gap > 2, 1, 0)
  
  lsg_all_lower_but_cg <- tables_prop_all(r, pillar="lsg_all_lower_but_cg", "weights")
  names(lsg_all_lower_but_cg) <- c("score", "4_lsg_all_lower_but_cg")
  
  lsg_cg_combinations<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(lsg_all_lower_but_cg, lsg_and_cg, 
                                                                              lsg_but_no_cg, lsg_or_cg))
  
  return(lsg_cg_combinations)
}
lsg_cg_combinations <- lsg_cg_combinations(response_with_composites)

multi_sectoral_msni<-Reduce(function(x,y) merge(x,y,by="score",all=T) ,list(least_3_all, nr_sectors_least_3, 
                                                                               least_3_all_pop_groups, combination_pillars_least_3,
                                                                            lsg_cg_combinations))

write.xlsx(multi_sectoral_msni, "Output/MSNI/multi_sectoral_msni_factsheet.xlsx")
