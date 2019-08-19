recoding_mcna <- function(r, loop) {
  loop_hoh <- loop[which(loop$relationship == "head"),]
  loop_children <- loop[which(loop$age < 18),]
  loop_females <- loop[which(loop$sex == "female"),]
  r <- r %>% 
    new_recoding(source=why_not_return, target=g73) %>% 
    recode_to(1, where=why_not_return.presence_of_mines == 1) %>% 
    recode_to(0, where=why_not_return.presence_of_mines == 0) %>%
    
    new_recoding(source=hh_dispute, target=g68) %>% 
    recode_to(to = 1, where.selected.exactly = "yes",
              otherwise.to = 0) %>% 
    
    new_recoding(target=g51) %>% 
    recode_to(1, where = pds == "no" | info_card == "no" | death_certificate == "no" | guardianship == "no" | inheritance == "no" | 
                trusteeship == "no" | passport_a18 == "no" | id_card_a18 == "no" | citizenship_a18 == "no" | birth_cert_a18 == "no" |
                school_cert_a18 == "non_valid" | marriage_cert_a18 == "non_valid" | divorce_cert_a18 == "non_valid" |
                passport_u18 == "no" | id_card_u18 == "no" | citizenship_u18 == "no" | birth_cert_u18 == "no" | 
                marriage_cert_u18 == "non_valid" | divorce_cert_u18 == "non_valid") %>% 
    recode_to(0, where = ! (pds == "no" | info_card == "no" | death_certificate == "no" | guardianship == "no" | inheritance == "no" | 
                              trusteeship == "no" | passport_a18 == "no" | id_card_a18 == "no" | citizenship_a18 == "no" | birth_cert_a18 == "no" |
                              school_cert_a18 == "non_valid" | marriage_cert_a18 == "non_valid" | divorce_cert_a18 == "non_valid" |
                              passport_u18 == "no" | id_card_u18 == "no" | citizenship_u18 == "no" | birth_cert_u18 == "no" | 
                              marriage_cert_u18 == "non_valid" | divorce_cert_u18 == "non_valid")) %>%  
    
    new_recoding(target=g51_sub) %>% 
    recode_to(1, where = passport_u18 == "no" | id_card_u18 == "no" | citizenship_u18 == "no" | birth_cert_u18 == "no" | 
                marriage_cert_u18 == "non_valid" | divorce_cert_u18 == "non_valid") %>% 
    recode_to(0, where = ! (passport_u18 == "no" | id_card_u18 == "no" | citizenship_u18 == "no" | birth_cert_u18 == "no" | 
                              marriage_cert_u18 == "non_valid" | divorce_cert_u18 == "non_valid")) %>% 
    
    new_recoding(target=g54) %>% 
    recode_to(1, where = restriction_clearance == "yes" | restriction_documents == "yes" | restriction_time == "yes" | 
                restriction_reason == "yes" | restriction_physical == "yes" | restriction_other == "yes") %>% 
    recode_to(0, where = !(restriction_clearance == "yes" | restriction_documents == "yes" | restriction_time == "yes" | 
                             restriction_reason == "yes" | restriction_physical == "yes" | restriction_other == "yes")) %>% 
    
    new_recoding(target=g63, source = unsafe_areas) %>% 
    recode_to(0, where.selected.exactly = "none",
              otherwise.to = 1) %>% 
    
    
    end_recoding
  r$g51_alt <- apply(r, 1, FUN=function(x){#print(which(r$X_uuid==x[,"X_uuid"])) 
                                    sum(x[c("pds","info_card","death_certificate", "guardianship", "inheritance",
                                       "trusteeship", "passport_a18", "id_card_a18", "citizenship_a18", "birth_cert_a18",
                                       "school_cert_a18", "marriage_cert_a18", "divorce_cert_a18",
                                       "passport_u18", "id_card_u18", "citizenship_u18", "birth_cert_u18",
                                       "marriage_cert_u18", "divorce_cert_u18")] %in% c("no", "non_valid"))})
  r$g51a <- r$birth_cert_missing_amount_u1
  r$g51b <- r$id_card_missing_amount
  r$a7 <- r$num_hh_member
  r$a8 <- r$num_family_member
  r$a9_male <- r$tot_male / (r$tot_male + r$tot_female)
  r$a9_female <- r$tot_female / (r$tot_male + r$tot_female)
  r$a10_child <- r$tot_child / (r$tot_male + r$tot_female)
  r$a10_adult <- (r$male_18_59_calc + r$female_18_59_calc) / (r$tot_male + r$tot_female)
  r$a10_elderly <- (r$male_60_calc + r$female_60_calc) / (r$tot_male + r$tot_female)
  r$a11 <- ifelse(loop_hoh[match(r$X_uuid,loop_hoh$X_submission__uuid), "marital_status"] %in%
          c("single", "separated", "widowed", "divorced"), 1, 
          ifelse(loop_hoh[match(r$X_uuid,loop_hoh$X_submission__uuid), "marital_status"] %in%
    c(NA, ""), NA, 0))
  r$a12 <- ifelse(loop_children[match(r$X_uuid,loop_children$X_submission__uuid), "marital_status"] %in%
                    c("married"), 1, 0)
  loop_females <- loop_females %>% mutate(plw = ifelse(pregnant_lactating == "yes", 1, 0))
  PLW <- as.data.frame(loop_females %>% group_by(X_submission__uuid) %>% summarize(sum(plw)))
  r$c11 <- PLW[match(r$X_uuid, PLW$X_submission__uuid),2]
  r$g35 <- apply(r, 1, FUN=function(x){
    ifelse(sum(loop$health_issue.chronic[which(loop$X_submission__uuid == x["X_uuid"])], na.rm = T) > 0, 1, 0)
  })
  r$c3 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop$difficulty_seeing[which(loop$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop$difficulty_hearing[which(loop$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop$difficulty_walking[which(loop$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop$difficulty_remembering[which(loop$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop$difficulty_washing[which(loop$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop$difficulty_communicating[which(loop$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")), 1, 0)
  })
  r$c2 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop$disab_explos[which(loop$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 
           ifelse(all(loop$disab_explos[which(loop$X_submission__uuid == x["X_uuid"])] == ""), NA, 0))
  })
  r$c9 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop$difficulty_accessing_services[which(loop$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 0)
  })
  r$g4 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop$attend_formal_ed[which(loop$X_submission__uuid == x["X_uuid"])] == "no" & 
                 loop$attend_informal_ed[which(loop$X_submission__uuid == x["X_uuid"])] == "no"), 1, 0)
  })
  r$g6 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop$ever_attend[which(loop$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 0)
  })
  r$a13 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop$age[which(loop$X_submission__uuid == x["X_uuid"])] < 18 & 
                 loop$work[which(loop$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 0)
  })
  r$g44 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop$age[which(loop$X_submission__uuid == x["X_uuid"])] > 17 & 
                 loop$work[which(loop$X_submission__uuid == x["X_uuid"])] == "no" & 
                 loop$actively_seek_work[which(loop$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 0)
  })
  r$g7_i <- ifelse(r$reasons_not_attend.cannot_afford %in% c(NA, 0), 0, 1)
  r$g7_ii <- ifelse(r$reasons_not_attend.school_closed %in% c(NA, 0), 0, 1)
  r$g7_iii <- ifelse(r$reasons_not_attend.not_safe %in% c(NA, 0), 0, 1)
  r$g7_iv <- ifelse(r$reasons_not_attend.impossible_to_enrol %in% c(NA, 0), 0, 1)
  r$g7_v <- ifelse(r$reasons_not_attend.cannot_go_physically %in% c(NA, 0), 0, 1)
  r$g7_vi <- ifelse(r$reasons_not_attend.overcrowded %in% c(NA, 0), 0, 1)
  r$g7_vii <- ifelse(r$reasons_not_attend.lack_of_staff %in% c(NA, 0), 0, 1)
  r$g7_viii <- ifelse(r$reasons_not_attend.poor_infrastructure %in% c(NA, 0), 0, 1)
  r$g7_ix <- ifelse(r$reasons_not_attend.curriculum %in% c(NA, 0), 0, 1)
  r$g7_x <- ifelse(r$reasons_not_attend.children_working %in% c(NA, 0), 0, 1)
  r$g7_xi <- ifelse(r$reasons_not_attend.parental_refusal %in% c(NA, 0), 0, 1)
  r$g7_xii <- ifelse(r$reasons_not_attend.uninterested %in% c(NA, 0), 0, 1)
  r$g7a <- ifelse(r$no_school_no_docs == "yes", 1, 0)
  r$g45_i <- ifelse(r$employment_primary_barriers.increased_competition == 1, 1, 0)
  r$g45_ii <- ifelse(r$employment_primary_barriers.jobs_far == 1, 1, 0)
  r$g45_iii <- ifelse(r$employment_primary_barriers.only_low_available == 1, 1, 0)
  r$g45_iv <- ifelse(r$employment_primary_barriers.underqualified_for_jobs == 1, 1, 0)
  r$g45_v <- ifelse(r$employment_primary_barriers.lack_of_connections == 1, 1, 0)
  r$g45_vi <- ifelse(r$employment_primary_barriers.none == 1, 1, 0)
  
  # CARI calculation
  r$fcs <- r$cereals * 2 + r$nuts_seed * 2 + r$milk_dairy * 4 + r$meat * 4 + 
    r$vegetables + r$fruits + r$oil_fats * 0.5 + r$sweets * 0.5
  r$fcs_score <- ifelse(r$fcs <= 21, 1, ifelse(r$fcs <= 35, 2, 3))
  # this method for coping strategy group is based on MCNA food security report from Afganistan 2017
  r$cs_score <- r$cheaper_quality + r$borrowing * 2 + r$reduce_meals + r$less_food + r$less_adult * 3
  r$cs_group <- ifelse(r$cs_score < 10, 3, ifelse(r$cs_score < 18, 2, 1))
  
  r$cari <- ifelse((r$fcs_score == 3 & r$cs_group %in% c(2,3)) | (r$fcs_score == 2 & r$cs_group == 3), 1,
                  ifelse((r$fcs_score == 1 & r$cs_group %in% c(1,2)) | (r$fcs_score == 2 & r$cs_group == 1), 3, 2))
  r$g14 <- ifelse(r$cari > 1, 1, 0)
  # food expenditure share, 1 for everything above 50% (medium, high and very high)
  r$fes <- r$food_exp / r$tot_expenditure
  r$g14a <- ifelse(r$fes > 0.5, 1, 0)
  # end of CARI
  r$b5 <- ifelse(r$selling_assets %in% c("no_already_did", "yes") |
                    r$borrow_debt  %in% c("no_already_did", "yes") |
                    r$reduce_spending %in% c("no_already_did", "yes"), 1, 0)
  r$b6 <- ifelse(r$selling_transportation_means %in% c("no_already_did", "yes") |
                   r$change_place  %in% c("no_already_did", "yes") |
                   r$child_work %in% c("no_already_did", "yes"), 1, 0)
  r$b7 <- ifelse(r$child_dropout_school %in% c("no_already_did", "yes") |
                   r$adult_risky  %in% c("no_already_did", "yes") |
                   r$family_migrating %in% c("no_already_did", "yes") |
                   r$child_forced_marriage %in% c("no_already_did", "yes"), 1, 0)
  r$g25 <- ifelse(r$distance_health_service %in% c("between_2km_5km", "within_2km"), 1, 0)
  r$g26 <- ifelse(r$distance_hospital %in% c("between_2km_5km", "between_6km_10km", "within_2km") &
                    r$hospital_emergency_ser == "yes" &
                    r$hospital_maternity_ser == "yes" &
                    r$hospital_surgical_ser == "yes" &
                    r$hospital_pediatric_ser == "yes", 1, 0)
  r$g32 <- ifelse(r$women_specialised_services == "yes", 1, 0)
  r$g56 <- ifelse(r$child_distress_number < 1 | is.na(r$child_distress_number), 0, 1)
  r$g57 <- ifelse(r$adult_distress_number < 1 | is.na(r$adult_distress_number), 0, 1)
  r$g34_i <- ifelse(r$health_barriers.civ_docs_problems %in% c(NA, 0), 0, 1)
  r$g34_ii <- ifelse(r$health_barriers.cost %in% c(NA, 0), 0, 1)
  r$g34_iii <- ifelse(r$health_barriers.unqualified_staff %in% c(NA, 0), 0, 1)
  r$g34_iv <- ifelse(r$health_barriers.refused_treatment %in% c(NA, 0), 0, 1)
  r$g34_v <- ifelse(r$health_barriers.no_medicine %in% c(NA, 0), 0, 1)
  r$g34_vi <- ifelse(r$health_barriers.not_inclusive %in% c(NA, 0), 0, 1)
  r$g34_vii <- ifelse(r$health_barriers.no_offered_treatment %in% c(NA, 0), 0, 1)
  r$g34_viii <- ifelse(r$health_barriers.no_referral_phc %in% c(NA, 0), 0, 1)
  r$g34_ix <- ifelse(r$health_barriers.phc_closed %in% c(NA, 0), 0, 1)
  r$g34_x <- ifelse(r$health_barriers.distance_to_treatmentcenter %in% c(NA, 0), 0, 1)
  r$g34_xi <- ifelse(r$health_barriers.not_access %in% c(NA, 0), 0, 1)
  r$g95 <- ifelse(rowSums(r[, c("drinking_water_source.network_private", "drinking_water_source.network_comm",
                      "drinking_water_source.borehole", "drinking_water_source.prot_spring",
                      "drinking_water_source.prot_well", "drinking_water_source.prot_tank",
                      "drinking_water_source.bottled_water")], na.rm = T) > 0, 1, 0)
  r$g94 <- ifelse(r$tank_capacity * r$refill_times / r$people_share_tank / 7 > 50, 1, 0)
  r$g96 <- ifelse(r$treat_drink_water_how == "not_necessary", 0, 1)
  r$g97 <- ifelse(rowSums(r[, c("latrines.flush", "latrines.vip_pit")], na.rm = T) > 0, 1, 0)
  r$g99 <- ifelse(r$access_hygiene_items == "yes" & r$use_of_soap.handwashing == 1, 1, 0)
  
  table(r$g99, useNA = "always")
  
  return(r)
}
