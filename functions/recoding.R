round2 = function(x, n=0) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

recoding_preliminary <- function(r, loop) {
  loop_hoh <- loop[which(loop$relationship == "head"),]
  loop_children <- loop[which(loop$age < 18),]
  loop_females <- loop[which(loop$sex == "female"),]
  loop_females <- loop_females %>% mutate(plw = ifelse(pregnant_lactating == "yes", 1, 0))
  r_female_headed <- r[which(r$X_uuid %in% loop$X_submission__uuid[which(loop$sex == "female" & loop$relationship == "head")]),]
  r$gender_hhh <- loop_hoh$sex[match(r$X_uuid, loop_hoh$X_submission__uuid)]
  r$f_hhh <- ifelse(r$gender_hhh == "female", 1, 0)
  
  response$tot_income[which(response$tot_income == 88)] <- NA
  response$inc_employment[which(response$inc_employment == 88)] <- NA
  response$inc_remittances[which(response$inc_remittances == 88)] <- NA
  response$inc_humaid[which(response$inc_humaid == 88)] <- NA
  response$inc_debt[which(response$inc_debt == 88)] <- NA
  response$inc_pension[which(response$inc_pension == 88)] <- NA
  response$inc_selling_assets[which(response$inc_selling_assets == 88)] <- NA
  response$inc_momd[which(response$inc_momd == 88)] <- NA
  response$inc_molsa[which(response$inc_molsa == 88)] <- NA
  response$inc_other_safety[which(response$inc_other_safety == 88)] <- NA
  response$inc_rent[which(response$inc_rent == 88)] <- NA
  
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
  r$a12 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop_children$marital_status[which(loop_children$X_submission__uuid == x["X_uuid"])] == "married"), 1, 0)
  })
  PLW <- as.data.frame(loop_females %>% dplyr::group_by(X_submission__uuid) %>% dplyr::summarize(sum(plw)))
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
  r$a16 <- ifelse(r$shelter_type %in% c("unfinished_abandoned_building", "damaged_building", "tent", 
                                        "religious_building", "public_building", "non_residential", 
                                        "container", "makeshift_shelter"), 1, 0)
  r$a17 <- ifelse(r$hh_hosted == "yes", 1, 0)
  r$g64 <- ifelse(r$hh_risk_eviction == "yes", 1, 0)
  r$g65_i <- ifelse(r$hh_main_risks.authorities_request == 1, 1, 0)
  r$g65_ii <- ifelse(r$hh_main_risks.lack_funds == 1, 1, 0)
  r$g65_iii <- ifelse(r$hh_main_risks.no_longer_hosted == 1, 1, 0)
  r$g65_iv <- ifelse(r$hh_main_risks.unaccepted_by_community == 1, 1, 0)
  r$g65_v <- ifelse(r$hh_main_risks.owner_request == 1, 1, 0)
  r$g65_vi <- ifelse(r$hh_main_risks.no_agreement == 1, 1, 0)
  r$g65_vii <- ifelse(r$hh_main_risks.inadequate == 1, 1, 0)
  r$g65_viii <- ifelse(r$hh_main_risks.occupied == 1, 1, 0)
  r$g65_ix <- ifelse(r$hh_main_risks.confiscation == 1, 1, 0)
  r$g65_x <- ifelse(r$hh_main_risks.dispute == 1, 1, 0)
  r$g90_i <- ifelse(r$shelter_issues.none == 1, 1, 0)
  r$g90_ii <- ifelse(r$shelter_issues.uxo == 1, 1, 0)
  r$g90_iii <- ifelse(r$shelter_issues.flood_landslide == 1, 1, 0)
  r$g90_iv <- ifelse(r$shelter_issues.waste_dumping == 1, 1, 0)
  r$g90_v <- ifelse(r$shelter_issues.fire == 1, 1, 0)
  r$g90_vi <- ifelse(r$shelter_issues.insicure_isolated == 1, 1, 0)
  r$g90_vii <- ifelse(r$shelter_issues.not_solid == 1, 1, 0)
  r$g90_viii <- ifelse(r$shelter_issues.no_fence == 1, 1, 0)
  r$g90_ix <- ifelse(r$shelter_issues.no_tenure == 1, 1, 0)
  r$g90_x <- ifelse(r$shelter_issues.no_separated_rooms == 1, 1, 0)
  r$g90_xi <- ifelse(r$shelter_issues.no_space == 1, 1, 0)
  r$g90_xii <- ifelse(r$shelter_issues.low_high_ceilings == 1, 1, 0)
  r$g90_xiii <- ifelse(r$shelter_issues.no_light == 1, 1, 0)
  r$g90_xiv <- ifelse(r$shelter_issues.leaking == 1, 1, 0)
  r$g90_xv <- ifelse(r$shelter_issues.insulation == 1, 1, 0)
  r$g90_xvi <- ifelse(r$shelter_issues.openings_walls == 1, 1, 0)
  r$g90_xvii <- ifelse(r$shelter_issues.broken_windows == 1, 1, 0)
  r$g90_xviii <- ifelse(r$shelter_issues.no_ventilation == 1, 1, 0)
  r$g90_xix <- ifelse(r$shelter_issues.no_electrical == 1, 1, 0)
  r$g90_xx <- ifelse(r$shelter_issues.missing_washing == 1, 1, 0)
  r$g90_xxi <- ifelse(r$shelter_issues.missing_cooking == 1, 1, 0)
  r$g90_xxii <- ifelse(r$shelter_issues.bends_structure == 1, 1, 0)
  r$g85 <- ifelse(rowSums(r[,c("nfi_priority_needs.bedding_items","nfi_priority_needs.mattresses_sleeping_mats",
                               "nfi_priority_needs.blankets", "nfi_priority_needs.cooking_utensils_kitchen_set",
                               "nfi_priority_needs.cooking_stove", "nfi_priority_needs.source_of_light",
                               "nfi_priority_needs.fuel_storage")]) > 0, 1, 0)
  r$g89 <- ifelse(rowSums(r[,c("shelter_better.improve_infrastructure", "shelter_better.improve_privacy",
                               "shelter_better.improve_safety", "shelter_better.improve_structure",
                               "shelter_better.improve_tenure", "shelter_better.protec_hazards",
                               "shelter_better.protect_climate")]) > 1, 1, 0)
  r$g8 <- ifelse(r$primary_school_place %in% c("between_2_5", "within_2km") &
                   r$secondary_school_place %in% c("between_2_5", "within_2km"), 1, 0)
  r$b1 <- ifelse(rowSums(r[, c("inc_employment", "inc_pension")], na.rm=T) < 480000, 1, 0)
  r$b3 <- NA
  r$b3[which(r$X_uuid %in% r_female_headed$X_uuid)] <- 
    ifelse(rowSums(r_female_headed[, c("inc_employment", "inc_pension")], na.rm=T) < 480000, 1, 0)
  r$b2 <- ifelse(r$primary_livelihood.ngo_charity_assistance == 1, 1, 0)
  r$g46 <- ifelse(r$employment_seasonal == "yes", 1, 0)
  r$g100 <- r$tot_expenditure / r$tot_income
  r$g37 <- ifelse(r$how_much_debt > 505000, 1, 0)
  r$g38 <- ifelse(r$reasons_for_debt %in% c("basic_hh_expenditure", "education", "food", "health"), 1, 0)
  r$g41 <- ifelse(r$market_place %in% c("within_2km", "within_5km"), 1, 0)
  r$f5 <- ifelse(r$previously_evicted == "yes", 1, 0)
  r$f6 <- ifelse(r$own_land == "yes", 1, 0)
  r$f7 <- ifelse(r$applied_compensation == "yes", ifelse(r$received_compensation == "no", 1, 0), NA)
  r$f7b <- ifelse(r$complaint_mechanisms == "yes", 1, ifelse(r$complaint_mechanisms %in% c("do_not_know", "no"), 0, NA))
  r$f9 <- ifelse(r$family_separated_reunification == "yes", 1, 0)
  r$f10 <- ifelse(r$voting_eligibility == "no", 1, 0)
  r$d1_i <- ifelse(r$info_aid.aid == 1, 1, 0)
  r$d1_ii <- ifelse(r$info_aid.safety == 1, 1, 0)
  r$d1_iii <- ifelse(r$info_aid.housing == 1, 1, 0)
  r$d1_iv <- ifelse(r$info_aid.livelihoods == 1, 1, 0)
  r$d1_v <- ifelse(r$info_aid.water == 1, 1, 0)
  r$d1_vi <- ifelse(r$info_aid.electricity == 1, 1, 0)
  r$d1_vii <- ifelse(r$info_aid.education == 1, 1, 0)
  r$d1_viii <- ifelse(r$info_aid.healthcare == 1, 1, 0)
  r$d1_ix <- ifelse(r$info_aid.legal == 1, 1, 0)
  r$d1_x <- ifelse(r$info_aid.property == 1, 1, 0)
  r$d1_xi <- ifelse(r$info_aid.uxo == 1, 1, 0)
  r$d1_xii <- ifelse(r$info_aid.documentation == 1, 1, 0)
  r$d1_xiii <- ifelse(r$info_aid.none == 1, 1, 0)
  r$d2_i <- ifelse(r$info_provider.ngo == 1, 1, 0)
  r$d2_ii <- ifelse(r$info_provider.friends_in_aoo == 1, 1, 0)
  r$d2_iii <- ifelse(r$info_provider.friends_visited_aoo == 1, 1, 0)
  r$d2_iv <- ifelse(r$info_provider.friends_not_been_in_aoo == 1, 1, 0)
  r$d2_v <- ifelse(r$info_provider.local_authorities == 1, 1, 0)
  r$d2_vi <- ifelse(r$info_provider.national_authorities == 1, 1, 0)
  r$d2_vii <- ifelse(r$info_provider.religious == 1, 1, 0)
  r$d2_viii <- ifelse(r$info_provider.mukhtars == 1, 1, 0)
  r$d2_ix <- ifelse(r$info_provider.sector_leaders == 1, 1, 0)
  r$d2_x <- ifelse(r$info_provider.schools == 1, 1, 0)
  r$d3_i <- ifelse(r$info_mode.mobile == 1, 1, 0)
  r$d3_ii <- ifelse(r$info_mode.direct_obs == 1, 1, 0)
  r$d3_iii <- ifelse(r$info_mode.face_cmmunic == 1, 1, 0)
  r$d3_iv <- ifelse(r$info_mode.television == 1, 1, 0)
  r$d3_v <- ifelse(r$info_mode.telephone == 1, 1, 0)
  r$d3_vi <- ifelse(r$info_mode.facebook_app == 1, 1, 0)
  r$d3_vii <- ifelse(r$info_mode.facebook_messenger == 1, 1, 0)
  r$d3_viii <- ifelse(r$info_mode.whatsapp == 1, 1, 0)
  r$d3_ix <- ifelse(r$info_mode.viber == 1, 1, 0)
  r$d3_x <- ifelse(r$info_mode.other_social == 1, 1, 0)
  r$d3_xi <- ifelse(r$info_mode.notice_board == 1, 1, 0)
  r$d3_xii <- ifelse(r$info_mode.newspapers == 1, 1, 0)
  r$d3_xiii <- ifelse(r$info_mode.leaflet == 1, 1, 0)
  r$d3_xiv <- ifelse(r$info_mode.loud_speakers == 1, 1, 0)
  r$d3_xv <- ifelse(r$info_mode.radio == 1, 1, 0)
  r$d4 <- ifelse(r$aid_received == "yes", 1, 0)
  r$d5_i <- ifelse(r$aid_type.cash == 1, 1, 0)
  r$d5_ii <- ifelse(r$aid_type.food == 1, 1, 0)
  r$d5_iii <- ifelse(r$aid_type.water == 1, 1, 0)
  r$d5_iv <- ifelse(r$aid_type.fuel == 1, 1, 0)
  r$d5_v <- ifelse(r$aid_type.shelter == 1, 1, 0)
  r$d5_vi <- ifelse(r$aid_type.seasonal_items == 1, 1, 0)
  r$d5_vii <- ifelse(r$aid_type.healthcare == 1, 1, 0)
  r$d5_viii <- ifelse(r$aid_type.other_nfi == 1, 1, 0)
  r$d5_ix <- ifelse(r$aid_type.education == 1, 1, 0)
  r$d5_x <- ifelse(r$aid_type.protection == 1, 1, 0)
  r$d6 <- ifelse(r$aid_satisfaction == "yes", 1, ifelse(r$aid_satisfaction %in% c("decline_to_answer", "do_not_know", "no"), 0, NA))
  r$d7 <- ifelse(r$aid_not_satisfied.quantity == 1, 1, 0)
  r$d10 <- ifelse(r$aid_workers_satisfied == "", NA, ifelse(r$aid_workers_satisfied == "no", 1, 0))
  r$d13_i <- ifelse(r$aid_feedback.sms == 1, 1, 0)
  r$d13_ii <- ifelse(r$aid_feedback.aid_worker_home == 1, 1, 0)
  r$d13_iii <- ifelse(r$aid_feedback.aid_worker_office == 1, 1, 0)
  r$d13_iv <- ifelse(r$aid_feedback.member_community == 1, 1, 0)
  r$d13_v <- ifelse(r$aid_feedback.phone_call == 1, 1, 0)
  r$d13_vi <- ifelse(r$aid_feedback.email == 1, 1, 0)
  r$d13_vii <- ifelse(r$aid_feedback.letter == 1, 1, 0)
  r$d13_viii <- ifelse(r$aid_feedback.social_media == 1, 1, 0)
  r$d13_ix <- ifelse(r$aid_feedback.suggestion_box == 1, 1, 0)
  r$d11 <- ifelse(r$community_decision_making == "yes", 1, 0)
  r$d12 <- ifelse(r$complaint_mechanisms == "yes", 1, ifelse(r$complaint_mechanisms %in% c("do_not_know", "no"), 0, NA))
  r$d14_i <- ifelse(r$info_specific_needs_who.age %in% c(NA, 0), 0, 1)
  r$d14_ii <- ifelse(r$info_specific_needs_who.unaccompanied %in% c(NA, 0), 0, 1)
  r$d14_iii <- ifelse(r$info_specific_needs_who.health_condition %in% c(NA, 0), 0, 1)
  r$d14_iv <- ifelse(r$info_specific_needs_who.protection_need %in% c(NA, 0), 0, 1)
  r$d14_v <- ifelse(r$info_specific_needs_who.single_women %in% c(NA, 0), 0, 1)
  r$d14_vi <- ifelse(r$info_specific_needs_who.fmhh %in% c(NA, 0), 0, 1)
  r$d14_vii <- ifelse(r$info_specific_needs_who.disabilities %in% c(NA, 0), 0, 1)
  r$d14_viii <- ifelse(r$info_specific_needs_who.mental_health %in% c(NA, 0), 0, 1)
  r$d14_ix <- ifelse(r$info_specific_needs_who.illeterate %in% c(NA, 0), 0, 1)
  
  return(r)
}

recoding_O <- function(r, loop) {
  loop_hoh <- loop[which(loop$relationship == "head"),]
  loop_children <- loop[which(loop$age < 18),]
  loop_females <- loop[which(loop$sex == "female"),]
  loop_females <- loop_females %>% mutate(plw = ifelse(pregnant_lactating == "yes", 1, 0))
  r_female_headed <- r[which(r$X_uuid %in% loop$X_submission__uuid[which(loop$sex == "female" & loop$relationship == "head")]),]
  r$gender_hhh <- loop_hoh$sex[match(r$X_uuid, loop_hoh$X_submission__uuid)]
  r$f_hhh <- ifelse(r$gender_hhh == "female", 1, 0)
  ### Extra CP indicators
  loop_girls_18 <- loop[which(loop$age < 18 & loop$sex == "female"),]
  loop_girls_12 <- loop[which(loop$age < 12 & loop$sex == "female"),]
  loop_girls_12_18 <- loop_children[which(loop_children$age >= 12 & loop_children$sex == "female"),]
  loop_boys_12 <- loop[which(loop$age < 12 & loop$sex == "male"),]
  loop_boys_18 <- loop[which(loop$age < 18 & loop$sex == "male"),]
  loop_boys_12_18 <- loop_children[which(loop_children$age >= 12 & loop_children$sex == "male"),]
  PLW <- as.data.frame(loop_females %>% dplyr::group_by(X_submission__uuid) %>% dplyr::summarize(sum(plw)))
  
  # CP indicators
  r$a12_ad1 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop_girls_12$marital_status[which(loop_girls_12$X_submission__uuid == x["X_uuid"])] == "married"), 1, 
           ifelse(x["X_uuid"] %in% loop_girls_12$X_submission__uuid, 0, NA))
  })
  r$a12_ad2 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop_girls_12_18$marital_status[which(loop_girls_12_18$X_submission__uuid == x["X_uuid"])] == "married"), 1, 
           ifelse(x["X_uuid"] %in% loop_girls_12_18$X_submission__uuid, 0, NA))
  })
  r$a12_ad3 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop_boys_12$marital_status[which(loop_boys_12$X_submission__uuid == x["X_uuid"])] == "married"), 1, 
           ifelse(x["X_uuid"] %in% loop_boys_12$X_submission__uuid, 0, NA))
  })
  r$a12_ad4 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop_boys_12_18$marital_status[which(loop_boys_12_18$X_submission__uuid == x["X_uuid"])] == "married"), 1, 
           ifelse(x["X_uuid"] %in% loop_boys_12_18$X_submission__uuid, 0, NA))
  })
  r$a13_ad1 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop_girls_12$work[which(loop_girls_12$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 
           ifelse(x["X_uuid"] %in% loop_girls_12$X_submission__uuid, 0, NA))
  })
  r$a13_ad2 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop_girls_12_18$work[which(loop_girls_12_18$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 
           ifelse(x["X_uuid"] %in% loop_girls_12_18$X_submission__uuid, 0, NA))
  })
  r$a13_ad3 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop_boys_12$work[which(loop_boys_12$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 
           ifelse(x["X_uuid"] %in% loop_boys_12$X_submission__uuid, 0, NA))
  })
  r$a13_ad4 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop_boys_12_18$work[which(loop_boys_12_18$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 
           ifelse(x["X_uuid"] %in% loop_boys_12_18$X_submission__uuid, 0, NA))
  })
  r$c3_ad1 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop_children$difficulty_seeing[which(loop_children$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop_children$difficulty_hearing[which(loop_children$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop_children$difficulty_walking[which(loop_children$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop_children$difficulty_remembering[which(loop_children$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop_children$difficulty_washing[which(loop_children$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop_children$difficulty_communicating[which(loop_children$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")), 1, 0)
  })
  r$c3_ad2 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop_girls_18$difficulty_seeing[which(loop_girls_18$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop_girls_18$difficulty_hearing[which(loop_girls_18$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop_girls_18$difficulty_walking[which(loop_girls_18$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop_girls_18$difficulty_remembering[which(loop_girls_18$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop_girls_18$difficulty_washing[which(loop_girls_18$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop_girls_18$difficulty_communicating[which(loop_girls_18$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")), 1, 
           ifelse(x["c3_ad1"] == 1, 0, NA))
  })
  r$c3_ad3 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop_boys_18$difficulty_seeing[which(loop_boys_18$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop_boys_18$difficulty_hearing[which(loop_boys_18$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop_boys_18$difficulty_walking[which(loop_boys_18$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop_boys_18$difficulty_remembering[which(loop_boys_18$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop_boys_18$difficulty_washing[which(loop_boys_18$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop_boys_18$difficulty_communicating[which(loop_boys_18$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")), 1, 
           ifelse(x["c3_ad1"] == 1, 0, NA))
  })
  r$g4_ad1 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop_girls_12$attend_formal_ed[which(loop_girls_12$X_submission__uuid == x["X_uuid"])] == "no" &
                 loop_girls_12$attend_informal_ed[which(loop_girls_12$X_submission__uuid == x["X_uuid"])] == "no"), 1, 
           ifelse(x["X_uuid"] %in% loop_girls_12$X_submission__uuid, 0, NA))
  })
  r$g4_ad2 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop_girls_12_18$attend_formal_ed[which(loop_girls_12_18$X_submission__uuid == x["X_uuid"])] == "no" &
                 loop_girls_12_18$attend_informal_ed[which(loop_girls_12_18$X_submission__uuid == x["X_uuid"])] == "no"), 1, 
           ifelse(x["X_uuid"] %in% loop_girls_12_18$X_submission__uuid, 0, NA))
  })
  r$g4_ad3 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop_boys_12$attend_formal_ed[which(loop_boys_12$X_submission__uuid == x["X_uuid"])] == "no" &
                 loop_boys_12$attend_informal_ed[which(loop_boys_12$X_submission__uuid == x["X_uuid"])] == "no"), 1, 
           ifelse(x["X_uuid"] %in% loop_boys_12$X_submission__uuid, 0, NA))
  })
  r$g4_ad4 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop_boys_12_18$attend_formal_ed[which(loop_boys_12_18$X_submission__uuid == x["X_uuid"])] == "no" &
                 loop_boys_12_18$attend_informal_ed[which(loop_boys_12_18$X_submission__uuid == x["X_uuid"])] == "no"), 1, 
           ifelse(x["X_uuid"] %in% loop_boys_12_18$X_submission__uuid, 0, NA))
  })
  
  r$g51_ad1 <- ifelse(r$birth_cert_missing_amount_u1 %in% c(NA, 0), 0, 1)
  r$g51_ad2 <- ifelse(r$birth_cert_missing_amount_a1 %in% c(NA, 0), 0, 1)
  r$g51_ad3 <- ifelse(r$id_card_u18 == "no", 1, 0)
  r$sev_ex_1 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop_females$difficulty_seeing[which(loop_females$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop_females$difficulty_hearing[which(loop_females$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop_females$difficulty_walking[which(loop_females$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop_females$difficulty_remembering[which(loop_females$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop_females$difficulty_washing[which(loop_females$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop_females$difficulty_communicating[which(loop_females$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")), 1, 0)
  })
  
  # Other bonus material
  r$f6 <- ifelse(r$own_land == "yes", 1, 0)
  r$g44 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop$age[which(loop$X_submission__uuid == x["X_uuid"])] > 17 & 
                 loop$work[which(loop$X_submission__uuid == x["X_uuid"])] == "no" & 
                 loop$actively_seek_work[which(loop$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 0)
  })
  r$g44_ad1 <- ifelse(r$g44 == 1, "unemployed_and_seeking_work", "not_unemployed_and_seeking_work")
  r$b5_ad1 <- ifelse(r$selling_assets %in% c("no_already_did", "yes"), 1, 0)
  r$b5_ad2 <- ifelse(r$borrow_debt  %in% c("no_already_did", "yes"), 1, 0)
  r$b5_ad3 <- ifelse(r$reduce_spending %in% c("no_already_did", "yes"), 1, 0)
  r$b6_ad1 <- ifelse(r$selling_transportation_means %in% c("no_already_did", "yes"), 1, 0)
  r$b6_ad2 <- ifelse(r$change_place  %in% c("no_already_did", "yes"), 1, 0)
  r$b6_ad3 <- ifelse(r$child_work %in% c("no_already_did", "yes"), 1, 0)
  r$b7_ad1 <- ifelse(r$child_dropout_school %in% c("no_already_did", "yes"), 1, 0)
  r$b7_ad2 <- ifelse(r$adult_risky  %in% c("no_already_did", "yes"), 1, 0)
  r$b7_ad3 <- ifelse(r$family_migrating %in% c("no_already_did", "yes"), 1, 0)
  r$b7_ad4 <- ifelse(r$child_forced_marriage %in% c("no_already_did", "yes"), 1, 0)
  
  r$seasonal <- ifelse(r$employment_seasonal == "yes", "seasonal_employment", "not_seasonal_employment")
  
  r$inc_source <- apply(r, 1, FUN=function(x){
    inc <- names(which.max(x[c("inc_debt", "inc_employment", "inc_remittances", "inc_humaid", "inc_debt",
                               "inc_pension", "inc_selling_assets", "inc_momd", "inc_molsa", "inc_other_safety",
                               "inc_other", "inc_rent")]))
    ifelse(is.null(inc),NA,inc)
  })
  r$g94 <- ifelse(r$tank_capacity * r$refill_times / r$people_share_tank / 7 < 80, 1, 0)
  r$g96 <- ifelse(r$treat_drink_water_how == "not_necessary", 0, 1)
  r$g97 <- ifelse(rowSums(r[, c("latrines.flush", "latrines.vip_pit")], na.rm = T) > 0, 1, 0)
  r$g99 <- ifelse(r$access_hygiene_items == "yes" & r$use_of_soap.handwashing == 1, 1, 0)
  r$disabled_hhh <- apply(r, 1, FUN=function(x){
    ifelse(any(loop_hoh[which(loop_hoh$X_submission__uuid == x["X_uuid"]), c("difficulty_seeing", "difficulty_hearing",
                                                                             "difficulty_walking", "difficulty_remembering",
                                                                             "difficulty_washing", "difficulty_communicating",
                                                                             "difficulty_accessing_services")] %in% 
                 c("a_lot_of_difficulty", "cannot_do_at_all")), "disabled_hhh", "non_disabled_hhh")
  })
  r$plw_hh <- ifelse(PLW$`sum(plw)`[match(r$X_uuid, PLW$X_submission__uuid)] %in% c(NA, 0), "HH_without_PLW", "HH_with_PLW")
  r$g56_ad1 <- ifelse(is.na(r$child_distress_number), 0, r$child_distress_number) + ifelse(is.na(r$adult_distress_number), 0, r$adult_distress_number)
  
  
  r$g44 %>% table(useNA = "always")
  quantile(r$g44)
  
  return(r)
}

recoding_severity <- function(r, loop){
  loop_children <- loop[which(loop$age < 18),]
  r$s1_1 <- ifelse(! (r$selling_assets %in% c("no_already_did", "yes") |
                        r$borrow_debt  %in% c("no_already_did", "yes") |
                        r$reduce_spending %in% c("no_already_did", "yes") |
                        r$selling_transportation_means %in% c("no_already_did", "yes") |
                        r$change_place  %in% c("no_already_did", "yes") |
                        r$child_work %in% c("no_already_did", "yes") |
                        r$child_dropout_school %in% c("no_already_did", "yes") |
                        r$adult_risky  %in% c("no_already_did", "yes") |
                        r$family_migrating %in% c("no_already_did", "yes") |
                        r$child_forced_marriage %in% c("no_already_did", "yes")), 1, 0)
  r$s1_2 <- NA
  r$s1_3 <- ifelse((r$selling_assets %in% c("no_already_did", "yes") |
                      r$borrow_debt  %in% c("no_already_did", "yes") |
                      r$reduce_spending %in% c("no_already_did", "yes")) & ! 
                     (r$selling_transportation_means %in% c("no_already_did", "yes") |
                        r$change_place  %in% c("no_already_did", "yes") |
                        r$child_work %in% c("no_already_did", "yes") |
                        r$child_dropout_school %in% c("no_already_did", "yes") |
                        r$adult_risky  %in% c("no_already_did", "yes") |
                        r$family_migrating %in% c("no_already_did", "yes") |
                        r$child_forced_marriage %in% c("no_already_did", "yes")), 1, 0)
  r$s1_4 <- ifelse((r$selling_transportation_means %in% c("no_already_did", "yes") |
                      r$change_place  %in% c("no_already_did", "yes") |
                      r$child_work %in% c("no_already_did", "yes")) & ! 
                     (r$child_dropout_school %in% c("no_already_did", "yes") |
                        r$adult_risky  %in% c("no_already_did", "yes") |
                        r$family_migrating %in% c("no_already_did", "yes") |
                        r$child_forced_marriage %in% c("no_already_did", "yes")), 1, 0)
  r$s1_5 <- ifelse(r$child_dropout_school %in% c("no_already_did", "yes") |
                     r$adult_risky  %in% c("no_already_did", "yes") |
                     r$family_migrating %in% c("no_already_did", "yes") |
                     r$child_forced_marriage %in% c("no_already_did", "yes"), 1, 0)
  r$perc_disabled <- apply(r, 1, FUN=function(x){
    nrow(loop %>% filter(X_submission__uuid == x["X_uuid"]) %>% 
           filter(difficulty_hearing %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
                    difficulty_seeing %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
                    difficulty_walking %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
                    difficulty_remembering %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
                    difficulty_washing %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
                    difficulty_communicating %in% c("a_lot_of_difficulty", "cannot_do_at_all"))) /
      nrow(loop %>% filter(X_submission__uuid == x["X_uuid"]))
  })
  
  r$s2_1 <- ifelse(r$perc_disabled <= 0.05, 1, 0)
  r$s2_2 <- ifelse(r$perc_disabled > 0.05 & r$perc_disabled <= 0.2, 1, 0)
  r$s2_3 <- ifelse(r$perc_disabled > 0.2 & r$perc_disabled <= 0.4, 1, 0)
  r$s2_4 <- ifelse(r$perc_disabled > 0.4 & r$perc_disabled <= 0.5, 1, 0)
  r$s2_5 <- ifelse(r$perc_disabled > 0.5, 1, 0)
  
  r$basic_basic_nfi <- rowSums(r[,c("nfi_priority_needs.bedding_items","nfi_priority_needs.mattresses_sleeping_mats",
                                    "nfi_priority_needs.blankets", "nfi_priority_needs.cooking_utensils_kitchen_set",
                                    "nfi_priority_needs.cooking_stove", "nfi_priority_needs.source_of_light",
                                    "nfi_priority_needs.fuel_storage")])
  r$basic_nfi <- rowSums(r[,c("nfi_priority_needs.bedding_items","nfi_priority_needs.mattresses_sleeping_mats",
                              "nfi_priority_needs.blankets", "nfi_priority_needs.cooking_utensils_kitchen_set",
                              "nfi_priority_needs.cooking_stove", "nfi_priority_needs.source_of_light",
                              "nfi_priority_needs.fuel_storage", "nfi_priority_needs.cool_box",
                              "nfi_priority_needs.water_storage", "nfi_priority_needs.fan",
                              "nfi_priority_needs.winter_heaters", "nfi_priority_needs.clothing",
                              #"nfi_priority_needs.awc", 
                              "nfi_priority_needs.cooking_fuel",
                              "nfi_priority_needs.heating_fuel")])
  r$s3_1 <- ifelse(r$basic_nfi == 0, 1, 0)
  r$s3_2 <- ifelse(r$basic_nfi == 1, 1, 0)
  r$s3_3 <- ifelse(r$basic_nfi == 2, 1, 0)
  r$s3_4 <- ifelse(r$basic_nfi == 3, 1, 0)
  r$s3_5 <- ifelse(r$basic_nfi == 4, 1, 0)
  
  r$s4_1 <- ifelse(r$shelter_better.none == 1, 1, 0)
  r$s4_4 <- ifelse(r$s4_1 != 1 & (r$shelter_better.improve_safety == 1 | r$shelter_better.improve_structure == 1), 1, 0)
  r$s4_3 <- ifelse((r$s4_1 != 1 & r$s4_4 != 1) & 
                     (r$shelter_better.improve_privacy == 1 | r$shelter_better.protect_climate == 1), 1, 0)
  r$s4_2 <- ifelse((r$s4_1 != 1 & r$s4_4 != 1 & r$s4_3 != 1) & 
                     (r$shelter_better.protec_hazards == 1 | 
                        r$shelter_better.improve_tenure == 1 | r$shelter_better.improve_infrastructure == 1), 1, 0)
  r$s4_5 <- NA
  r[which(rowSums(r[,which(startsWith(names(r), "shelter_better."))]) == 0 | r$shelter_better.other == 1), 
    which(startsWith(names(r), "s4"))] <- NA
  
  r$s5_2 <- NA
  r$s5_3 <- ifelse(r$shelter_type %in% c("unfinished_abandoned_building", "damaged_building", "tent", 
                                         "religious_building", "public_building", "non_residential", 
                                         "container", "makeshift_shelter") | !is.na(r$camp), 1, 0)
  r$s5_1 <- ifelse(r$s5_3 == 1, 0, 1)
  r$s5_4 <- NA
  r$s5_5 <- NA
  
  
  loop_hoh <- loop[which(loop$relationship == "head"),]
  loop_females <- loop[which(loop$sex == "female"),]
  loop_females <- loop_females %>% mutate(plw = ifelse(pregnant_lactating == "yes", 1, 0))
  r_female_headed <- r[which(r$X_uuid %in% loop$X_submission__uuid[which(loop$sex == "female" & loop$relationship == "head")]),]
  r$gender_hhh <- loop_hoh$sex[match(r$X_uuid, loop_hoh$X_submission__uuid)]
  
  r$s6_1 <- ifelse(r$gender_hhh == "male", 1, 0)
  r$s6_2 <- NA
  r$s6_3 <- ifelse(r$gender_hhh == "female", 1, 0)
  r$s6_4 <- NA
  r$s6_5 <- NA
  
  r$s7_1 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop$disab_explos[which(loop$X_submission__uuid == x["X_uuid"])] == "yes"), 0, 1)
  })
  r$s7_2 <- NA
  r$s7_3 <- NA
  r$s7_4 <- apply(r, 1, FUN=function(x){
    select <- loop %>% filter(X_submission__uuid == x["X_uuid"]) %>% 
      filter(difficulty_hearing == "a_lot_of_difficulty" |
               difficulty_seeing == "a_lot_of_difficulty" |
               difficulty_walking == "a_lot_of_difficulty" |
               difficulty_remembering == "a_lot_of_difficulty" |
               difficulty_washing == "a_lot_of_difficulty" |
               difficulty_communicating == "a_lot_of_difficulty") %>% dplyr::select(disab_explos)
    ifelse(nrow(select) > 0 && any(select == "yes"), 1, 0)
  })
  r$s7_5 <- apply(r, 1, FUN=function(x){
    select <- loop %>% filter(X_submission__uuid == x["X_uuid"]) %>% 
      filter(difficulty_hearing == "cannot_do_at_all" |
               difficulty_seeing == "cannot_do_at_all" |
               difficulty_walking == "cannot_do_at_all" |
               difficulty_remembering == "cannot_do_at_all" |
               difficulty_washing == "cannot_do_at_all" |
               difficulty_communicating == "cannot_do_at_all") %>% dplyr::select(disab_explos)
    ifelse(nrow(select) > 0 && any(select == "yes"), 1, 0)
  })
  r <- r %>% mutate(s7_4 = ifelse(s7_4 == 1 & s7_5 == 1, 0, s7_4))
  
  # r$info_specific_needs_who_sum <- rowSums(r[, c("info_specific_needs_who.mental_health", "info_specific_needs_who.disabilities",
  #                                                "info_specific_needs_who.illeterate", "info_specific_needs_who.unaccompanied",
  #                                                "info_specific_needs_who.other", "info_specific_needs_who.fmhh",
  #                                                "info_specific_needs_who.health_condition", "info_specific_needs_who.single_women",
  #                                                "info_specific_needs_who.age", "info_specific_needs_who.protection_need")], na.rm = T)
  # r$s8_1 <- ifelse(r$info_specific_needs_who_sum == 0, 1, 0)
  # r$s8_2 <- ifelse(r$info_specific_needs_who_sum == 1, 1, 0)
  # r$s8_3 <- ifelse(r$info_specific_needs_who_sum %in% c(2,3), 1, 0)
  # r$s8_4 <- ifelse(r$info_specific_needs_who_sum %in% c(4,5), 1, 0)
  # r$s8_5 <- ifelse(r$info_specific_needs_who_sum >= 6, 1, 0)
  r$s8_5 <- ifelse(r$aid_received == "yes" & r$aid_workers_satisfied == "no", 1, 0)
  r$s8_4 <- ifelse(r$s8_5 != 1 & ! r$aid_not_satisfied.delays %in% c(NA, 0), 1, 0)
  r$s8_3 <- ifelse((r$s8_4 != 1 & r$s8_5 != 1) & ! r$aid_not_satisfied.quality %in% c(NA, 0), 1, 0)
  r$s8_2 <- ifelse((r$s8_3 != 1 & r$s8_4 != 1 & r$s8_5 != 1) & ! r$aid_not_satisfied.quantity %in% c(NA, 0), 1, 0)
  r$s8_1 <- ifelse((r$s8_2 != 1 & r$s8_3 != 1 & r$s8_4 != 1 & r$s8_5 != 1) & r$aid_received == "yes", 1, 0)
  r[which(r$aid_received != "yes"), 
    which(startsWith(names(r), "s8"))] <- NA
  
  r$perc_edu <- apply(r, 1, FUN=function(x){
    (loop %>% filter(X_submission__uuid == x["X_uuid"] & (attend_formal_ed == "yes" | attend_informal_ed == "yes")) %>% nrow) / 
      (loop %>% filter(X_submission__uuid == x["X_uuid"] & attend_formal_ed != "") %>% nrow)
  })
  r$s9_1 <- ifelse(r$perc_edu == 1, 1, 0)
  r$s9_2 <- ifelse(r$perc_edu >= 0.75 & r$perc_edu < 1, 1, 0)
  r$s9_3 <- ifelse(r$perc_edu >= 0.5 & r$perc_edu < 0.75, 1, 0)
  r$s9_4 <- ifelse(r$perc_edu >= 0.25 & r$perc_edu < 0.5, 1, 0)
  r$s9_5 <- ifelse(r$perc_edu < 0.25, 1, 0)
  
  r$fcs <- r$cereals * 2 + r$nuts_seed * 3 + r$milk_dairy * 4 + r$meat * 4 + 
    r$vegetables + r$fruits + r$oil_fats * 0.5 + r$sweets * 0.5
  r$fcs_score <- ifelse(r$fcs <= 28, 4, ifelse(r$fcs <= 42, 3, 1))
  
  
  r$stress <- ifelse(r$selling_assets %in% c("no_already_did", "yes") |
                       r$borrow_debt  %in% c("no_already_did", "yes") |
                       r$reduce_spending %in% c("no_already_did", "yes") |
                       r$spending_savings %in% c("no_already_did", "yes")   , 1, 0)
  r$crisis <- ifelse(r$selling_transportation_means %in% c("no_already_did", "yes") |
                       r$change_place  %in% c("no_already_did", "yes") |
                       r$child_work %in% c("no_already_did", "yes"), 1, 0)
  r$emergency <- ifelse(r$child_dropout_school %in% c("no_already_did", "yes") |
                          r$adult_risky  %in% c("no_already_did", "yes") |
                          r$family_migrating %in% c("no_already_did", "yes") |
                          r$child_forced_marriage %in% c("no_already_did", "yes"), 1, 0)
  r$cs_score <- ifelse(r$emergency == 1, 4, ifelse(r$crisis == 1, 3, ifelse(r$stress == 1, 2, 1)))
  
  r$fes <- r$food_exp / r$tot_expenditure
  r$fes_score <- ifelse(r$fes <= 0.5, 1, ifelse(r$fes <= 0.65, 2, ifelse(r$fes <= 0.75, 3, 4)))
  
  r$fs_class_int <- rowMeans(r[,c("fes_score", "cs_score")])
  r$fs_class <- rowMeans(r[,c("fcs_score", "fs_class_int")])
  
  r$s10_1 <- ifelse(r$fs_class < 1.5, 1, 0)
  r$s10_2 <- ifelse(r$fs_class >= 1.5 & r$fs_class < 2.5, 1, 0)
  r$s10_3 <- ifelse(r$fs_class >= 2.5 & r$fs_class < 3.5, 1, 0)
  r$s10_4 <- ifelse(r$fs_class >= 3.5, 1, 0)
  r$s10_5 <- NA
  
  r$s11_1 <- ifelse(r$distance_health_service == "within_2km", 1, 0)
  r$s11_2 <- ifelse(r$distance_health_service == "between_2km_5km", 1, 0)
  r$s11_3 <- ifelse(r$distance_health_service == "more_than5kmaway", 1, 0)
  r$s11_4 <- ifelse(r$distance_health_service == "none", 1, 0)
  r$s11_5 <- NA
  r[which(r$distance_health_service == "do_not_know"), 
    which(startsWith(names(r), "s11"))] <- NA
  
  r$s12_1 <- ifelse(r$distance_hospital == "within_2km", 1, 0)
  r$s12_2 <- ifelse(r$distance_hospital == "between_2km_5km", 1, 0)
  r$s12_3 <- ifelse(r$distance_hospital == "between_6km_10km", 1, 0)
  r$s12_4 <- ifelse(r$distance_hospital == "more_10km", 1, 0)
  r$s12_5 <- ifelse(r$distance_hospital == "none", 1, 0)
  r[which(r$distance_hospital == "do_not_know"), 
    which(startsWith(names(r), "s12"))] <- NA
  
  r$perc_work <- apply(r, 1, FUN=function(x){
    (loop %>% filter(X_submission__uuid == x["X_uuid"] & age > 17 & work == "no" & actively_seek_work %in% c("yes")) %>% nrow) / 
      (loop %>% filter(X_submission__uuid == x["X_uuid"] & age > 17) %>% nrow)
  })
  r$s13_1 <- ifelse(r$perc_work == 0, 1, 0)
  r$s13_2 <- ifelse(r$perc_work > 0 & r$perc_work <= 0.5, 1, 0)
  r$s13_3 <- ifelse(r$perc_work > 0.5 & r$perc_work <= 0.7, 1, 0)
  r$s13_4 <- ifelse(r$perc_work > 0.7 & r$perc_work <= 0.9, 1, 0)
  r$s13_5 <- ifelse(r$perc_work > 0.9, 1, 0)
  
  r$s14_1 <- ifelse(r$reasons_for_debt %in% c("", "clothing", "other", "purchase_pro_assets"), 1, 0)
  r$s14_2 <- NA
  r$s14_3 <- ifelse(r$reasons_for_debt %in% c("education", "basic_hh_expenditure"), 1, 0)
  r$s14_4 <- ifelse(r$reasons_for_debt == "health", 1, 0)
  r$s14_5 <- ifelse(r$reasons_for_debt == "food", 1, 0)
  
  xe <- 1192
  r$s15_1 <- ifelse(r$how_much_debt <= 400 * xe, 1, 0)
  r$s15_2 <- ifelse(r$how_much_debt > 400 * xe & r$how_much_debt <= 600 * xe, 1, 0)
  r$s15_3 <- ifelse(r$how_much_debt > 600 * xe & r$how_much_debt <= 800 * xe, 1, 0)
  r$s15_4 <- ifelse(r$how_much_debt > 800 * xe & r$how_much_debt <= 1000 * xe, 1, 0)
  r$s15_5 <- ifelse(r$how_much_debt > 1000 * xe, 1, 0)
  
  r$child_marriage <- apply(r, 1, FUN=function(x){
    ifelse(any(loop_children$marital_status[which(loop_children$X_submission__uuid == x["X_uuid"])] == "married"), 1, 0)
  })
  r$child_labor <- apply(r, 1, FUN=function(x){
    ifelse(any(loop$age[which(loop$X_submission__uuid == x["X_uuid"])] < 18 & 
                 loop$work[which(loop$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 0)
  })
  r$child_education <- apply(r, 1, FUN=function(x){
    ifelse(any(loop$attend_formal_ed[which(loop$X_submission__uuid == x["X_uuid"])] == "no" & 
                 loop$attend_informal_ed[which(loop$X_submission__uuid == x["X_uuid"])] == "no"), 1, 0)
  })
  r$child_documents <- ifelse(r$id_card_u18 == "no", 1, 0)
  r$child_distress <- ifelse(r$child_distress_number < 1 | is.na(r$child_distress_number), 0, 1)
  r$child_composite <- rowSums(r[,c("child_marriage", "child_labor", "child_documents", "child_distress", "child_education")])
  r$s16_1 <- ifelse(r$child_composite == 0, 1, 0)
  r$s16_2 <- 0
  r$s16_3 <- ifelse(r$child_composite == 1, 1, 0)
  r$s16_4 <- ifelse(r$child_composite == 2, 1, 0)
  r$s16_5 <- ifelse(r$child_composite >= 3, 1, 0)
  
  r$unsafe_areas_sum <- rowSums(r[,c("unsafe_areas.facilities", "unsafe_areas.water_points",
                                     "unsafe_areas.social_areas", "unsafe_areas.distribution_areas",
                                     "unsafe_areas.markets", "unsafe_areas.way_to_centers",
                                     "unsafe_areas.way_to_school")], na.rm=T)
  r$s17_1 <- ifelse(r$unsafe_areas_sum == 0, 1, 0)
  r$s17_2 <- ifelse(r$unsafe_areas_sum == 1, 1, 0)
  r$s17_3 <- ifelse(r$unsafe_areas_sum == 2, 1, 0)
  r$s17_4 <- ifelse(r$unsafe_areas_sum == 3, 1, 0)
  r$s17_5 <- ifelse(r$unsafe_areas_sum >= 4, 1, 0)
  
  r$sum_documents <- apply(r, 1, FUN=function(x){ 
    sum(x[c("pds", "info_card", "death_certificate", "guardianship", "inheritance",
            "trusteeship", "passport_a18", "id_card_a18", "citizenship_a18", "birth_cert_a18",
            "school_cert_a18", "marriage_cert_a18", "divorce_cert_a18",
            "passport_u18", "id_card_u18", "citizenship_u18", "birth_cert_u18",
            "marriage_cert_u18", "divorce_cert_u18")] %in% c("no", "non_valid"))})
  # r$sum_specific_documents <- apply(r, 1, FUN=function(x){ 
  #   sum(x[c("pds","id_card_a18", "birth_cert_a18", "id_card_u18", "birth_cert_u18")] %in% 
  #         c("no", "non_valid"))})
  r$documents_new <- rowSums(r[,c("pds","info_card")] == "no") +
    ifelse(rowSums(r[,c("id_card_a18","id_card_u18")] == "no") >= 1, 1, 0) +
    ifelse(rowSums(r[,c("birth_cert_a18","birth_cert_u18")] == "no") >= 1, 1, 0) +
    ifelse(rowSums(r[,c("citizenship_a18","citizenship_u18")] == "no") >= 1, 1, 0) 
  r$documents_new2 <- rowSums(r[,c("pds","info_card","id_card_a18","id_card_u18","birth_cert_a18",
                                   "birth_cert_u18","citizenship_a18","citizenship_u18")] == "no")
  
  r$s18_1 <- ifelse(r$sum_documents == 0, 1, 0)
  r$s18_2 <- ifelse(r$sum_documents > 0 & r$documents_new2 == 0, 1, 0)
  r$s18_3 <- ifelse(r$documents_new2 %in% c(1,2), 1, 0)
  r$s18_4 <- ifelse(r$documents_new2 == 3, 1, 0)
  r$s18_5 <- ifelse(r$documents_new2 >= 4, 1, 0)
  
  
  r$s19_1 <- ifelse(r$hh_risk_eviction != "yes", 1, 0)
  r$s19_2 <- NA
  r$s19_5 <- ifelse(r$hh_risk_eviction == "yes" & rowSums(r[, c("hh_main_risks.authorities_request",
                                                                "hh_main_risks.inadequate",
                                                                "hh_main_risks.occupied",
                                                                "hh_main_risks.confiscation")]) > 0, 1, 0)
  r$s19_4 <- ifelse(r$hh_risk_eviction == "yes" & r$s19_5 != 1 &
                      rowSums(r[, c("hh_main_risks.no_longer_hosted",
                                    "hh_main_risks.unaccepted_by_community",
                                    "hh_main_risks.dispute")]) > 0, 1, 0)
  r$s19_3 <- ifelse(r$hh_risk_eviction == "yes" & r$s19_5 != 1 & r$s19_4 != 1 &
                      rowSums(r[, c("hh_main_risks.lack_funds",
                                    "hh_main_risks.owner_request",
                                    "hh_main_risks.no_agreement")]) > 0, 1, 0)
  
  r$water_quantity <- r$tank_capacity * r$refill_times / r$people_share_tank / 7
  r$s20_1 <- ifelse(r$water_quantity >= 80, 1, 0)
  r$s20_2 <- ifelse(r$water_quantity >= 50 & r$water_quantity < 80, 1, 0)
  r$s20_3 <- ifelse(r$water_quantity >= 15 & r$water_quantity < 50, 1, 0)
  r$s20_4 <- ifelse(r$water_quantity >= 7.5 & r$water_quantity < 15, 1, 0)
  r$s20_5 <- ifelse(r$water_quantity < 7.5, 1, 0)
  
  r$s21_1 <- ifelse(r$drinking_water_source.network_private == 1, 1, 0)
  r$s21_2 <- ifelse(r$s21_1 != 1 & rowSums(r[, c("drinking_water_source.network_comm",
                                                 "drinking_water_source.borehole",
                                                 "drinking_water_source.prot_well",
                                                 "drinking_water_source.prot_spring",
                                                 "drinking_water_source.prot_tank")]) > 0, 1, 0)
  r$s21_3 <- ifelse(rowSums(r[,c("s21_1", "s21_2")]) == 0 & 
                      rowSums(r[, c("drinking_water_source.bottled_water",
                                    "drinking_water_source.water_trucking")]) > 0, 1, 0)
  r$s21_4 <- ifelse(rowSums(r[,c("s21_1", "s21_2", "s21_3")]) == 0 & 
                      rowSums(r[, c("drinking_water_source.unprot_spring",
                                    "drinking_water_source.unprot_tank",
                                    "drinking_water_source.unprot_well",
                                    "drinking_water_source.illegal_connection")]) > 0, 1, 0)
  r$s21_5 <- ifelse(rowSums(r[,c("s21_1", "s21_2", "s21_3", "s21_4")]) == 0 & 
                      r$drinking_water_source.surface_water == 1, 1, 0)
  r[which(rowSums(r[,which(startsWith(names(r), "drinking_water_source."))]) == 1 & r$drinking_water_source.other == 1), 
    which(startsWith(names(r), "s21"))] <- NA
  
  
  r$s22_1 <- ifelse(rowSums(r[, c("latrines.flush", "latrines.vip_pit")], na.rm = T) > 0 & r$shared_sanitation == "no", 1, 0)
  r$s22_2 <- ifelse(rowSums(r[, c("latrines.flush", "latrines.vip_pit")], na.rm = T) > 0 & r$shared_sanitation == "yes", 1, 0)
  r$s22_3 <- ifelse(rowSums(r[, c("latrines.flush", "latrines.vip_pit", "latrines.none")], na.rm = T) == 0 & r$shared_sanitation == "no", 1, 0)
  r$s22_4 <- ifelse(rowSums(r[, c("latrines.flush", "latrines.vip_pit", "latrines.none")], na.rm = T) == 0 & r$shared_sanitation == "yes", 1, 0)
  r$s22_5 <- ifelse(r$latrines.none == 1, 1, 0)
  
  r$s10_1 %>% table(useNA = "always")
  sum(length(which(r$s22_1==1)),length(which(r$s22_2==1)),length(which(r$s22_3==1)),length(which(r$s22_4==1)),length(which(r$s22_5==1)))
  sum(length(which(r$s22_1==1)),length(which(r$s22_2==1)))
  sum(length(which(r$s22_3==1)),length(which(r$s22_4==1)),length(which(r$s22_5==1)))
  return(r)
}

recoding_snfi <- function(r, loop){
  
  r$hh_pwsn <- apply(r, 1, FUN=function(x){
    ifelse(any(loop$difficulty_seeing[which(loop$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop$difficulty_hearing[which(loop$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop$difficulty_walking[which(loop$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop$difficulty_remembering[which(loop$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop$difficulty_washing[which(loop$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
             any(loop$difficulty_communicating[which(loop$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")), 1, 0)
  })
  
  r$cs <- ifelse(r$shelter_type %in% c("unfinished_abandoned_building", "damaged_building", "tent", 
                                       "religious_building", "public_building", "non_residential", 
                                       "container", "makeshift_shelter") | !is.na(r$camp), 1, 0)
  
  r$hh_pwsn_cs <- ifelse(r$hh_pwsn == 0, NA, ifelse(r$cs == 1, 1, 0))
  
  r$s4_1 <- ifelse(r$shelter_better.none == 1, 1, 0)
  r$s4_4 <- ifelse(r$s4_1 != 1 & (r$shelter_better.improve_safety == 1 | r$shelter_better.improve_structure == 1), 1, 0)
  r$s4_3 <- ifelse((r$s4_1 != 1 & r$s4_4 != 1) & 
                     (r$shelter_better.improve_privacy == 1 | r$shelter_better.protect_climate == 1), 1, 0)
  r$s4_2 <- ifelse((r$s4_1 != 1 & r$s4_4 != 1 & r$s4_3 != 1) & 
                     (r$shelter_better.protec_hazards == 1 | 
                        r$shelter_better.improve_tenure == 1 | r$shelter_better.improve_infrastructure == 1), 1, 0)
  r$s4_5 <- NA
  r[which(rowSums(r[,which(startsWith(names(r), "shelter_better."))]) == 0 | r$shelter_better.other == 1 | r$hh_pwsn == 0), 
    which(startsWith(names(r), "s4"))] <- NA
  
  r$s4_3 %>% table(useNA = "always")
  sum(length(which(r$s4_1==1)),length(which(r$s4_2==1)),length(which(r$s4_3==1)),length(which(r$s4_4==1)),length(which(r$s4_5==1)))
  sum(length(which(r$s4_1==1)),length(which(r$s4_2==1)))
  sum(length(which(r$s4_3==1)),length(which(r$s4_4==1)),length(which(r$s4_5==1)))
  return(r)
  
  
}

recoding_education <- function(r, loop) {
  r$g4 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop$attend_formal_ed[which(loop$X_submission__uuid == x["X_uuid"])] == "no"), 1, 0)
  })
  return(r)
}

recoding_cp <- function(r, loop) {
  r$g51a <- ifelse(rowSums(r[,c("birth_cert_u18","id_card_u18")] == "no") > 0, 1, 0)
  return(r)
}

########################################################
#MSNI Analysis
########################################################

recoding_msni <- function(r, loop) {
  loop_children <- loop[which(loop$age < 18),]
  
  # Sub-pillar Impact on People
  r$a18 <- ifelse(r$population_group != "host", 3, 1)
  r$g67 <- ifelse(rowSums(r[,c("why_not_return.house_damaged_destroyed", "why_not_return.house_land_occupied")], na.rm=T) > 0, 1, 0)
  r$g73 <- ifelse(r$why_not_return.presence_of_mines %in% c(NA, 0), 0, 1)
  r$c2 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop$disab_explos[which(loop$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 0)
  })
  r$g6 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop$ever_attend[which(loop$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 0)
  })
  
  r$impact <- rowSums(r[,c("a18", "g67", "g73", "c2", "g6")])
  r <- r %>% mutate(impact = case_when(
    impact == 0 ~ 1,
    impact > 0 & impact <= 3 ~ 2,
    impact == 4 ~ 3,
    impact > 4 ~ 4
  ))
  
  # Sub-pillar Impact on Humanitarian Access
 # r$d6 <- ifelse(r$aid_satisfaction == "no", 2, ifelse(r$aid_satisfaction %in% c("decline_to_answer", "do_not_know", "yes"), 0, NA))
 # r$d10 <- ifelse(r$aid_workers_satisfied == "", NA, ifelse(r$aid_workers_satisfied == "no", 1, 0))
 # r$d11 <- ifelse(r$community_decision_making == "no", 1, 0)
 # r$d12 <- ifelse(r$complaint_mechanisms == "no", 1, ifelse(r$complaint_mechanisms %in% c("do_not_know", "yes", "decline_to_answer"), 0, NA))
 # r$d14 <- ifelse(r$info_specific_needs == "yes", 1, 0)
  
 # r$impact_hum <- round2(rowSums(r[,c("d6","d10","d11","d12","d14")]) / 6 * 3 + 1)
  
 # r$impact_hum %>% table(useNA = "always")
  
  # Sub-pillar Capacity Gap
  r$capacity_gap <- ifelse(r$child_dropout_school %in% c("no_already_did", "yes") |
                             r$adult_risky  %in% c("no_already_did", "yes") |
                             r$family_migrating %in% c("no_already_did", "yes") |
                             r$child_forced_marriage %in% c("no_already_did", "yes"), 4, 
                           ifelse(r$selling_transportation_means %in% c("no_already_did", "yes") |
                                    r$change_place  %in% c("no_already_did", "yes") |
                                    r$child_work %in% c("no_already_did", "yes"), 3, 
                                  ifelse(r$selling_assets %in% c("no_already_did", "yes") |
                                           r$borrow_debt  %in% c("no_already_did", "yes") |
                                           r$reduce_spending %in% c("no_already_did", "yes") |
                                           r$spending_savings %in% c("no_already_did", "yes"), 2, 1)))
  
  # Sub-pillar Well-being
 # r$g56 <- ifelse(r$child_distress_number < 1 | is.na(r$child_distress_number), 0, 1)
#r$g57 <- ifelse(r$adult_distress_number < 1 | is.na(r$adult_distress_number), 0, 1)
 # r$c3 <- apply(r, 1, FUN=function(x){
  #  ifelse(any(loop$difficulty_seeing[which(loop$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
   #          any(loop$difficulty_hearing[which(loop$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
    #         any(loop$difficulty_walking[which(loop$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
     #        any(loop$difficulty_remembering[which(loop$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
      #       any(loop$difficulty_washing[which(loop$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")) |
       #      any(loop$difficulty_communicating[which(loop$X_submission__uuid == x["X_uuid"])] %in% c("a_lot_of_difficulty", "cannot_do_at_all")), 1, 0)
#  })
  
#  r$well_being <- rowSums(r[,c("g56","g57","c3")]) + 1
  
  
  # Sub-pillar Vulnerability
  loop_hoh <- loop[which(loop$relationship == "head"),]
  loop_children <- loop[which(loop$age < 18),]
  loop_females <- loop[which(loop$sex == "female"),]
  loop_females <- loop_females %>% mutate(plw = ifelse(pregnant_lactating == "yes", 1, 0))
  r_female_headed <- r[which(r$X_uuid %in% loop$X_submission__uuid[which(loop$sex == "female" & loop$relationship == "head")]),]
  r$gender_hhh <- loop_hoh$sex[match(r$X_uuid, loop_hoh$X_submission__uuid)]
  r$f_hhh <- ifelse(r$gender_hhh == "female", 1, 0)
  r$a11 <- ifelse(loop_hoh[match(r$X_uuid,loop_hoh$X_submission__uuid), "marital_status"] %in%
                    c("single", "separated", "widowed", "divorced"), 3, 
                  ifelse(loop_hoh[match(r$X_uuid,loop_hoh$X_submission__uuid), "marital_status"] %in%
                           c(NA, ""), NA, 0))  
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
  r$vulnerability <- rowSums(r[,c("a11", "g35", "c3")])
  r$vulnerability <- round2(r$vulnerability)
  r <- r %>% mutate(vulnerability = case_when(
    vulnerability == 5 ~ 4, 
    vulnerability == 3 ~ 3,
    vulnerability == 1 ~ 1,
    vulnerability == 2 ~ 2,
    vulnerability == 4 ~ 4,
    vulnerability == 0 ~ 1))
  
  
  # Sub-pillar FoodSec
  r$fcs <- r$cereals * 2 + r$nuts_seed * 3 + r$milk_dairy * 4 + r$meat * 4 + 
    r$vegetables + r$fruits + r$oil_fats * 0.5 + r$sweets * 0.5
  r$fcs_score <- ifelse(r$fcs <= 28, 4, ifelse(r$fcs <= 42, 3, 1))
  
  r$stress <- ifelse(r$selling_assets %in% c("no_already_did", "yes") |
                       r$borrow_debt  %in% c("no_already_did", "yes") |
                       r$reduce_spending %in% c("no_already_did", "yes") |
                       r$spending_savings %in% c("no_already_did", "yes")   , 1, 0)
  r$crisis <- ifelse(r$selling_transportation_means %in% c("no_already_did", "yes") |
                       r$change_place  %in% c("no_already_did", "yes") |
                       r$child_work %in% c("no_already_did", "yes"), 1, 0)
  r$emergency <- ifelse(r$child_dropout_school %in% c("no_already_did", "yes") |
                          r$adult_risky  %in% c("no_already_did", "yes") |
                          r$family_migrating %in% c("no_already_did", "yes") |
                          r$child_forced_marriage %in% c("no_already_did", "yes"), 1, 0)
  r$cs_score <- ifelse(r$emergency == 1, 4, ifelse(r$crisis == 1, 3, ifelse(r$stress == 1, 2, 1)))
  
  r$fes <- r$food_exp / r$tot_expenditure
  r$fes_score <- ifelse(r$fes <= 0.5, 1, ifelse(r$fes <= 0.65, 2, ifelse(r$fes <= 0.75, 3, 4)))
  
  r$fs_class_int <- rowMeans(r[,c("fes_score", "cs_score")])
  r$fs_class <- rowMeans(r[,c("fcs_score", "fs_class_int")])
  r$lsg_fs <- round2(r$fs_class)
  
  # Sub-pillar SNFI
  r$basic_nfi <- rowSums(r[,c("nfi_priority_needs.bedding_items","nfi_priority_needs.mattresses_sleeping_mats",
                              "nfi_priority_needs.blankets", "nfi_priority_needs.cooking_utensils_kitchen_set",
                              "nfi_priority_needs.cooking_stove", "nfi_priority_needs.source_of_light",
                              "nfi_priority_needs.fuel_storage", "nfi_priority_needs.cool_box",
                              "nfi_priority_needs.water_storage", "nfi_priority_needs.fan",
                              "nfi_priority_needs.winter_heaters", "nfi_priority_needs.clothing",
                              "nfi_priority_needs.awc", 
                              "nfi_priority_needs.cooking_fuel",
                              "nfi_priority_needs.heating_fuel")])
  r$g85 <- ifelse(r$basic_nfi >= 2, 1, 0)
  r$a16 <- ifelse(r$shelter_type %in% c("unfinished_abandoned_building", "damaged_building", "tent", 
                                        "religious_building", "public_building", "non_residential", 
                                        "container", "makeshift_shelter") | !is.na(r$camp), 3, 0)
  r <- r %>% mutate(g89 = case_when(
    shelter_better.none == 1 ~ 0,
    shelter_better.improve_safety == 1 | shelter_better.improve_structure == 1 ~ 4,
    shelter_better.improve_privacy == 1 | shelter_better.protect_climate == 1 ~ 3,
    shelter_better.protec_hazards == 1 | shelter_better.improve_tenure == 1 | 
      shelter_better.improve_infrastructure == 1 ~ 2,
    shelter_better.none == 0 ~ 0
  ))
  
  r$lsg_snfi <- rowSums(r[,c("g85","g89", "a16")])
  r <- r %>% mutate(lsg_snfi = case_when(
    lsg_snfi <= 3 ~ 1,
    lsg_snfi == 4 ~ 2,
    lsg_snfi == 5 ~ 3,
    lsg_snfi >= 6 ~ 4,
    TRUE ~ lsg_snfi
  ))
  
  # Sub-pillar Health
  r$g25 <- ifelse(r$distance_health_service %in% c("between_2km_5km", "within_2km"), 0, 2)
  r$g26 <- ifelse(r$distance_hospital %in% c("between_2km_5km", "between_6km_10km", "within_2km") &
                    r$hospital_emergency_ser == "yes" &
                    r$hospital_maternity_ser == "yes" &
                    r$hospital_surgical_ser == "yes" &
                    r$hospital_pediatric_ser == "yes", 0, 2)
  r$g32 <- ifelse(r$women_specialised_services == "yes", 1, 0)
  
  
  r$lsg_health <- rowSums(r[,c("g25","g26","g32")])
  r <- r %>% mutate(lsg_health = case_when(
    lsg_health == 0 ~ 1,
    lsg_health == 1 ~ 1,
    lsg_health == 2 ~ 2,
    lsg_health == 3 ~ 3, 
    lsg_health == 4 ~ 4))
  
  # Sub-pillar Protection
  r <- r %>% mutate(g68 = case_when(
    hh_dispute == "yes" ~ 1,
    hh_dispute != "yes" ~ 0
  ))
  r$g51 <- ifelse(rowSums(r[,c("pds","info_card","id_card_a18","id_card_u18","birth_cert_a18",
                               "birth_cert_u18","citizenship_a18","citizenship_u18")] == "no") > 0, 1, 0)
  r <- r %>% mutate(g54 = case_when(
    restriction_clearance == "yes" | restriction_documents == "yes" | restriction_time == "yes" | 
      restriction_reason == "yes" | restriction_physical == "yes" | restriction_other == "yes" ~ 1,
    !(restriction_clearance == "yes" | restriction_documents == "yes" | restriction_time == "yes" | 
        restriction_reason == "yes" | restriction_physical == "yes" | restriction_other == "yes") ~ 0,
    TRUE ~ 1 # In camp there was another question before if they could leave the camp at all, if no the restriction question was not asked.
  ))
  r$g63 <- ifelse(rowSums(r[,c("unsafe_areas.facilities", "unsafe_areas.water_points",
                               "unsafe_areas.social_areas", "unsafe_areas.distribution_areas",
                               "unsafe_areas.markets", "unsafe_areas.way_to_centers",
                               "unsafe_areas.way_to_school")], na.rm=T) > 0, 1, 0)
  r$a13 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop$age[which(loop$X_submission__uuid == x["X_uuid"])] < 18 & 
                 loop$work[which(loop$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 0)
  })
  r$a12 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop_children$marital_status[which(loop_children$X_submission__uuid == x["X_uuid"])] == "married"), 1, 0)
  })
  r$g7a <- ifelse(r$no_school_no_docs == "yes", 1, 0)
  r$g64 <- ifelse(r$hh_risk_eviction == "yes", 1, 0)
  r$g64 <- ifelse(is.na(r$hh_risk_eviction), 0, r$g64)
  r$g65 <- ifelse(r$hh_member_distress == "yes", 1, 0)
  
  r$lsg_protection <- rowSums(r[,c("g68","g51","g54","g63","a13","a12","g7a","g64","g65")])
  r <- r %>% mutate(lsg_protection = case_when(
    lsg_protection <= 1 ~ 1,
    lsg_protection == 2 ~ 2,
    lsg_protection == 3 ~ 3,
    lsg_protection >= 4 ~ 4,
    TRUE ~ lsg_protection
  ))
  
  # Sub-pillar EL
  r$g44 <- apply(r, 1, FUN=function(x){
    ifelse(any(loop$age[which(loop$X_submission__uuid == x["X_uuid"])] > 17 & 
                 loop$work[which(loop$X_submission__uuid == x["X_uuid"])] == "no" & 
                 loop$actively_seek_work[which(loop$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 0)
  })
  r$b1 <- ifelse(rowSums(r[, c("inc_employment", "inc_pension")], na.rm=T) < 480000, 1, 0)
  r$b2 <- ifelse(r$primary_livelihood.ngo_charity_assistance == 1 & 
                   rowSums(r[,which(startsWith(names(r), "primary_livelihood."))]) == 1, 1, 0)
  r$g37 <- ifelse(r$how_much_debt > 505000, 1, 0)
  r$g38 <- ifelse(r$reasons_for_debt %in% c("basic_hh_expenditure", "education", "food", "health"), 1, 0)
  
  r$lsg_el <- round2(rowSums(r[,c("g44", "b1", "b2", "g37", "g38")]) / 5 * 3)+ 1 # TO BE DISCUSSED

  #LSG Education
  r$perc_edu <- apply(r, 1, FUN=function(x){
    (loop %>% filter(X_submission__uuid == x["X_uuid"] & (attend_formal_ed == "yes" | attend_informal_ed == "yes")) %>% nrow) / 
      (loop %>% filter(X_submission__uuid == x["X_uuid"] & attend_formal_ed != "") %>% nrow)
  })
  r$lsg_ed <- ifelse(r$perc_edu == 1, 1, 0)
  r$lsg_ed <- ifelse(r$perc_edu >= 0.75 & r$perc_edu < 1, 2, r$lsg_ed)
  r$lsg_ed <- ifelse(r$perc_edu >= 0.5 & r$perc_edu < 0.75, 3, r$lsg_ed)
  r$lsg_ed <- ifelse(r$perc_edu < 0.5, 4, r$lsg_ed)

  r$lsg_ed <- round2(r$lsg_ed)
  
  
  # Sub-pillar WASH
  r$g94 <- ifelse(r$access_private_shared_watertank == "no", 2.5, ifelse(r$tank_capacity * r$refill_times / r$people_share_tank / 7 < 50, 2.5, 0))
  r$g95 <- ifelse(r$drinking_water_source.network_private == 1 | r$drinking_water_source.network_comm == 1 | 
                    r$drinking_water_source.borehole == 1 | r$drinking_water_source.prot_well == 1 | 
                    r$drinking_water_source.prot_tank == 1 | r$drinking_water_source.prot_spring == 1 |
                    r$drinking_water_source.bottled_water == 1, 0, 1.5)
  r$g96 <- ifelse(r$treat_drink_water_how == "not_necessary", 0, ifelse(r$treat_drink_water_how == "", NA, 2.5))
  r$g97 <- ifelse(rowSums(r[, c("latrines.flush", "latrines.vip_pit")], na.rm = T) == 0, 2, 0)
  r$g99 <- ifelse(r$access_hygiene_items == "no" | r$use_of_soap.handwashing == 0, 1.5, 0)

  r$lsg_wash <- round2(rowSums(r[,c("g94","g95","g96","g97","g99")]) / 10 * 3) + 1
  
  
#  # MSNI decision tree
#  r$msni <- apply(r, 1, function(x) {
#    step_1 <- as.numeric(max(unique(x[c("lsg_health", "lsg_protection", "lsg_snfi")])))
#    step_2 <- as.numeric(min(step_1, x["impact"]))
#    co_ex <- x[c("lsg_health", "lsg_protection", "lsg_snfi")] %>% as.numeric %>% table
#    co_ex2 <- which(co_ex == 2)
#    step_3 <- ifelse(length(co_ex2) == 0, 0, as.numeric(names(co_ex)[co_ex2])) %>% max(step_2)
#    step_4 <- as.numeric(max(step_3, max(x[c("lsg_fs", "lsg_el", "lsg_wash", "capacity_gap")])))
#    step_5 <- step_4 + ifelse(step_4 == 1 & max(x[c("lsg_health", "lsg_protection", "lsg_snfi")]) >= 3, 1, 0)
#    return(step_5)
#  })  
  
  r$lsg_snfi %>% table(useNA = "always")
  return(r)
}


