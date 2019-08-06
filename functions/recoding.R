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
  return(r)
}