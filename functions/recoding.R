recoding_mcna <- function(r) {
  r <- r %>% 
    new_recoding(source=why_not_return, target=G73) %>% 
    recode_to(1, where=why_not_return.presence_of_mines == 1) %>% 
    recode_to(0, where=why_not_return.presence_of_mines == 0) %>%
    
    new_recoding(source=hh_dispute, target=G68) %>% 
    recode_to(1, where.selected.exactly = "yes",
              otherwise.to = 0) %>% 
    
    new_recoding(target=G51) %>% 
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
    
    new_recoding(target=G51_sub) %>% 
    recode_to(1, where = passport_u18 == "no" | id_card_u18 == "no" | citizenship_u18 == "no" | birth_cert_u18 == "no" | 
                marriage_cert_u18 == "non_valid" | divorce_cert_u18 == "non_valid") %>% 
    recode_to(0, where = ! (passport_u18 == "no" | id_card_u18 == "no" | citizenship_u18 == "no" | birth_cert_u18 == "no" | 
                              marriage_cert_u18 == "non_valid" | divorce_cert_u18 == "non_valid")) %>% 
    
    new_recoding(target=G54) %>% 
    recode_to(1, where = restriction_clearance == "yes" | restriction_documents == "yes" | restriction_time == "yes" | 
                restriction_reason == "yes" | restriction_physical == "yes" | restriction_other == "yes") %>% 
    recode_to(0, where = !(restriction_clearance == "yes" | restriction_documents == "yes" | restriction_time == "yes" | 
                             restriction_reason == "yes" | restriction_physical == "yes" | restriction_other == "yes")) %>% 
    
    new_recoding(target=G63, source = unsafe_areas) %>% 
    recode_to(0, where.selected.exactly = "none",
              otherwise.to = 1) %>% 
    
    
    end_recoding
  r$G51_alt <- apply(r, 1, FUN=function(x){#print(which(r$X_uuid==x[,"X_uuid"])) 
                                    sum(x[c("pds","info_card","death_certificate", "guardianship", "inheritance",
                                       "trusteeship", "passport_a18", "id_card_a18", "citizenship_a18", "birth_cert_a18",
                                       "school_cert_a18", "marriage_cert_a18", "divorce_cert_a18",
                                       "passport_u18", "id_card_u18", "citizenship_u18", "birth_cert_u18",
                                       "marriage_cert_u18", "divorce_cert_u18")] %in% c("no", "non_valid"))})
  r$G51a <- r$birth_cert_missing_amount_u1
  r$G51b <- r$id_card_missing_amount
  return(r)
}