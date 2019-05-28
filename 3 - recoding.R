
# horizontal operations

response <- response %>% 
  filter(!is.na(type_hh)) %>% 
  mutate(strata = paste0(district_mcna,type_hh))


r <- response %>%
  new_recoding(source=how_much_debt, target=hh_with_debt_value) %>% 
  recode_to(0.25,where.num.larger.equal = 505000,otherwise.to=0) %>% 
  
  new_recoding(target=hh_unemployed) %>% 
  recode_to(0 ,where=!(is.na(response$work) | is.na(response$actively_seek_work))) %>% 
  recode_to(0.5,where=(work == "no") & (actively_seek_work == "yes")) %>% 
  
  new_recoding(source=reasons_for_debt, target=hh_unable_basic_needs) %>% 
  recode_to(0.25, where.selected.any = c("health","food","education","basic_hh_expenditure"), otherwise.to=0) %>% 
  
  end_recoding %>% 
  mutate(score_livelihoods = hh_with_debt_value+hh_unemployed+hh_unable_basic_needs)