# MCNA_Analysys_Iraq

## NOW

- M: long template
- B: single result template

## NEXT
- M: map_to_table - numerics issue (M)
- ignore vars in dap generator

## TO DO



### Adjust inputs

- [x] update kobo tool: lowercase alphanumeric _ (B)
- [x] add cluster id to kobo tool (B)
- [ ] change questions


### Add to implementation

- threshold recoding
  - [ x ] example
  - [   ] complete recoding
  
- indicator recoding
  - [   ] with batch csv file example
  - [   ] complete

### Package features to add

- global MSNI tree (M)

### testing

- combine cluster & simple random sampling?  (M & B)
`
### Bugs to fix




### Clean up

- refactor main script (M & B)

### Individuals

- handle loops in kobo
  - koboloops package

```
individuals %>% 
    new_recoding(disability_binary) %>% 
    recode_to(1, where = sm_selected(disability, any = "yes"),otherwise.to = 0) 


# manual/step by step:
individuals_aggregated <- individuals %>% group_by("parent_UUID") %>% summarise(any_has_disability = max(disability_binary))

outer_join / inner_join

or

response$disability <- individuals_aggregated$any_has_disability[match(individuals$submission_uuid, hh$X_uuid)]

# with koboloops:

koboloops::affect_loop_to_parent(individuals, parent,max,uuid.name.loop = "parent_submission_uuid",uuid.name.parent = "X_uuid")



```

