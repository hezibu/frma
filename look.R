look <- full_data %>% mutate(prey_species = if_else(condition = !is.na(prey_species),prey_species,"not available")) %>% 
  select(log_a,log_h,source,species,prey_species,alien,log_pred,log_prey,aspect_ratio,temp,log_arena) %>% 
  filter(complete.cases(.)) %>% 
  rename(ratio = aspect_ratio,
         zalien = alien) %>% 
  # mutate(zalien = case_when(zalien == "Y" ~  TRUE,
  #                           zalien == "N" ~ FALSE)) %>% 
  filter(log_h > - 20)


look %>% count(species,prey_species) %>% View()

full_data %>% select(log_a,log_h,source,species,alien,log_pred,log_prey,aspect_ratio,temp,log_arena) %>% 
  filter(complete.cases(.)) %>% 
  rename(ratio = aspect_ratio,
         zalien = alien) %>% 
  # mutate(zalien = case_when(zalien == "Y" ~  TRUE,
  #                           zalien == "N" ~ FALSE)) %>% 
  filter(log_h > - 20) 


full_data %>% select(log_a,water,log_h,source,species,alien,log_pred,log_prey,aspect_ratio,temp,log_arena) %>% 
  filter(complete.cases(.)) %>% 
  rename(ratio = aspect_ratio,
         zalien = alien) %>% 
  # mutate(zalien = case_when(zalien == "Y" ~  TRUE,
  #                           zalien == "N" ~ FALSE)) %>% 
  filter(log_h > - 20) %>% 
  count(water)
