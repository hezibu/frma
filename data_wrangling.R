source("required packages.R")

full_data <- read_csv(here("data","raw","fish_functional_response.csv"))


list_of_alien <- read_csv(here("data","raw","griis.csv")) %>% 
  filter(accepted_species_name %in% full_data$species) %>% 
  .$accepted_species_name %>% 
  unique()


full_data <- mutate(full_data,alien = ifelse(test = species %in% list_of_alien,
                                             yes = "Y",no = "N"),idd = rownames(full_data))

full_data <- left_join(full_data,read_csv(here("data","raw","ar_full_list.csv")),by = "species")

# full_data_without_meta <- full_data %>% 
#   select(source,predator_scientific_name,predator_mass,prey_mass,a,ci_a_lo,h,ci_h_lo,arena_size_3d,temperature,mean_aspect_ratio,alien,ppmr) 
# 
# full_data_without_meta <- full_data_without_meta %>% filter(complete.cases(.))
# 
# full_data_without_meta[complete.cases(full_data_without_meta),] %>% View()






full_data <- left_join(full_data,select(read_csv(here("data","raw","source_duration_locale.csv")),-units),by = c("source","species","prey_species")) %>% 
  distinct_all()


# 
# full_data <- full_data %>% left_join(select(read_csv(here("data","raw","duration.csv")),-duration),by = c("source","duration_notes","temp"))
# 
# full_data[is.na(full_data$duration),]$duration <- sapply(full_data[is.na(full_data$duration),]$probable_duration,function(x){
#   eval(parse(text = x))
# })

#manually filling duration
# full_data[is.na(full_data$duration)&full_data$source == "Miller et al 1992 CAN J FISH AQUAT SCI",]$duration <- sample(c(15,30),1)
# full_data[is.na(full_data$duration)&full_data$source == "Moustahfid et al 2010 OEC",]$duration <- 24*60 #this could be even a year, have to check
# full_data[is.na(full_data$duration),]$duration <- runif(1,1,100)

full_data %<>% mutate(duration_sec = duration*60,
                                   log_duration = log(duration_sec))

#manually filling prey_mass, not really sure about any of these.
full_data[is.na(full_data$prey_mass)&full_data$source == "Miller et al 1992 CAN J FISH AQUAT SCI",]$prey_mass <- 0.3022
full_data[is.na(full_data$prey_mass)&full_data$source == "Oyugi et al 2012 J OF THERMAL BIOLOGY",]$prey_mass <- 22 #11mg dry pretty confident
full_data[is.na(full_data$prey_mass)&full_data$source == "Murray et al 2016 HYDROBIOLOGIA",]$prey_mass <- 22 # same as oyugi
full_data[is.na(full_data$prey_mass)&full_data$source == "Alexander et al 2014 BIOL LETT",]$prey_mass <- 200 
full_data[is.na(full_data$prey_mass)&(full_data$source == "Letcher et al 1997 CJFAS"|
                                        full_data$source == "Ryer et al 2002 CJFAS"|
                                        full_data$source == "Gibson and Ezzi 1992 J OF FISH BIOLOGY"|
                                        full_data$source == "Vollset and Bailey 2011 J OF FISH BIOLOGY"|
                                        full_data$source == "S?rnes and Aksnes 2004 LIMNOLOGY AND OCEANOGRAPHY"),]$prey_mass <- 0.7
full_data[is.na(full_data$prey_mass)&full_data$source == "Ljunggren and Sandstr?m 2007 J OF FISH BIOLOGY",]$prey_mass <- 1
full_data[is.na(full_data$prey_mass)&full_data$source == "Cowan et al 2016 CORAL REEFS",]$prey_mass <- 0.0005
full_data[is.na(full_data$prey_mass)&full_data$source == "Moss and Beauchamp 2007 J FISH BIOL",]$prey_mass <- 0.0002
full_data[is.na(full_data$prey_mass)&full_data$source == "De Figueiredo et al 2007 J OF THE MARINE BIOL ASSOC OF THE UK",]$prey_mass <- 0.00015
daphnia_weight <- full_data %>% filter(major_grouping_prey_2 == "Cladoceran") %>% .$prey_mass
full_data[is.na(full_data$prey_mass)&full_data$source == "Koski and Johnson 2002",]$prey_mass <- sample(size = 1,x = daphnia_weight)
full_data[is.na(full_data$prey_mass)&full_data$source == "Murdoch et al 1975 ECOLOGY",]$prey_mass <- 3
	

#get wet weight for larvae:
full_data <- full_data %>% 
  mutate(pred_mass = if_else(str_detect(type, "Larva ")&!is.na(type),
                             pred_mass * 2,
                             pred_mass))

full_data <- full_data %>% 
  mutate(log_arena = log(arena_3d),
         log_pred = log(pred_mass),
         log_prey = log(prey_mass),
         log_a = log(a),
         log_h = log(h),
         log_se_a = log((a-a_low)/2),
         log_se_h = log((h-h_low)/2))

#get larva weight using linear model:
larva <- full_data %>% filter(str_detect(type, "arva")) %>% select(type,log_pred,idd)

larva <- larva %>% 
  mutate(age = parse_number(type),
         age_week = if_else(age > 5, ceiling(age/7),age),
         age_week = if_else(condition = is.na(age_week),
                            true = case_when(str_detect(type, " A") ~ 0,
                                             str_detect(type, " B") ~ 0.5,
                                             str_detect(type, " C") ~ 1,
                                             str_detect(type, " D") ~ 1.5,
                                             str_detect(type, " E") ~ 2,
                                             str_detect(type, " F") ~ 2.5), false = age_week))


larva %>% mutate(aa = ifelse(is.na(log_pred), "new","old")) %>% 
  mutate(log_pred = ifelse(is.na(log_pred), 4,log_pred)) %>% 
  ggplot()+
  aes(x = age_week, y = log_pred,color = aa)+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "lm")


mod <-  lm(log_pred ~ age_week,subset(larva, age_week <= 7.5),na.action = "na.omit")


rnorm(nobs(mod),mean=predict(mod),
      sd=summary(mod)$sigma)

larva <- larva %>% 
  mutate(log_pred = if_else(!is.na(log_pred)&!is.na(age_week),
                             log_pred,(predict(mod,newdata = .)+
                                         rnorm(1,mean = 0,sd = sqrt(summary(mod)$sigma)))))

full_data <- full_data %>% 
  left_join(select(larva,-c(age,age_week)),by = c("type","idd")) %>%
  mutate(log_pred = if_else(is.na(log_pred.x),
                            log_pred.y,
                            log_pred.x)) %>% #select(log_pred,log_pred.x,log_pred.y) %>% arrange(desc(log_pred)) %>% print(n=Inf)
  select(-c(log_pred.x,log_pred.y))


# missing aspect ratio for some... Let's fill those:
missingno <- full_data %>%
  filter(is.na(aspect_ratio)) %>% 
  select(species) %>% 
  distinct(species) %>% 
  filter(!str_detect(species,pattern = ","))
  
missingno <- missingno %>% mutate(ar = map(species, function(s) rfishbase::morphometrics(species_list = s,fields = "AspectRatio")))

missingno <- missingno %>%
  mutate(aspect_ratio = map(ar, function(x) mean(x$AspectRatio))) %>% 
  unnest(aspect_ratio) %>% 
  select(species,aspect_ratio)

full_data <- full_data %>% left_join(missingno, by = "species") %>% 
  mutate(aspect_ratio = if_else(is.na(aspect_ratio.x),
                                aspect_ratio.y,
                                aspect_ratio.x)) %>% 
  select(-c(aspect_ratio.x,aspect_ratio.y))

full_data[full_data$species == "Lophius americanus",]$aspect_ratio <- 1.088496012
full_data[full_data$species == "Anablepsoides hartii",]$aspect_ratio <- 1.807469658
full_data[full_data$species == "Chirostoma riojai",]$aspect_ratio <- 1.926893635
full_data[full_data$species == "Coregonus fontanae",]$aspect_ratio <- 2.074582504



#no:
#full_data[is.na(full_data$aspect_ratio)&full_data$species == "Chirostoma riojai",]$aspect_ratio <- 2.66

# full_data %>% write.csv(here("data","processed","data_for_analysis.csv"))

fd <- full_data %>% select(log_a,log_h,source,species,alien,log_pred,log_prey,aspect_ratio,temp,log_arena) %>% 
  filter(complete.cases(.)) %>% 
  rename(ratio = aspect_ratio,
         zalien = alien) %>% 
  # mutate(zalien = case_when(zalien == "Y" ~  TRUE,
  #                           zalien == "N" ~ FALSE)) %>% 
  filter(log_h > - 20) 


#check if invasives are in their native range:

# species_locale <- full_data %>% filter(alien == "Y") %>% select("species","country") %>%
#   dplyr::distinct()
# 
# invasion_points <- read_csv(here("data","raw","griis.csv")) %>% select(accepted_species_name,country_territory) %>%
#   rename(species = accepted_species_name,
#           country = country_territory) %>%
#   group_by(species) %>% nest() %>% mutate("list" = map(data,unlist)) %>% select(species,list)
# 
# lapply(species_locale$species, function(species){
#   species_locale[species_locale$species == species,]$country %in% invasion_points[invasion_points$species == species,]$list
# })
# 
# # looks like all obsvs are from native range...
# rm(species_locale,invasion_points)
# 
# invasion_points %>% 
#   filter(species == "Micropterus salmoides") %>% 
#   .$list
# 
