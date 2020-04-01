fd1 <- full_data %>% 
  mutate(predator = exp(log_pred)) %>% 
  mutate(ppmr = log(predator/prey_mass)) %>% 
  select(log_a,log_h,source,species,alien,ppmr,aspect_ratio,temp,log_arena) %>% 
  filter(complete.cases(.)) %>% 
  rename(ratio = aspect_ratio,
         zalien = alien) %>% 
  # mutate(zalien = case_when(zalien == "Y" ~  TRUE,
  #                           zalien == "N" ~ FALSE)) %>% 
  filter(log_h > - 20)

ppmr_model <-  function(data,parameter,random){
  parameters <- c("zalien", "log_arena", "ppmr", "ratio", "temp",
                  "ppmr:zalien", "temp:zalien", "ppmr:ratio", "ppmr:temp",
                  "ratio:temp","ppmr:temp:ratio","ppmr:zalien:temp")
  full.model  <- lmer(formula = as.formula(paste(parameter, "~", paste(parameters,collapse = " + "),
                                                 random)),
                      data,na.action = "na.fail",REML = F,
                      control = lmerControl(optimizer ="Nelder_Mead"))
}

ppmr_full_model <- ppmr_model(fd1,"log_a"," + (1|source)")
ppmr_dredge <- par_dredge(model = ppmr_full_model,
                                 data = fd1,
                                 cores = 7)
ppmr_best <- get_function(ppmr_dredge,summary)[[1]]
best.model.a

get_function(ppmr_dredge,function(m) plot_model(m,type = "int"))[[1]]


fd %>% 
  ggplot()+
  aes(x = ratio,y = log_prey)+
  geom_point()+
  geom_density_2d()
