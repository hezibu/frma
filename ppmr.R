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
best.model.a.ppmr <- get.models(ppmr_dredge,delta == 0)[[1]]

ppmr_full_model_h <- ppmr_model(fd1,"log_h"," + (1|source)")
ppmr_dredge_h <- par_dredge(model = ppmr_full_model_h,
                          data = fd1,
                          cores = 8)
best.model.h.ppmr <- get.models(ppmr_dredge_h,delta == 0)[[1]]

AICc(best.model.h,best.model.h.2,best.model.h.ppmr) %>% mutate(delta = `AICc` - first(AICc))
AICc(best.model.a,best.model.a.2,best.model.a.ppmr,best.model.a.no.alien) %>% rownames_to_column() %>% mutate(delta = `AICc` - first(AICc))
