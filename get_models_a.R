source("functions.R")


full.model.a.1 <- lmer(log_a ~ log_arena  + water +  temp*log_pred*log_prey*aspect_ratio + 
                         alien  * temp  + (1|source) + (1|prey_type_coarse),
                       data_for_analysis,REML = F )
dredged.a.1 <- par_dredge(model = full.model.a.1,
                          data = data_for_analysis,
                          cores = 7)
best.model.a.1 <- get.models(dredged.a.1,subset = delta==0)[[1]]

full.model.a.2 <- lmer(log_a ~ log_arena + water + poly(temp,2)*log_pred*log_prey*aspect_ratio + 
                         alien * poly(temp,2)  +
                         (1|source) + (1|prey_type_coarse),data_for_analysis,REML = F )
dredged.a.2 <- par_dredge(model = full.model.a.2,
                          data = data_for_analysis,
                          cores = 7)
best.model.a.2 <- get.models(dredged.a.2,subset = delta==0)[[1]]



full.model.a.ppmr <- lmer(log_a ~ log_arena + water + temp * ppmr * aspect_ratio + 
                            alien * temp + 
                            (1|source) + (1|prey_type_coarse),
                          data_for_analysis, REML = F)
ppmr.dredge <- par_dredge(model = full.model.a.ppmr,
                          data = data_for_analysis,
                          cores = 7)
best.model.a.ppmr <- get.models(ppmr.dredge,delta == 0)[[1]]


full.model.a.ppmr.2 <- lmer(log_a ~ log_arena + water + temp * poly(ppmr,2) * aspect_ratio + 
                            alien * temp + 
                            (1|source) + (1|prey_type_coarse),
                            data_for_analysis, REML = F)
ppmr.dredge.2 <- par_dredge(model = full.model.a.ppmr.2,
                          data = data_for_analysis,
                          cores = 7)
best.model.a.ppmr.2 <- get.models(ppmr.dredge.2,delta == 0)[[1]]
