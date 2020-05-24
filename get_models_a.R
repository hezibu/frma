source("functions.R")

full.model.a.1 <- lmer(log_a ~ log_arena + water +  temp*log_pred*log_prey*aspect_ratio + 
                         alien * temp * log_pred + (1|source) + (1|prey_type_coarse),
                       fd,REML = F )
dredged.a.1 <- par_dredge(model = full.model.a.1,
                          data = fd,
                          cores = 7)
best.model.a.1 <- get.models(dredged.a.1,subset = delta==0)[[1]]

full.model.a.2 <- lmer(log_a ~ log_arena + water + poly(temp,2)*log_pred*log_prey*aspect_ratio + 
                         alien * poly(temp,2) * log_pred +
                         (1|source) + (1|prey_type_coarse),fd,REML = F )
dredged.a.2 <- par_dredge(model = full.model.a.2,
                          data = fd,
                          cores = 7)
best.model.a.2 <- get.models(dredged.a.2,subset = delta==0)[[1]]



full.model.a.ppmr <- lmer(log_a ~ log_arena + water + temp * ppmr * aspect_ratio + 
                            alien * ppmr * temp + 
                            (1|source) + (1|prey_type_coarse),
                          fd.ppmr, REML = F)
ppmr.dredge <- par_dredge(model = full.model.a.ppmr,
                          data = fd.ppmr,
                          cores = 7)
best.model.a.ppmr <- get.models(ppmr.dredge,delta == 0)[[1]]
