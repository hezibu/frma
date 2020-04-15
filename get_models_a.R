source("functions.R")

full.model.a.1 <- lmer(log_a ~ log_arena + water + temp*log_pred*log_prey*ratio + 
                                zalien * temp * log_pred + (1|source),fd,REML = F )
dredged.a.1 <- par_dredge(model = full.model.a.1,
                                 data = fd,
                                 cores = 7)
best.model.a.1 <- get.models(dredged.a.1,subset = delta==0)[[1]]

full.model.a.2 <- lmer(log_a ~ log_arena + water + poly(temp,2)*log_pred*log_prey*ratio + 
                         zalien * poly(temp,2) * log_pred + (1|source),fd,REML = F )
dredged.a.2 <- par_dredge(model = full.model.a.2,
                          data = fd,
                          cores = 7)
best.model.a.2 <- get.models(dredged.a.2,subset = delta==0)[[1]]



full.model.a.ppmr <- lmer(log_a ~ log_arena + water + temp * ppmr * ratio + 
                            zalien * ppmr * temp + (1|source),
                          fd.ppmr, REML = F)
ppmr.dredge <- par_dredge(model = full.model.a.ppmr,
                          data = fd.ppmr,
                          cores = 7)
best.model.a.ppmr <- get.models(ppmr.dredge,delta == 0)[[1]]
