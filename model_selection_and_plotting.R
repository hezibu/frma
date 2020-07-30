source("data_wrangling.R")
source("get_models_a.R")
source("get_models_h.R")


AICc(best.model.a.1,best.model.a.2,best.model.a.ppmr,best.model.a.ppmr.2) %>%
  rownames_to_column(var = "model") %>%
  arrange(AICc) %>% 
  mutate(delta = `AICc` - first(AICc))

AICc(best.model.h.1,best.model.h.2,best.model.h.ppmr,best.models.h.ppmr.2) %>%
  rownames_to_column(var = "model") %>%
  arrange(AICc) %>% 
  mutate(delta = `AICc` - first(AICc))

tab_model(best.model.h.1)
tab_model(best.model.a.1)

fd$temp %>% quantile(c(0.05,0.95))
fd$log_pred %>% quantile(c(0.05,0.95))
fd$log_prey %>% quantile(c(0.05,0.95))
fd$ratio %>% quantile(c(0.05,0.95))
fd.ppmr$ppmr %>% quantile(c(0.05,0.95))

gridExtra::grid.arrange(
  plot_model(best.model.a.1, type = "pred",
             terms = c("temp [10,27]","log_pred [-4,6]"))+ggtitle("A")
  ,
  plot_model(best.model.a.1, type = "pred",
             terms = c("log_pred [-2,10]","log_prey [-8,5]","aspect_ratio [1.14, 2.6]"))+ggtitle("B")
) 

plot_model(best.model.h.1, type = "pred",
           terms = c("temp [10,27]","log_pred [-2,10]"))

plot_model(best.model.a.1, type = "pred",
           terms = c("temp [10,27]","alien"))


gridExtra::grid.arrange(
  plot_model(best.model.h.1, type = "pred",
             terms = c("temp [10:27]","log_pred [-4,6]"))+ggtitle("A")
  ,
  
  plot_model(best.model.h.1, type = "pred",
             terms = c("log_prey [-8,5]","temp [10,27]","aspect_ratio [1.14, 2.6]"))+ggtitle("B")
)

    plot_model(best.model.h.1, type = "pred",
           terms = c("temp [10,27]","log_prey [-8,5]","aspect_ratio [1.14, 2.6]"))+ggtitle("B")


plot_model(best.model.h.ppmr, type = "pred",
           terms = c("temp [10:27]","ppmr [3,12]"))

just.aliens <- filter(fd,alien == "Y") 

model.just.aliens <- update(best.model.a.1, ~ . - alien - alien:temp,data = just.aliens)


just.natives <- filter(fd,alien == "N") 

model.just.natives <- update(best.model.a.1, ~ . - alien - alien:temp,data = just.natives) 

tab_model(best.model.a.1,model.just.aliens,model.just.natives)

just.aliens.ppmr <- filter(fd.ppmr,alien == "Y") 

model.just.aliens.ppmr <- update(best.model.h.ppmr, ~ . - alien - alien:temp - alien:ppmr - alien:ppmr:temp,data = just.aliens.ppmr)


just.natives.ppmr <- filter(fd.ppmr,alien == "N") 

model.just.natives.ppmr <- update(best.model.h.ppmr, ~ . - alien - alien:temp - alien:ppmr - alien:ppmr:temp,data = just.natives.ppmr) 

tab_model(best.model.h.ppmr,model.just.aliens.ppmr,model.just.natives.ppmr)



just.fresh <- filter(fd,water == "Fresh") 

model.just.fresh <- update(best.model.h.1, ~ . - water,data = just.fresh)


just.marine <- filter(fd,water == "Marine") 

model.just.marine <- update(best.model.h.1, ~ . - water,data = just.marine) 

tab_model(best.model.h.1,model.just.fresh,model.just.marine)


