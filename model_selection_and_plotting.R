source("data_wrangling.R")
source("get_models_a.R")
source("get_models_h.R")

AICc(best.model.a.1,best.model.a.2)

best.model.h.1 %>% summary()

AICc(best.model.a.1,best.model.a.2,best.model.a.ppmr) %>%
  rownames_to_column(var = "model") %>%
  mutate(delta = `AICc` - first(AICc))

AICc(best.model.h.1,best.model.h.2,best.model.h.ppmr) %>%
  rownames_to_column(var = "model") %>%
  mutate(delta = `AICc` - first(AICc))

tab_model(best.model.a.1,get.model s(dredged.a.1,subset = delta < 0.04)[[2]])
tab_model(best.model.h.1)


fd$temp %>% quantile(c(0.05,0.95))
fd$log_pred %>% quantile(c(0.05,0.95))
fd$log_prey %>% quantile(c(0.05,0.95))
fd$ratio %>% quantile(c(0.05,0.95))

gridExtra::grid.arrange(plot_model(best.model.a.1, type = "pred",
           terms = c("temp [10,27]","log_pred [-4,6]"))+ggtitle("A")
     ,
plot_model(best.model.a.1, type = "pred",
           terms = c("log_pred [-2,10]","log_prey [-8,5]","ratio [1.14, 2.6]"))+ggtitle("B")
) 

plot_model(best.model.h.1, type = "pred",
           terms = c("temp [10,27]","log_pred [-2,10]"))


gridExtra::grid.arrange(
plot_model(best.model.h.1, type = "pred",
           terms = c("temp [10,27]","log_pred [-4,6]"))+ggtitle("A")
,

plot_model(best.model.h.1, type = "pred",
           terms = c("log_prey [-8,5]","temp [10,27]","ratio [1.14, 2.6]"))+ggtitle("B")
)
