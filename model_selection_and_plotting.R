source("data_wrangling.R")
source("get_models_a.R")
source("get_models_h.R")


AICc(best.model.a.1,best.model.a.2,best.model.a.ppmr) %>%
  rownames_to_column(var = "model") %>%
  arrange(AICc) %>% 
  mutate(delta = `AICc` - first(AICc))

AICc(best.model.h.1,best.model.h.2,best.model.h.ppmr) %>%
  rownames_to_column(var = "model") %>%
  arrange(AICc) %>% 
  mutate(delta = `AICc` - first(AICc))

tab_model(best.model.h.1)
tab_model(best.model.a.1)

