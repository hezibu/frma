fd.forest <- fd %>% 
  mutate(alien = alien == "Y",
         water = water == "Fresh")

a.forest <- fd.forest %>%
  randomForest(log_a ~ log_pred + log_prey + aspect_ratio + temp + alien + water, data = .,localImp = T)  

randomForestExplainer::explain_forest(a.forest,interactions = TRUE)

fd.forest.ppmr <- fd.ppmr %>% 
  mutate(alien = alien == "Y",
         water = water == "Fresh")

a.forest.ppmr <- fd.forest.ppmr %>%
  randomForest(log_a ~ ppmr + aspect_ratio + temp + alien + water, data = .,localImp = T)  

randomForestExplainer::explain_forest(a.forest.ppmr,interactions = TRUE)


varImpPlot(a.forest)

min_depth_distribution(a.forest) %>% plot_min_depth_distribution(mean_sample = "relevant_trees")
min_depth_distribution(a.forest) %>% plot_min_depth_distribution(mean_sample = "top_trees")
min_depth_distribution(a.forest) %>% plot_min_depth_distribution(mean_sample = "all_trees")

measure_importance(a.forest) %>%
plot_importance_ggpairs 

(vars <- important_variables(a.forest, k = 5, measures = c("mean_min_depth", "no_of_trees")))
interactions_frame <- min_depth_interactions(a.forest, vars)
plot_min_depth_interactions(interactions_frame)

fd %>% mutate(zalien = as.numeric(as.factor(zalien))) %>% 
  plot_predict_interaction(a.forest, data = ., "log_prey", "temp")
  
  
  
  



relaimpo::calc.relimp(best.model.a,type = "lmg")


h.forest <- fd.forest %>%
  randomForest(log_h ~ log_pred + log_prey + aspect_ratio + temp + alien + water, data = .,localImp = T) 

randomForestExplainer::explain_forest(h.forest,interactions = TRUE)


h.forest.ppmr <- fd.forest.ppmr %>%
  randomForest(log_h ~ ppmr + aspect_ratio + temp + alien + water, data = .,localImp = T)  

randomForestExplainer::explain_forest(h.forest.ppmr,interactions = TRUE)