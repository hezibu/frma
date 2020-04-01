a.forest <- fd %>%
  randomForest(log_h ~ log_pred + log_prey + zalien + ratio + temp, data = .,localImp = T)  

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
  
  
  
  
  
  
randomForestExplainer::explain_forest()

fd %>% mutate(zalien = as.numeric(as.factor(zalien)),
              temp2 = temp^2) %>% 
  randomForest(log_h ~ log_pred + log_prey + zalien + ratio + temp + temp2, data = .,importance=TRUE,proximity=TRUE)  %>%
randomForestExplainer::explain_forest()

library(relaimpo)
install.packages("corpcor")

relaimpo::calc.relimp(best.model.a)


h.forest <- fd %>%
  randomForest(log_h ~ log_pred + log_prey + zalien + ratio + temp, data = .,localImp = T)  
