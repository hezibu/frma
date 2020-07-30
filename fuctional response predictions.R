functional_response <- function(a,h,density_vector) {
  return((a * density_vector) / (1 + a * h * density_vector))
}

qplot(0:100,functional_response(0.1,2,0:100),geom = "line")

density <- seq(0,1e5,100)
(predictions <- fd %>% 
    left_join(fd.ppmr) %>% 
    mutate(temp = temp + 0.5) %>% 
    mutate(new_log_a = predict(best.model.a.1,newdata = .),
           new_log_h = predict(best.model.h.ppmr.2,newdata = .),
           predator_size_group = cut(log_pred,2,labels = c("small predator","big predator")),
           prey_size_group = cut(log_prey,2,labels = c("small prey","big prey")),
           activity_level = cut(aspect_ratio,breaks = c(0,1.4,Inf),labels = c("sedentary","active"))) %>%
    group_by(predator_size_group,prey_size_group,activity_level) %>% 
    filter(n() > 1 ) %>% 
    summarize(n = n(),
              old_a = exp(mean(log_a)),
              old_a_sd = exp(sd(log_a)),
              old_h = exp(mean(log_h)),
              old_h_sd = exp(sd(log_h)),
              new_a = exp(mean(new_log_a)),
              new_h = exp(mean(new_log_h))))

plot_data <- predictions %>% 
  filter(n  > 1) %>% 
  group_by(predator_size_group,prey_size_group,activity_level) %>% 
  nest() %>% 
  mutate(plot = 
           pmap(.l = list(predator_size_group,prey_size_group,activity_level,data),
                .f = function(pred_s,prey_size,ar, f){
                  cbind(n = density,
                        old = functional_response(f$old_a,f$old_h,density),
                        new = functional_response(f$new_a,f$new_h,density)) %>% 
                    as_tibble() %>% 
                    ggplot()+
                    geom_line(aes(x = n, y = old),color = "black")+
                    geom_line(aes(x = n, y = new),,color = "red")+
                    xlab("Prey Density") + ylab("Feeding Rate")+
                    theme(axis.text = element_blank(),axis.ticks = element_blank())+
                    ggtitle(stringr::str_glue("{ar} {pred_s} feeding on {prey_size}, n = {f$n}"))
                }))



ggpubr::ggarrange(plotlist = plot_data$plot,ncol = 3,nrow = 2)
