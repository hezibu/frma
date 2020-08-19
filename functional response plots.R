functional_response <- function(a,h,density_vector) {
  return((a * density_vector) / (1 + a * h * density_vector))
}

(predictions <- data_for_analysis %>%  
    mutate(temp = temp + 1) %>% 
    mutate(new_log_a = predict(best.model.a.1,newdata = .),
           new_log_h = predict(best.model.h.ppmr.2,newdata = .),
           predator_size_group = cut(log_pred,2,labels = c("small predator","big predator")),
           prey_size_group = cut(log_prey,2,labels = c("small prey","big prey")),
           activity_level = cut(aspect_ratio,breaks = c(0,1.5,2,Inf),labels = c("sedentary","mild active","active"))) %>%
    group_by(predator_size_group,prey_size_group,activity_level) %>% 
    filter(n() > 1 ) %>% 
    summarize(n = n(),
              old_a = exp(mean(log_a)),
              old_a_sd = exp(sd(log_a)),
              old_h = exp(mean(log_h)),
              old_h_sd = exp(sd(log_h)),
              new_a = exp(mean(new_log_a)),
              new_h = exp(mean(new_log_h))))

density <- seq(0,9e4,1e2)
(p1 <- predictions[1,] %>% 
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
                      geom_line(aes(x = n, y = new),color = "red")+
                      xlab("Prey Density") + ylab("Feeding Rate")+
                      ggtitle(stringr::str_glue("{ar} {pred_s} feeding on {prey_size}, n = {f$n}"))
                  })
    ) %>% 
    .$plot %>% 
    .[[1]]
)

density <- seq(0,9e6,1e2)
(p2 <- predictions[2,] %>% 
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
                      geom_line(aes(x = n, y = new),color = "red")+
                      xlab("Prey Density") + ylab("Feeding Rate")+
                      ggtitle(stringr::str_glue("{ar} {pred_s} feeding on {prey_size}, n = {f$n}"))
                  })
    ) %>% 
    .$plot %>% 
    .[[1]]
)

density <- seq(0,3e6,1e2)
(p3 <- predictions[3,] %>% 
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
                      geom_line(aes(x = n, y = new),color = "red")+
                      xlab("Prey Density") + ylab("Feeding Rate")+
                      ggtitle(stringr::str_glue("{ar} {pred_s} feeding on {prey_size}, n = {f$n}"))
                  })
    ) %>% 
    .$plot %>% 
    .[[1]]
)

density <- seq(0,1e4,1e1)
(p4 <- predictions[4,] %>% 
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
                      geom_line(aes(x = n, y = new),color = "red")+
                      xlab("Prey Density") + ylab("Feeding Rate")+
                      ggtitle(stringr::str_glue("{ar} {pred_s} feeding on {prey_size}, n = {f$n}"))
                  })
    ) %>% 
    .$plot %>% 
    .[[1]]
)


density <- seq(0,1e4,1e1)
(p5 <- predictions[5,] %>% 
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
                      geom_line(aes(x = n, y = new),color = "red")+
                      xlab("Prey Density") + ylab("Feeding Rate")+
                      ggtitle(stringr::str_glue("{ar} {pred_s} feeding on {prey_size}, n = {f$n}"))
                  })
    ) %>% 
    .$plot %>% 
    .[[1]]
)

density <- seq(0,3e4,1e1)
(p6 <- predictions[6,] %>% 
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
                      geom_line(aes(x = n, y = new),color = "red")+
                      xlab("Prey Density") + ylab("Feeding Rate")+
                      ggtitle(stringr::str_glue("{ar} {pred_s} feeding on {prey_size}, n = {f$n}"))
                  })
    ) %>% 
    .$plot %>% 
    .[[1]]
)

density <- seq(0,1e3,1e1)
(p7 <- predictions[7,] %>% 
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
                      geom_line(aes(x = n, y = new),color = "red")+
                      xlab("Prey Density") + ylab("Feeding Rate")+
                      ggtitle(stringr::str_glue("{ar} {pred_s} feeding on {prey_size}, n = {f$n}"))
                  })
    ) %>% 
    .$plot %>% 
    .[[1]]
)

density <- seq(0,1e3,1e1)
(p8 <- predictions[8,] %>% 
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
                      geom_line(aes(x = n, y = new),color = "red")+
                      xlab("Prey Density") + ylab("Feeding Rate")+
                      ggtitle(stringr::str_glue("{ar} {pred_s} feeding on {prey_size}, n = {f$n}"))
                  })
    ) %>% 
    .$plot %>% 
    .[[1]]
)

density <- seq(0,4e2,1)
(p9 <- predictions[9,] %>% 
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
                      geom_line(aes(x = n, y = new),color = "red")+
                      xlab("Prey Density") + ylab("Feeding Rate")+
                      ggtitle(stringr::str_glue("{ar} {pred_s} feeding on {prey_size}, n = {f$n}"))
                  })
    ) %>% 
    .$plot %>% 
    .[[1]]
)

plot_list <- list(p1,p2,p3,p4,p5,p6,p7,p8,p9)

plot_list <- lapply(plot_list, function(plot) 
  plot +
    theme_classic()+
    theme(axis.text = element_blank(),axis.ticks = element_blank()))

ggpubr::ggarrange(plotlist = plot_list,ncol = 3,nrow = 3)
