# Big Predator
mean_traits <- data_for_analysis %>% 
  summarize_if(.predicate = is.numeric, .funs = mean)

mean_traits$water = "Fresh"
mean_traits$alien = "N"  

mean_traits$log_pred <- 10
mean_traits$log_a <- predict(best.model.a.1,newdata = mean_traits,re.form = ~0)
mean_traits$log_h <- predict(best.model.h.1,newdata = mean_traits,re.form = ~0)

mean_traits <- mean_traits %>% 
  mutate(temp = temp + 1) %>%
  mutate(new_log_a = predict(best.model.a.1,newdata = .,re.form = ~0),
         new_log_h = predict(best.model.h.1,newdata = .,re.form = ~0),
         old_a = exp(mean(log_a)),
         old_h = exp(mean(log_h)),
         new_a = exp(mean(new_log_a)),
         new_h = exp(mean(new_log_h)))


density <- seq(0,1e5,1e2)
plot_3a1 <- cbind(n = density,
                 old = functional_response(mean_traits$old_a,mean_traits$old_h,density),
                 new = functional_response(mean_traits$new_a,mean_traits$new_h,density)) %>% 
  as_tibble() %>% 
  ggplot()+
  geom_line(size = 1.4, aes(x = n, y = old),color = "#2C409C")+
  geom_line(size = 1.4,aes(x = n, y = new),color = "#E5AE29")+
  xlab("Prey Density") + ylab("Feeding Rate")+
  ggtitle("A\nBig predator")

# Small Predator
mean_traits <- data_for_analysis %>% 
  summarize_if(.predicate = is.numeric, .funs = mean)

mean_traits$water = "Fresh"
mean_traits$alien = "N"  

mean_traits$log_pred <- -2
mean_traits$log_a <- predict(best.model.a.1,newdata = mean_traits,re.form = ~0)
mean_traits$log_h <- predict(best.model.h.1,newdata = mean_traits,re.form = ~0)

mean_traits <- mean_traits %>% 
  mutate(temp = temp + 1) %>%
  mutate(new_log_a = predict(best.model.a.1,newdata = .,re.form = ~0),
         new_log_h = predict(best.model.h.1,newdata = .,re.form = ~0),
         old_a = exp(mean(log_a)),
         old_h = exp(mean(log_h)),
         new_a = exp(mean(new_log_a)),
         new_h = exp(mean(new_log_h)))


density <- seq(0,1e5,1e2)
plot_3a2 <- cbind(n = density,
                 old = functional_response(mean_traits$old_a,mean_traits$old_h,density),
                 new = functional_response(mean_traits$new_a,mean_traits$new_h,density)) %>% 
  as_tibble() %>% 
  ggplot()+
  geom_line(size = 1.4,aes(x = n, y = old),color = "#2C409C")+
  geom_line(size = 1.4,aes(x = n, y = new),color = "#E5AE29")+
  xlab("Prey Density") + ylab("Feeding Rate")+
  ggtitle("\nSmall predator")

######################

# Active Species small prey:

mean_traits <- data_for_analysis %>% 
  summarize_if(.predicate = is.numeric, .funs = mean)

mean_traits$water = "Fresh"
mean_traits$alien = "N"  

mean_traits$log_prey <-  -8
mean_traits$aspect_ratio <- 2.6
mean_traits$log_a <- predict(best.model.a.1,newdata = mean_traits,re.form = ~0)
mean_traits$log_h <- predict(best.model.h.1,newdata = mean_traits,re.form = ~0)

mean_traits <- mean_traits %>% 
  mutate(temp = temp + 1) %>%
  mutate(new_log_a = predict(best.model.a.1,newdata = .,re.form = ~0),
         new_log_h = predict(best.model.h.1,newdata = .,re.form = ~0),
         old_a = exp(mean(log_a)),
         old_h = exp(mean(log_h)),
         new_a = exp(mean(new_log_a)),
         new_h = exp(mean(new_log_h)))


density <- seq(0,4e4,1e1)
plot_3b1 <- cbind(n = density,
      old = functional_response(mean_traits$old_a,mean_traits$old_h,density),
      new = functional_response(mean_traits$new_a,mean_traits$new_h,density)) %>% 
  as_tibble() %>% 
  ggplot()+
  geom_line(size = 1.4,aes(x = n, y = old),color = "#2C409C")+
  geom_line(size = 1.4,aes(x = n, y = new),color = "#E5AE29")+
  xlab("Prey Density") + ylab("Feeding Rate")+
  ggtitle("B\nActive predator feeding on small prey")

# Active Species Big Prey
mean_traits <- data_for_analysis %>% 
  summarize_if(.predicate = is.numeric, .funs = mean)

mean_traits$water = "Fresh"
mean_traits$alien = "N"  

mean_traits$log_prey <-  4
mean_traits$aspect_ratio <- 2.6
mean_traits$log_a <- predict(best.model.a.1,newdata = mean_traits,re.form = ~0)
mean_traits$log_h <- predict(best.model.h.1,newdata = mean_traits,re.form = ~0)

mean_traits <- mean_traits %>% 
  mutate(temp = temp + 1) %>%
  mutate(new_log_a = predict(best.model.a.1,newdata = .,re.form = ~0),
         new_log_h = predict(best.model.h.1,newdata = .,re.form = ~0),
         old_a = exp(mean(log_a)),
         old_h = exp(mean(log_h)),
         new_a = exp(mean(new_log_a)),
         new_h = exp(mean(new_log_h)))


density <- seq(0,9e4,1e1)
plot_3b2 <- cbind(n = density,
      old = functional_response(mean_traits$old_a,mean_traits$old_h,density),
      new = functional_response(mean_traits$new_a,mean_traits$new_h,density)) %>% 
  as_tibble() %>% 
  ggplot()+
  geom_line(size = 1.4,aes(x = n, y = old),color = "#2C409C")+
  geom_line(size = 1.4,aes(x = n, y = new),color = "#E5AE29")+
  xlab("Prey Density") + ylab("Feeding Rate")+
  ggtitle("\nActive predator feeding on big prey")


# Sedentary Species Small Prey

mean_traits <- data_for_analysis %>% 
  summarize_if(.predicate = is.numeric, .funs = mean)

mean_traits$water = "Fresh"
mean_traits$alien = "N"  

mean_traits$log_prey <- -8
mean_traits$aspect_ratio <- 1.14
mean_traits$log_a <- predict(best.model.a.1,newdata = mean_traits,re.form = ~0)
mean_traits$log_h <- predict(best.model.h.1,newdata = mean_traits,re.form = ~0)

mean_traits <- mean_traits %>% 
  mutate(temp = temp + 1) %>%
  mutate(new_log_a = predict(best.model.a.1,newdata = .,re.form = ~0),
         new_log_h = predict(best.model.h.1,newdata = .,re.form = ~0),
         old_a = exp(mean(log_a)),
         old_h = exp(mean(log_h)),
         new_a = exp(mean(new_log_a)),
         new_h = exp(mean(new_log_h)))


density <- seq(0,7e5,1e1)
plot_3b3 <- cbind(n = density,
      old = functional_response(mean_traits$old_a,mean_traits$old_h,density),
      new = functional_response(mean_traits$new_a,mean_traits$new_h,density)) %>% 
  as_tibble() %>% 
  ggplot()+
  geom_line(size = 1.4,aes(x = n, y = old),color = "#2C409C")+
  geom_line(size = 1.4,aes(x = n, y = new),color = "#E5AE29")+
  xlab("Prey Density") + ylab("Feeding Rate")+
  ggtitle("Sedentary predator feeding on small prey")

# Sedentary Species Big Prey
mean_traits <- data_for_analysis %>% 
  summarize_if(.predicate = is.numeric, .funs = mean)

mean_traits$water = "Fresh"
mean_traits$alien = "N"  

mean_traits$log_prey <- 4
mean_traits$aspect_ratio <- 1.14
mean_traits$log_a <- predict(best.model.a.1,newdata = mean_traits,re.form = ~0)
mean_traits$log_h <- predict(best.model.h.1,newdata = mean_traits,re.form = ~0)

mean_traits <- mean_traits %>% 
  mutate(temp = temp + 1) %>%
  mutate(new_log_a = predict(best.model.a.1,newdata = .,re.form = ~0),
         new_log_h = predict(best.model.h.1,newdata = .,re.form = ~0),
         old_a = exp(mean(log_a)),
         old_h = exp(mean(log_h)),
         new_a = exp(mean(new_log_a)),
         new_h = exp(mean(new_log_h)))


density <- seq(0,3e4,1e1)
plot_3b4 <- cbind(n = density,
      old = functional_response(mean_traits$old_a,mean_traits$old_h,density),
      new = functional_response(mean_traits$new_a,mean_traits$new_h,density)) %>% 
  as_tibble() %>% 
  ggplot()+
  geom_line(size = 1.4,aes(x = n, y = old),color = "#2C409C")+
  geom_line(size = 1.4,aes(x = n, y = new),color = "#E5AE29")+
  xlab("Prey Density") + ylab("Feeding Rate")+
  ggtitle("Sedentary predator feeding on big prey")

plot_list_3 <- list(plot_3a1, plot_3a2,
                    plot_3b1, plot_3b2,
                    plot_3b3, plot_3b4)

plot_list_3_theme <- lapply(plot_list_3,
                            function(p) {
                              p + 
                                theme_classic() +
                                my_theme +
                                theme(axis.title = element_blank(),
                                      axis.text = element_blank(),
                                      axis.ticks= element_blank(),
                                      plot.title = element_text(size = 18))
                            })

(figure_3 <- ggpubr::ggarrange(plotlist = plot_list_3_theme,
                  ncol = 2, nrow = 3) %>% 
  ggpubr::annotate_figure(
    left = ggpubr::text_grob("Feeding Rate", rot = 90, size = 22),
    bottom = ggpubr::text_grob("Prey Density",vjust = 0.3, size = 22))
)
#ggsave width 998 height 901