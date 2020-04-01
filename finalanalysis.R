source("functions.R")
#attack rate

#species can be excluded from both models because it has low variacne of random effect


full.model.a.source.1 <- first_order_model(fd,"log_a"," + (1|source)")
dredged.a.source.1 <- par_dredge(model = full.model.a.source.1,
                                 data = fd,
                                 cores = 7)
model.a.source.1 <- get_function(dredged.a.source.1,summary)[[1]]
# get_function(dredged.a.source.1,function(m) plot_model(m))[[1]]
# get_function(dredged.a.source.1,function(m) plot_model(m,type = "pred"))[[1]]
# p<-get_function(dredged.a.source.1,function(m) plot_model(m,type = "int"))[[1]]
# p + theme(text = element_text(size = 30))+xlab("log(Predator Mass)")+ylab("log(Attack Rate)")+ggtitle(NULL)
# 

full.model.a.source.2 <- second_order_model(fd,"log_a"," + (1|source)")
dredged.a.source.2 <- par_dredge(model = full.model.a.source.2,
                                 data = fd,
                                 cores = 8)
model.a.source.2 <- get_function(dredged.a.source.2,summary)[[1]]

full.model.a.source.1.no.alien <- first_order_model_no_alien(fd,"log_a"," + (1|source)")
dredged.a.source.1.no.alien <- par_dredge(model = full.model.a.source.1.no.alien,
                                 data = fd,
                                 cores = 7)
a <- get_function(dredged.a.source.1.no.alien,summary)[[1]]
b <- stats::update(object = best.model.a, ~ . - (zalien + temp:zalien))

identical(a,b)

#handling time
fd <- fd %>% mutate(max = log(1/exp(log_h)))
full.model.h.source.1 <- first_order_model(fd,"log_h"," +  (1|source)")
dredged.h.source.1 <- par_dredge(model = full.model.h.source.1,
                                 data = fd,
                                 cores = 7)
model.h.source.1 <- get_function(dredged.h.source.1,summary)[[1]]
# get_function(dredged.h.source.1,function(m) plot_model(m))[[1]]
# get_function(dredged.h.source.1,function(m) plot_model(m,type = "pred"))[[1]]
# get_function(dredged.h.source.1,function(m) plot_model(m,type = "int"))[[1]]
# 
# 
# get.models(dredged.a.source.1,delta ==0)[[1]] %>% 
#   update(. ~ . -log_duration) %>% 
#   AICc()
# 
# View(dredged.a.source.1)

full.model.h.source.2 <- second_order_model(fd,"log_h"," + (1|source)")
full.model.h.source.2 <- update(full.model.h.source.2, ~ . + log_pred:log_prey:ratio:poly(temp,2))
dredged.h.source.2 <- par_dredge(model = full.model.h.source.2,
                                 data = fd,
                                 cores = 7)
model.h.source.2 <- get_function(dredged.h.source.2,summary)[[1]]


best.model.a <- get.models(dredged.a.source.1,subset = delta==0)[[1]]
best.model.a.2 <- get.models(dredged.a.source.2,subset = delta==0)[[1]]
best.model.h <- get.models(dredged.h.source.1,subset = delta==0)[[1]]
best.model.h.2 <- get.models(dredged.h.source.2,subset = delta==0)[[1]]

corvif(fd[,-c(1:4)])
car::vif(best.model.h)

plot_model(best.model.a,type = "diag")
plot_model(best.model.h.2,type = "diag")


plot_model(best.model.a, type = "pred", 
           terms = c("log_pred","log_prey [-7, 7]", "ratio [ 0.8, 3]"))

plot_model(best.model.a,type = "re")+theme_classic()+ggtitle("look at this")


plot_model(best.model.a,type = "pred",
           terms = c("temp","log_pred [ 0, 7, 14]"))

plot_model(best.model.a, type = "pred", 
           terms = c("log_pred", "temp [10, 16, 25]"))

plot_model(best.model.h, type = "int")

plot_model(best.model.h, type = "pred",
           terms = c("temp","ratio","log_prey [-10, 10]"))


plot_model(best.model.h.2,type = "pred",
           terms = c("log_pred [all]","temp [4, 29]"))

gridExtra::grid.arrange(grobs = plot_model(best.model.h,type = "pred",
           terms = c("log_pred [all]","temp [ 6 , 15 ,29]","ratio [0.8, 5]","log_prey [-10, 7]")),ncol = 1) %>% print
gridExtra::grid.arrange(grobs = plot_model(best.model.h.2,type = "pred",
                                           terms = c("log_pred [all]","temp [ 6 , 15 ,29]",
                                                     "ratio [0.8, 5]","log_prey [-10, 7]")),
                        ncol = 1) %>% print
plot_model(best.model.h,type = "pred",
           terms = c("log_pred [all]","log_prey","ratio"))

plot_model(best.model.h.2,type = "pred",
           terms = c("log_prey[all]","ratio [0.8, 2, 5]","temp [4, 15, 29]"))

plot_model(best.model.h,type = "pred",
           terms = c("log_pred[all]","ratio [0.8, 2, 5]","temp [4, 15, 29]"))

plot_model(best.model.h.2,type = "pred",
           terms = c("temp","ratio [0.8, 2, 5]","log_pred","log_prey"))



plot_model(best.model.h,type = "pred",
           terms = c("log_prey","ratio [[1, 3, 5]","temp [8, 25]"))

plot_model(best.model.h,type = "pred",
           terms = c("temp [8, 25]","ratio [0.8,2.7]","log_prey[-7, -1, 7]"))

plot_model(best.model.h, type = "pred",
           terms = c("log_prey","temp [10, 16, 25]","ratio [0.8, 2.7]"))


plot_model(best.model.h, type = "pred",
           terms = c("log_prey [-7, 7]","ratio [0.8,3]","temp [12, 19, 27]"))

plot_model(best.model.h, type = "pred",
           terms = c("temp [12, 27]","ratio [0.8,3]","log_prey [-7, 0, 7]"))

plot_model(best.model.h, type = "pred",
           terms = c("temp [12, 27]","ratio [0.8,3]"))

plot_model(best.model.h, type = "pred",
           terms = c("log_prey [all]","temp [12, 27]","ratio [3]"))

plot_model(best.model.h, type = "pred",
           terms = c("temp [12, 27]","log_prey [-7, 0, 7]","ratio [0.8,3]"))


##################################


plot_model(best.model.a,type = "pred",
           terms = c("temp","log_prey [-7]","ratio [0.8]","log_pred [-2]"))
plot_model(best.model.h,type = "pred",
           terms = c("temp","log_prey [-7]","ratio [0.8]","log_pred [-2]"))


plot_model(best.model.a,type = "pred",
           terms = c("temp","log_prey [7]","ratio [0.8]","log_pred [11]"))
plot_model(best.model.h,type = "pred",
           terms = c("temp","log_prey [7]","ratio [0.8]","log_pred [11]"))


plot_model(best.model.a,type = "pred",
           terms = c("temp","log_prey [5]","ratio [0.8]","log_pred [-1]"))
plot_model(best.model.h,type = "pred",
           terms = c("temp","log_prey [5]","ratio [0.8]","log_pred [-1]"))


plot_model(best.model.a,type = "pred",
           terms = c("temp","log_prey [-7]","ratio [3]","log_pred [0]"))
plot_model(best.model.h,type = "pred",
           terms = c("temp","log_prey [-7]","ratio [3]","log_pred [0]"))

plot_model(best.model.a,type = "pred",
           terms = c("temp","log_prey [10]","ratio [3]","log_pred [15]"))
plot_model(best.model.h,type = "pred",
           terms = c("temp","log_prey [10]","ratio [3]","log_pred [15]"))


plot_model(best.model.a,type = "pred",
           terms = c("temp","log_prey [-4]","ratio [3]","log_pred [10]"))
plot_model(best.model.h,type = "pred",
           terms = c("temp","log_prey [-4]","ratio [3]","log_pred [10]"))

fd %>% filter(ratio > 3) 



#####
fd %>% 
  ggplot()+
  aes(x = ratio, y = log_prey, color = cut(temp,breaks = c(0,12,21,30)))+
  geom_point(size = 2)+
  facet_wrap(~cut(temp,breaks = c(0,12,21,30)))

fd %>% 
  ggplot()+
  aes(x = log_pred, y = log_prey,color = cut(ratio,breaks = c(0,1.5,2.3,5)))+
  geom_point()

AICc(best.model.h,
     update(best.model.h, ~ . + poly(temp,2)),
     update(best.model.h, ~ . + poly(temp,2) + log_pred:poly(temp,2)),
     update(best.model.h, ~ . + poly(temp,2) + log_pred:poly(temp,2)+log_prey:poly(temp,2)),
     update(best.model.h, ~ . + poly(temp,2) + 
              log_pred:poly(temp,2)+log_prey:poly(temp,2) + log_prey:ratio:poly(temp,2)))

AICc(best.model.a,
     update(best.model.a, ~ . + poly(temp,2)))




fd %>% ggplot()+geom_point(aes(x = log(temp), y = log_h))

fd$log_pred %>% summary

fd %>% filter(ratio >= 3 ) %>% .$log_prey %>% min

average.a <- model.avg(object = get.models(dredged.a.source.1, subset = delta < 2))
average.h <-model.avg(object = get.models(dredged.h.source.1, subset = delta < 2))

summary(average.a)
summary(average.h)

fd %>% mutate(temp1 = cut(temp,breaks = 3)) %>% 
ggplot()+
  aes(x = log_pred, y = log_a,color = zalien)+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~temp1)

plot_model(average.a)

fd %>% mutate(fit = predict(best.model.a,newdata = fd)) %>% 
  ggplot()+
  aes(x = log_a, y = fit)+
  geom_point()+
  geom_abline(slope = 1,intercept = 0,size = 1.2, color = "grey")+
  coord_equal()



fd %>% mutate(fit = predict(best.model.h,newdata = fd)) %>% 
  ggplot()+
  aes(x = log_h, y = fit)+
  geom_point()+
  geom_abline(slope = 1,intercept = 0,size = 1.2, color = "grey")+
  coord_equal()


fd %>% ggplot()+
  aes(x = log_a)+geom_histogram()

fd %>% filter(log_h > -20)


fd %>% mutate(fit = predict(average.a,newdata = fd)) %>% mutate(temp1 = cut(temp, breaks = 3)) %>% 
  ggplot()+
  aes(x = log_prey, y = fit,color  = temp1)+
  geom_point()+
  geom_smooth(method = "lm")

randomForest(x = select(fd,temp,zalien,ratio,log_pred,log_prey), y = fd$log_a)
