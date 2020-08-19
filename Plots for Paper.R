# Plots for paper

fd$temp %>% quantile(c(0.05,0.95))
fd$log_pred %>% quantile(c(0.05,0.95))
fd$log_prey %>% quantile(c(0.05,0.95))
fd$ratio %>% quantile(c(0.05,0.95))
fd.ppmr$ppmr %>% quantile(c(0.05,0.95))


colors = c("#2C409C","#E5AE29") #http://colorschemedesigner.com/csd-3.5/#3S21TqgsHP7BA

my_theme <- theme(plot.title = element_text(size = 20, face = "italic"),
                  axis.title = element_text(size = 18),
                  axis.text = element_text(size = 14),
                  legend.position = c(0.9,0.2),
                  legend.title = element_text(size = 21),
                  legend.text = element_text(size = 20),
                  legend.background = element_blank())


( # Plot 1A:
  # Temperature * Pred Size Interaction Effect
  plot_1a <- 
    plot_model(best.model.a.1, type = "pred",
               terms = c("temp [10,27]","log_pred [-2,10]"),colors = colors, alpha = 0.3) +
    ggtitle("A")+
    geom_line(size = 1.4)+
    theme_classic()+
    xlab("Temperature (degrees Celsius)")+
    ylab("Space Clearance Rate (log transformed)")+
    my_theme
)

(
  plot_1b_left <- 
    plot_model(best.model.a.1, type = "pred",
               terms = c("log_prey [-8,5]","log_pred [-2,10]","aspect_ratio [1.14]"),colors = colors, alpha = 0.3) +
    ggtitle(str_glue("B\nAspect Ratio = 1.14 (Sedentary)"))+
    geom_line(size = 1.4)+
    theme_classic()+
    scale_y_continuous(limits = c(-5.5,5.5),breaks = seq(-3,3,3))+
    xlab("Prey Size (log transformed)")+
    ylab("Space Clearance Rate (log transformed)")+
    my_theme + theme(legend.position = "none")
)

(
  plot_1b_right <- 
    plot_model(best.model.a.1, type = "pred",
               terms = c("log_prey [-8,5]","log_pred [-2,10]","aspect_ratio [2.6]"),colors = colors, alpha = 0.3) +
    ggtitle("\nAspect Ratio = 2.6 (Active)")+
    geom_line(size = 1.4)+
    theme_classic()+
    scale_y_continuous(limits = c(-5.5,5.5),breaks = seq(-3,3,3))+
    xlab("Predator Size (log transformed)")+
    ylab("Space Clearance Rate (log transformed)")+
    my_theme + theme(legend.position = "none",axis.title.y = element_blank())
)


fig1 <- ggpubr::ggarrange(plot_1a,
                          ggpubr::ggarrange(plot_1b_left,plot_1b_right,ncol = 2),
                          ncol = 1)

#ggsave(fig1,"fig1.tiff",width = 1081,height = 901)

(
  plot_2a <- 
    plot_model(best.model.h.1, type = "pred",
               terms = c("temp [10,27]","log_pred [-2,10]"),colors = colors, alpha = 0.3) +
    ggtitle("A")+
    geom_line(size = 1.4)+
    theme_classic()+
    xlab("Temperature (degrees Celsius)")+
    ylab("Handling Time (log transformed)")+
    my_theme+theme(legend.position = c(0.9,0.9))
)

(
  plot_2b_left <- 
    plot_model(best.model.h.1, type = "pred",
               terms = c("log_prey [-8,5]","temp [10,27]","aspect_ratio [1.14]"),colors = colors, alpha = 0.3) +
    ggtitle(str_glue("B\nAspect Ratio = 1.14 (Sedentary)"))+
    geom_line(size = 1.4)+
    theme_classic()+
    scale_y_continuous(limits = c(-19,-1))+
    xlab("Prey Size (log transformed)")+
    ylab("Handling Time (log transformed)")+
    my_theme + theme(legend.position = c(0.1,0.8))
)

(
  plot_2b_right <- 
    plot_model(best.model.h.1, type = "pred",
               terms = c("log_prey [-8,5]","temp [10,27]","aspect_ratio [2.6]"),colors = colors, alpha = 0.3) +
    ggtitle("\nAspect Ratio = 2.6 (Active)")+
    geom_line(size = 1.4)+
    theme_classic()+
    scale_y_continuous(limits = c(-19,-1))+
    xlab("Prey Size (log transformed)")+
    ylab("Handling Time (log transformed)")+
    my_theme + theme(legend.position = "none",axis.title.y = element_blank())
)

fig2 <- ggpubr::ggarrange(plot_2a,
                          ggpubr::ggarrange(plot_2b_left,plot_2b_right,ncol = 2),
                          ncol = 1)


(
  plot_s1a <- plot_model(best.model.a.1, type = "pred",
                         terms = c("temp [10,27]","alien"),colors = colors, alpha = 0.3) +
    ggtitle("A")+
    geom_line(size = 1.4)+
    theme_classic()+
    xlab("Temperature (degrees Celsius)")+
    ylab("Space Clearance Rate (log transformed)")+
    my_theme + theme(legend.direction = "horizontal",legend.position = c(0.9,0.1))
)

(
  plot_s1b <-     tibble(a1 = result.a,
                         a.no.alien = result.no.alien) %>% 
    gather(key = model,value = RMSE) %>% 
    ggplot()+
    aes(x = model, y = RMSE,fill = model)+
    geom_violin()+
    stat_summary(geom = "point",fun.data =  "mean_se")+
    stat_summary(geom = "errorbar", fun.data =  "mean_se")+
    theme_classic()+scale_fill_manual(values = colors)+
    my_theme+theme(axis.title.x = element_blank(),legend.position = "none")+
    scale_x_discrete(breaks = c("a.no.alien","a1"),
                     labels = c("Model without alien", "Model with alien"))+
    ggtitle("B")
  
)

fig1s <- ggpubr::ggarrange(plot_s1a,plot_s1b,ncol = 1)
