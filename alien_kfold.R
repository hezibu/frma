best.no.alien <- get.models(dredged.a.1,subset = delta < 0.1)[[2]]


fd_kf <- data_for_analysis
k <- 10
nrow(fd_kf)/k

group <- function(fd,k){
  grouping <- vector(mode = "numeric",length = nrow(fd))
  grouping[1:(nrow(fd)-1)] <- rep(seq(1:k),each = nrow(fd)/k)
  grouping[nrow(fd)] <- sample(1:k, size = 1)
  grouping <- sample(grouping)
  return(grouping)
}

result.no.alien <- vector(mode = "numeric", length = 100)
for (j in seq_len(100)){
  print(j)
  fd_kf <- data_for_analysis
  fd_kf <- fd_kf %>% mutate(grouping = group(fd = .,k = k)) %>% 
    group_by(grouping) %>% 
    nest()
  
  
  cross_validation <- vector(mode = "list", length = k)
  for (i in seq_len(k)){
    group_out <- fd_kf[i,] %>% ungroup %>% dplyr::select(-grouping) %>% unnest(cols = c(data))
    all_others <- fd_kf[-i,] %>% ungroup %>% dplyr::select(-grouping) %>% unnest(cols = c(data))
    train_model <- update(best.no.alien,data = all_others)
    group_out <- group_out %>% mutate(fit = predict(train_model,group_out,re.form = ~0))
    cross_validation[[i]] <- caret::postResample(pred  = group_out$fit,obs = group_out$log_a)
  }
  
  result.no.alien[[j]] <- map(cross_validation,1) %>% as.numeric() %>% mean()
}


tibble(a1 = result.a,
       a.no.alien = result.no.alien) %>% 
  gather(key = model,value = RMSE) %>% 
  ggplot()+
  aes(x = model, y = RMSE)+
  geom_violin()+
  stat_summary(geom = "point",fun.data =  "mean_se")+
  stat_summary(geom = "errorbar", fun.data =  "mean_se")
