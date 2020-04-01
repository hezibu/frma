# k fold

fd_kf <- fd
k <- 10
nrow(fd)/k

group <- function(fd,k){
  grouping <- vector(mode = "numeric",length = nrow(fd))
  grouping[1:(nrow(fd)-1)] <- rep(seq(1:k),each = nrow(fd)/k)
  grouping[nrow(fd)] <- sample(1:k, size = 1)
  grouping <- sample(grouping)
  return(grouping)
}


result.h2 <- vector(mode = "numeric", length = 100)
for (j in seq_len(100)){
  print(j)
  fd_kf <- fd
fd_kf <- fd_kf %>% mutate(grouping = group(fd = .,k = k)) %>% 
  group_by(grouping) %>% 
  nest()


cross_validation <- vector(mode = "list", length = k)
for (i in seq_len(k)){
  group_out <- fd_kf[i,] %>% ungroup %>% dplyr::select(-grouping) %>% unnest(cols = c(data))
  all_others <- fd_kf[-i,] %>% ungroup %>% dplyr::select(-grouping) %>% unnest(cols = c(data))
  train_model <- update(best.model.h.2,data = all_others)
  group_out <- group_out %>% mutate(fit = predict(train_model,group_out,re.form = ~0))
  cross_validation[[i]] <- caret::postResample(pred  = group_out$fit,obs = group_out$log_h)
}

result.h2[[j]] <- map(cross_validation,1) %>% as.numeric() %>% mean()
}

result.h <- vector(mode = "numeric", length = 100)
for (j in seq_len(100)){
  print(j)
  fd_kf <- fd
  fd_kf <- fd_kf %>% mutate(grouping = group(fd = .,k = k)) %>% 
    group_by(grouping) %>% 
    nest()
  
  
  cross_validation <- vector(mode = "list", length = k)
  for (i in seq_len(k)){
    group_out <- fd_kf[i,] %>% ungroup %>% dplyr::select(-grouping) %>% unnest(cols = c(data))
    all_others <- fd_kf[-i,] %>% ungroup %>% dplyr::select(-grouping) %>% unnest(cols = c(data))
    train_model <- update(best.model.h,data = all_others)
    group_out <- group_out %>% mutate(fit = predict(train_model,group_out,re.form = ~0))
    cross_validation[[i]] <- caret::postResample(pred  = group_out$fit,obs = group_out$log_h)
  }
  
  result.h[[j]] <- map(cross_validation,1) %>% as.numeric() %>% mean()
}




t.test(result.h2,result.h)


result.a2 <- vector(mode = "numeric", length = 100)
for (j in seq_len(100)){
  print(j)
  fd_kf <- fd
  fd_kf <- fd_kf %>% mutate(grouping = group(fd = .,k = k)) %>% 
    group_by(grouping) %>% 
    nest()
  
  
  cross_validation <- vector(mode = "list", length = k)
  for (i in seq_len(k)){
    group_out <- fd_kf[i,] %>% ungroup %>% dplyr::select(-grouping) %>% unnest(cols = c(data))
    all_others <- fd_kf[-i,] %>% ungroup %>% dplyr::select(-grouping) %>% unnest(cols = c(data))
    train_model <- update(best.model.a.2,data = all_others)
    group_out <- group_out %>% mutate(fit = predict(train_model,group_out,re.form = ~0))
    cross_validation[[i]] <- caret::postResample(pred  = group_out$fit,obs = group_out$log_a)
  }
  
  result.a2[[j]] <- map(cross_validation,1) %>% as.numeric() %>% mean()
}

result.a <- vector(mode = "numeric", length = 100)
for (j in seq_len(100)){
  print(j)
  fd_kf <- fd
  fd_kf <- fd_kf %>% mutate(grouping = group(fd = .,k = k)) %>% 
    group_by(grouping) %>% 
    nest()
  
  
  cross_validation <- vector(mode = "list", length = k)
  for (i in seq_len(k)){
    group_out <- fd_kf[i,] %>% ungroup %>% dplyr::select(-grouping) %>% unnest(cols = c(data))
    all_others <- fd_kf[-i,] %>% ungroup %>% dplyr::select(-grouping) %>% unnest(cols = c(data))
    train_model <- update(best.model.a,data = all_others)
    group_out <- group_out %>% mutate(fit = predict(train_model,group_out,re.form = ~0))
    cross_validation[[i]] <- caret::postResample(pred  = group_out$fit,obs = group_out$log_a)
  }
  
  result.a[[j]] <- map(cross_validation,1) %>% as.numeric() %>% mean()
}




t.test(result.a,result.a2)
