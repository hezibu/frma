#supp functions

par_dredge<-function (model, data, cores, subsets=NULL)
{
  
  if(!require("lme4")){
    install.packages("lme4")
    library("lme4")
  }
  
  if(!require("snow")){
    install.packages("snow")
    library("snow")
  }
  if(!require("MuMIn")){
    install.packages("MuMIn")
    library("MuMIn")
  }
  
  
  options(na.action = "na.fail")
  cat(sep="\n", "Setting up cluster")
  cat(sep="\n", "\n")
  clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
  clust <- try(makeCluster(getOption("cl.cores", cores), type = clusterType))
  clusterEvalQ(clust, library(lme4))
  clusterEvalQ(clust, library(stats4))
  clusterExport(clust, deparse(substitute(data)), envir=environment())
  clusterExport(clust, deparse(substitute(model)), envir=environment())
  if(is.null(subsets))
  {
    dredged_model<- pdredge(get(deparse(substitute(model)), envir=environment()), cluster = clust)
  }
  else
  {
    dredged_model<- pdredge(get(deparse(substitute(model)), envir=environment()), cluster = clust, subset(subset))
  }
  stopCluster(clust)
  return(dredged_model)
}

get_function <- function(dredge,fun){
  lapply(get.models(dredge,subset = delta < 2),fun)
}


get_full_model_select_random<-  function(data,parameter,random,degree){
  parameters <- c("zalien", "log_arena", "log_pred", "log_prey", "ratio", "poly(temp,degree)","log_duration",
                  "log_pred:zalien", "poly(temp,degree):zalien", "log_pred:ratio", "log_pred:poly(temp,degree)","log_prey:ratio",
                  "ratio:poly(temp,degree)","log_pred:log_prey","log_prey:poly(temp,degree)",
                  "log_duration:log_arena")
  full.model  <- lmer(formula = as.formula(paste(parameter, "~", paste(parameters,collapse = " + "),
                                                 random)),
                      data,na.action = "na.fail",REML = F,
                      control = lmerControl(optimizer ="Nelder_Mead")) 
}

first_order_model <-  function(data,parameter,random){
  parameters <- c("zalien", "log_arena", "log_pred", "log_prey", "ratio", "temp",
                  "log_pred:zalien", "temp:zalien", "log_pred:ratio", "log_pred:temp","log_prey:ratio",
                  "ratio:temp","log_pred:log_prey","log_prey:temp",
                  "log_pred:temp:ratio","log_pred:zalien:temp","log_pred:log_prey:ratio",
                  "log_prey:ratio:temp",
                  "log_pred:log_prey:temp","log_pred:log_prey:ratio:temp")
  full.model  <- lmer(formula = as.formula(paste(parameter, "~", paste(parameters,collapse = " + "),
                                                 random)),
                      data,na.action = "na.fail",REML = F,
                      control = lmerControl(optimizer ="Nelder_Mead"))
}

second_order_model <-  function(data,parameter,random){
  parameters <- c("zalien", "log_arena", "log_pred", "log_prey", "ratio", "poly(temp,2)",
                  "log_pred:zalien", "poly(temp,2):zalien", "log_pred:ratio", "log_pred:poly(temp,2)","log_prey:ratio",
                  "ratio:poly(temp,2)","log_pred:log_prey","log_prey:poly(temp,2)",
                  "log_pred:poly(temp,2):ratio","log_pred:zalien:poly(temp,2)","log_pred:log_prey:ratio",
                  "log_pred:log_prey:poly(temp,2)","log_pred:log_prey:ratio:poly(temp,2)","log_prey:ratio:poly(temp,2)")
  full.model  <- lmer(formula = as.formula(paste(parameter, "~", paste(parameters,collapse = " + "),
                                                 random)),
                      data,na.action = "na.fail",REML = F,
                      control = lmerControl(optimizer ="Nelder_Mead"))
}

second_order_model_no_ratio <-  function(data,parameter,random){
  parameters <- c("zalien", "log_arena", "log_pred", "log_prey", "poly(temp,2)",
                  "log_pred:zalien", "poly(temp,2):zalien", "log_pred:poly(temp,2)",
                  "log_pred:log_prey","log_prey:poly(temp,2)",
                  "log_pred:zalien:poly(temp,2)","log_pred:log_prey:poly(temp,2)")
  full.model  <- lmer(formula = as.formula(paste(parameter, "~", paste(parameters,collapse = " + "),
                                                 random)),
                      data,na.action = "na.fail",REML = F,
                      control = lmerControl(optimizer ="Nelder_Mead"))
} 


first_order_model_no_alien <-  function(data,parameter,random){
  parameters <- c("log_arena", "log_pred", "log_prey", "ratio", "temp",
                  "log_pred:ratio", "log_pred:temp","log_prey:ratio",
                  "ratio:temp","log_pred:log_prey","log_prey:temp",
                  "log_pred:temp:ratio","log_pred:log_prey:ratio",
                  "log_pred:log_prey:temp","log_pred:log_prey:ratio:temp")
  full.model  <- lmer(formula = as.formula(paste(parameter, "~", paste(parameters,collapse = " + "),
                                                 random)),
                      data,na.action = "na.fail",REML = F,
                      control = lmerControl(optimizer ="Nelder_Mead"))
}
