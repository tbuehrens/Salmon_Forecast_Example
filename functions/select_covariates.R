select_covariates<-function(all_vars){
  ## test individual covariates using auto.arima()
  AICcs<-data.frame(variable=all_vars,AICc=NA,Order=NA)
  for(i in 1:length(all_vars)){
    mod_fit<-tsCV2(y=log(dat$runsize_obs[-dim(dat)[1]]),
                   Year=dat$Year,
                   xreg=dat%>%dplyr::select(all_of(all_vars[i]))%>%as.matrix(),
                   h=FY,
                   min=TY,
                   type="ARIMA_auto"
    )
    AICcs[i,2]<-mod_fit$fit$aicc
    AICcs[i,3]<-paste0(arimaorder(mod_fit$fit),collapse="")
  } 
  AICcs$deltaAICc<-AICcs$AICc-min(AICcs$AICc)
  ## get best three predictor variables
  vars<-AICcs%>%arrange(desc(-deltaAICc))%>%slice(1:3)
  vars<-vars$variable
  ## all combinations of best three predictor variables
  covars<-list(c(vars[1]),
               c(vars[2]),
               c(vars[3]),
               c(vars[1],vars[2]),
               c(vars[1],vars[3]),
               c(vars[2],vars[3]),
               c(vars[1],vars[2],vars[3]))
  ## test all combinations of 1-3 predictors
  AICc_tab<-c(NULL)
  for(i in 1:length(covars)){
    mod_fit<-tsCV2(y=log(dat$runsize_obs[-dim(dat)[1]]),
                   Year=dat$Year,
                   xreg=dat%>%dplyr::select(all_of(covars[[i]]))%>%as.matrix(),
                   h=FY,
                   min=TY,
                   type="ARIMA_auto"
    )
    AICc_tab[i]<-mod_fit$fit$aicc
  } 
  ## return the best combination of predictors
  index<-which(AICc_tab==min(AICc_tab))
  covars_used<-as.character(unlist(covars[index])) 
  # results<-list(covars=covars_used)
  return(covars_used)
}
