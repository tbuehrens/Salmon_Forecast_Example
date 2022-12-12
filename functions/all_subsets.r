all_subsets<-function(series,covariates,min,max){
  series<-series%>%
    ungroup()%>%
    dplyr::select(Year,runsize_obs,all_of(covariates))%>%
    filter(
      across(
        .cols = all_of(covariates),
        .fns = ~ !is.na(.x)
      )
    )
  
  vars<-list()
  AICc=c()
  formula=c()
  model_num = c()
  tmin<-ifelse(min==0,1,min)
  for(i in tmin:max){
    temp<-combn(covariates,i, simplify = FALSE)
    for(j in 1:length(temp)){
      vars[[length(vars) + 1]]<-temp[[j]]
    }
  }
  total = ifelse(min==0,length(vars)+1,length(vars))
  print(paste0("There are ",total," models to fit! Fitting model number:"))
  for(i in 1:length(vars)){
    print(paste0(i," out of ",total))
    #arma<-data.frame(NA,nrow=length(vars),ncol=7)
    #colnames(arma)<-c("p","d","q","P","D","Q")
    xreg<-series%>%
      ungroup%>%
      dplyr::select(all_of(vars[[i]]))%>%
      as.matrix()
    
    m1<-series%>%
      ungroup()%>%
      dplyr::select(runsize_obs)%>%
      unlist()%>%
      ts(frequency = 1)%>%
      auto.arima(lambda=0,seasonal = F, xreg = xreg)
    AICc[i]<-m1$aicc
    #arma[i,]<-arimaorder(m1)
    formula[i]<-paste0("runsize_obs ~ ",paste(all_of(vars[[i]]),collapse = " + "))
    model_num[i]<-i
  }
  if(min==0){
    i = total
    print(paste0(i," out of ", total))
    m1<-series%>%
      ungroup()%>%
      dplyr::select(runsize_obs)%>%
      unlist()%>%
      ts(frequency = 2)%>%
      auto.arima(lambda=0,seasonal = T, xreg = NULL)
    AICc[i]<-m1$aicc
    #arma[i,]<-arimaorder(m1)
    formula[i]<-"runsize_obs ~ 1"
    model_num[i]<-i
  }
  table<-as_tibble(data.frame(model_num,AICc,formula))%>%
    arrange(AICc)
  results<-list(vars,table)
  return(results)
}

