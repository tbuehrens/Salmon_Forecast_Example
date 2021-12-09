#fitting wrapper for brms
fc3<-function(TY,Year,vars,dat,formula,prior){
  TY=TY
  trainingset<-list(NULL)
  validationset<-list(NULL)
  indexes<-TY:c(dim(dat)[1]-1)
  for(i in 1:length(indexes)){
    trainingset[[i]] <- data.frame(dat[1:indexes[i],])
    validationset[[i]] <- data.frame(dat[indexes[i]+1,])
  }
  prior=prior
  mod<- brm_multiple(
    formula = formula
    ,family = gaussian()
    ,data = trainingset
    ,chains = 4
    ,cores = 4
    ,iter = 2000
    ,warmup = 1000
    ,control=list(adapt_delta = 0.99)
    ,prior=prior
    ,combine = FALSE
  )
  preds<-data.frame(matrix(NA,nrow=dim(dat)[1],ncol=4))
  for(i in 1:length(indexes)){
    preds[indexes[i]+1,] = predict(mod[[i]],newdata=validationset[[i]])
  }
  colnames(preds)<-c("Estimate","SE","L95","U95")
  preds<-preds%>%
    bind_cols(data.frame(Year=Year))%>%
    dplyr::select(Year,Estimate,SE,L95,U95)
  
  results<-list(results=preds,fit=mod)
  return(results)
}
