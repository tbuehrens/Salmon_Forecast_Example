#one step ahead cross validation
tsCV2<-function(y,Year,xreg,type,min,h, ...){
  forecast<-data.frame(matrix(NA,ncol=5,nrow=1))
  colnames(forecast)<-c("Year","Estimate","SE","L95","U95")
  for(i in min:length(y)){
    temp<-NULL
    if(type=="ARIMA"){
      temp=fc(y=y[1:i],Year=Year[i+1],xreg=xreg,h=h, ...)
    }
    if(type=="gam"){
      temp=fc2(y=y[1:i],Year=Year[i+1],xreg=xreg,h=h, ...)
    }
    forecast[i+1,] = temp$results
  }
  forecast<-forecast%>%
    remove_rownames()
  
  results<-list()
  results$results<-forecast
  results$fit<-temp$fit
  return(results)
}