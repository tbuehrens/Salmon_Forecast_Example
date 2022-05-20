#one step ahead cross validation
tsCV2<-function(y,Year,xreg,type,min,h, ...){
  forecast<-data.frame(matrix(NA,ncol=5,nrow=1))
  colnames(forecast)<-c("Year","Estimate","SE","L95","U95")
  for(i in min:length(y)){
    forecast_yrs<-c((i+1):(i+h))
    temp<-NULL
    if(type=="ARIMA"){
      #temp=fc(y=y[1:i],Years=Year[i+1],xreg=xreg,h=h,...)
      temp=fc(y=y[1:i],Years=Year[forecast_yrs],xreg=xreg,h=h,...)
    }
    if(type=="ARIMA_auto"){
      #temp=fc4(y=y[1:i],Years=Year[i+1],xreg=xreg,h=h,...)
      temp=fc4(y=y[1:i],Years=Year[forecast_yrs],xreg=xreg,h=h,...)
    }
    if(type=="gam"){
      #temp=fc2(y=y[1:i],Years=Year[i+1],xreg=xreg,h=h,...)
      temp=fc2(y=y[1:i],Years=Year[forecast_yrs],xreg=xreg,h=h,...)
    }
    # forecast[i+1,]<-temp$results
    forecast[forecast_yrs,]<-temp$results
  } # end i-loop
  forecast<-forecast%>%remove_rownames()
  results<-list()
  results$results<-forecast
  results$fit<-temp$fit
  return(results)
}