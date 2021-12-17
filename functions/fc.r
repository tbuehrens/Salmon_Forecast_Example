#fit model function for ARIMA models
fc <- function(y,Year, h, xreg, order){
  if(!is.null(xreg)){
    if(ncol(xreg)>1){
      X <- xreg[1:length(y),]
    }else(X<-xreg[1:length(y)])
    if(NROW(xreg) < length(y) + h)
      stop("Not enough xreg data for forecasting")
    if(ncol(xreg)>1){
      if(h==1){
        newX <- t(as.matrix(xreg[length(y)+(1:h),]))
      }else(newX <- as.matrix(xreg[length(y)+(1:h),]))
    }else(newX<-xreg[length(y)+(1:h)])
    fit <-Arima(y,order=order, xreg=X,method="ML")
    
    #results<-forecast(fit, xreg=newX,h=h)$mean[[1]]
    results<-data.frame(forecast(fit, xreg=newX,h=h))%>%
      rename(Estimate = Point.Forecast, L95 = Lo.95, U95 =Hi.95)%>%
      mutate(SE=abs((U95-Estimate)/1.96))%>%
      dplyr::select(Estimate,SE,L95,U95)
  }else{
    fit <- Arima(y,order=order,method="ML")
    #results<-forecast(fit,h=h)$mean[[1]]
    results<-data.frame(forecast(fit,h=h))%>%
      rename(Estimate = Point.Forecast, L95 = Lo.95, U95 = Hi.95)%>%
      mutate(SE=abs((U95-Estimate)/1.96))%>%
      dplyr::select(Estimate,SE,L95,U95)
  }
  results<-results%>%
    bind_cols(data.frame(Year=Year))%>%
    dplyr::select(Year,Estimate,SE,L95,U95)
  
  results<-list(results=results,fit=fit)
  return(results)
}
