#fc2 fit model for gam 
fc2 <- function(y, Year, h, xreg, order){
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
    dat<-data.frame(y,X,c(1:length(y)))
    colnames(dat)<-c("y",colnames(xreg),"year")
    formula = as.formula(paste0("exp(y) ~ s(year, m=2, bs='ps',","k =",length(y)-floor(dim(xreg)[2]-4),") +",paste(colnames(xreg),collapse = "+")))
    fit<-gam(formula=formula,data=dat,family="nb",link=log)
    newdat<-data.frame(newX,c(length(y)+1))
    colnames(newdat)<-c(colnames(xreg),"year")
  }else{
    dat<-data.frame(y,c(1:length(y)))
    colnames(dat)<-c("y","year")
    fit<-gam(exp(y) ~ s(year, m=1, k=round(length(y)-2)),data=dat,family="nb",link=log)
    newdat<-data.frame((length(y)+1))
    colnames(newdat)<-c("year")
  }
  results<-data.frame(predict(fit, newdata = newdat,type="link",se.fit=T))%>%
    mutate(L95 = fit - 1.96 * se.fit, U95 = fit + 1.96 * se.fit)%>%
    rename(Estimate=fit,SE=se.fit)%>%
    bind_cols(data.frame(Year=Year))%>%
    dplyr::select(Year,Estimate,SE,L95,U95)
  
  results<-list(results=results,fit=fit)
  return(results)
}
