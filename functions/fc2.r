#fc2 fit model for gam 
fc2 <- function(y, Years, h, xreg, knots, m){
  if(!is.null(xreg)){
    if(ncol(xreg)>1){
      X <- xreg[1:length(y),]
    }else(X<-xreg[1:length(y)])
    if(NROW(xreg) < length(y) + h)
      stop("Not enough xreg data for forecasting")
    if(ncol(xreg)>1){
      if(h==1){
        newX <- t(as.matrix(xreg[length(y)+(1:h),]))
      }else(newX<-as.matrix(xreg[length(y)+(1:h),]))
    }else(newX<-xreg[length(y)+(1:h)])
    data<-data.frame(y,X,c(1:length(y)))
    colnames(data)<-c("y",colnames(xreg),"year")
    formula=as.formula(paste0("exp(y) ~ s(year, m=",m,", bs='ps',","k =",knots,") +",paste(colnames(xreg),collapse = "+")))
    fit<-gam(formula=formula,data=data,family="nb",link=log)
    newdat<-data.frame(newX,c(length(y)+1))
    colnames(newdat)<-c(colnames(xreg),"year")
  }else{
    data<-data.frame(y,c(1:length(y)))
    colnames(data)<-c("y","year")
    #fit<-gam(exp(y)~s(year,m=1,k=round(length(y)-2)),data=data,family="nb",link=log)
    fit<-gam(exp(y)~s(year,m=1,k=knots),data=data,family="nb",link=log)
    
    #newdat<-data.frame(length(y)+1)
    newdat<-data.frame(c((length(y)+1):(length(y)+h))) # for multiple years
    colnames(newdat)<-c("year")
  }
  # results<-data.frame(predict(fit,newdata=newdat,type="link",se.fit=T)) %>%
  #   mutate(L95 = fit - 1.96 * se.fit, U95 = fit + 1.96 * se.fit)%>%
  #   rename(Estimate=fit,SE=se.fit)%>%
  #   bind_cols(data.frame(Year=Years))%>%
  #   dplyr::select(Year,Estimate,SE,L95,U95)

  results<-data.frame(predict(fit,newdat,type="link",se.fit=T)) %>%
    cbind(log(data.frame(gam_pred_ints(fit,newdat,c(0.025,0.975))))) %>%
    data.frame() %>%
    rename(Estimate=fit,SE=se.fit,L95=X0.025,U95=X0.975) %>%
    bind_cols(data.frame(Year=Years))%>%
    dplyr::select(Year,Estimate,SE,L95,U95)

  results<-list(results=results,fit=fit)
  return(results)
}
