#one step ahead cross validation
fit_models<-function(data,TY,...){
  #==========
  #ARIMA models
  #==========
  ARIMA100<-tsCV2(y=log(data[-dim(data)[1],2]),
                  Year=data$Year,
                  xreg=NULL,
                  h=FY,
                  min=TY,
                  type="ARIMA",
                  order=c(1,0,0)
  )
  ARIMA100_withcov<-tsCV2(y=log(data[-dim(data)[1],2]),
                          Year=data$Year,
                          xreg=data%>%dplyr::select("NPGO")%>%as.matrix(),
                          h=FY,
                          min=TY,
                          type="ARIMA",
                          order=c(1,0,0)
  )
  
  ARIMA111_withcov<-tsCV2(y=log(data[-dim(data)[1],2]),
                          Year=data$Year,
                          xreg=data%>%dplyr::select("NPGO")%>%as.matrix(),
                          h=FY,
                          min=TY,
                          type="ARIMA",
                          order=c(1,1,1)
  )
  ARIMA010_withcov<-tsCV2(y=log(data[-dim(data)[1],2]),
                          Year=data$Year,
                          xreg=data%>%dplyr::select("NPGO")%>%as.matrix(),
                          h=FY,
                          min=TY,
                          type="ARIMA",
                          order=c(0,1,0)
  )
  ARIMA010<-tsCV2(y=log(data[-dim(data)[1],2]),
                  Year=data$Year,
                  xreg=NULL,
                  h=FY,
                  min=TY,
                  type="ARIMA",
                  order=c(0,1,0)
  )
  # ARIMA111<-tsCV2(y=log(dat$runsize_obs[-dim(dat)[1]]),
  #             Year=dat$Year,
  #             xreg=NULL,
  #             h=FY,
  #             min=TY,
  #             type="ARIMA",
  #             order=c(1,1,1)
  # )
  ARIMA101_withcov<-tsCV2(y=log(data[-dim(data)[1],2]),
                          Year=data$Year,
                          xreg=data%>%dplyr::select("NPGO")%>%as.matrix(),
                          h=FY,
                          min=TY,
                          type="ARIMA",
                          order=c(1,0,1)
  )
  ARIMA101<-tsCV2(y=log(data[-dim(data)[1],2]),
                  Year=data$Year,
                  xreg=NULL,
                  h=FY,
                  min=TY,
                  type="ARIMA",
                  order=c(1,0,1)
  )
  ARIMA001_withcov<-tsCV2(y=log(data[-dim(data)[1],2]),
                          Year=data$Year,
                          xreg=data%>%dplyr::select("NPGO")%>%as.matrix(),
                          h=FY,
                          min=TY,
                          type="ARIMA",
                          order=c(0,0,1)
  )
  ARIMA001<-tsCV2(y=log(data[-dim(data)[1],2]),
                  Year=data$Year,
                  xreg=NULL,
                  h=FY,
                  min=TY,
                  type="ARIMA",
                  order=c(0,0,1)
  )
  #==========
  #GAM models
  #==========
  gam<-tsCV2(y=log(data[-dim(data)[1],2]),
             Year=data$Year,
             xreg=NULL,
             h=FY,
             min=TY,
             type="gam",
             m=1,
             knots=dim(dat)[1]-1
  )
  # LFO10<-tsCV2(y=log(dat$runsize_obs[-dim(dat)[1]]),
  #             Year=dat$Year,
  #             xreg=dat%>%dplyr::select(all_of(vars))%>%as.matrix(),
  #             h=FY,
  #             min=TY,
  #             type="gam"
  #             m=1,
  #             knots=dim(dat)[1]-1
  # )
  #================================
  #BRMS models with Horseshoe Prior
  #================================
  # gp_horseshoe<-fc3(
  #   Year=dat$Year,
  #   TY=TY
  #   ,vars=vars
  #   ,dat=dat
  #   ,formula = as.formula(paste0("log(runsize_obs) ~ gp(Year) + ",paste(vars,collapse = "+")))
  #   ,prior = set_prior(horseshoe(df = 1, par_ratio = 0.5), class="b")
  # )
  # LFO12<-fc3(
  #   Year=dat$Year,
  #   TY=TY
  #   ,vars=vars
  #   ,dat=dat
  #   ,formula = as.formula(paste0("log(runsize_obs) ~ arma(p = 1, q = 1, cov=TRUE) + ",paste(vars,collapse = "+")))
  #   ,prior = set_prior(horseshoe(df = 1, par_ratio = 0.5), class="b")
  # )
  
   models <- list(ARIMA100,ARIMA100_withcov,ARIMA111_withcov,ARIMA010_withcov,ARIMA010,
         ARIMA101_withcov,ARIMA101,ARIMA001_withcov,ARIMA001,gam)
  
  return(models)
  
}