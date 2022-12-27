#function to evaluate performance of ARIMA model 
inseason_forecast_v2<-function(series,
                            leave_yrs=20,
                            covariates,
                           
                            plot_results,
                            write_model_summaries,
                            # forecast_period_start_m, #inclusive
                            # forecast_period_start_d, #inclusive
                            # obs_period_2,
                            # p1_covariates_only,
                            stack_metric,
                            seasonal=FALSE
                            ){
  if(write_model_summaries ==T){
    write.table(NULL,"summary.txt")
  }
  # browser()
  series<-series%>%
    ungroup()%>%
    dplyr::select(Year,runsize_obs,all_of(unique(unlist(covariates))))%>%
    filter(
      across(
        .cols = all_of(unique(unlist(covariates))),
        .fns = ~ !is.na(.x)
      )
    )
  
  for(c in 1:length(covariates)){
    for(i in 1:(leave_yrs+1)){
      last_train_yr = max(series$Year) - (leave_yrs-i+2)
      tdat<-series%>%
        filter(Year <= (last_train_yr+1))%>%
        mutate(train_test = ifelse(Year > last_train_yr, 1, 0),
        )
      
      exists1<-tdat%>%filter(Year==(last_train_yr+1) )%>%ungroup%>%dplyr::select(runsize_obs)%>%nrow()
      if(exists1==0){
        adddat<-tdat%>%tail(1)
        # adddat$period<-1
        adddat$runsize_obs<-NA
        adddat$train_test<-1
        tdat<-tdat%>%
          bind_rows(adddat)
      }
      exists2<-tdat%>%filter(Year==(last_train_yr+1))%>%ungroup%>%dplyr::select(runsize_obs)%>%nrow()
      if(exists2==0){
        adddat<-tdat%>%tail(1)
        # adddat$period<-2
        adddat$runsize_obs<-NA
        adddat$train_test<-1
        tdat<-tdat%>%
          bind_rows(adddat)
      }
      

      
      xreg<-tdat%>%
        filter(train_test==0)%>%
        ungroup%>%
        dplyr::select(all_of(covariates[[c]]))%>%
        as.matrix()
      
      xreg_pred<-tdat%>%
        filter(train_test==1)%>%
        ungroup%>%
        dplyr::select(all_of(covariates[[c]]))%>%
        as.matrix()
  
      if(ncol(xreg)>0){
        m1<-tdat%>%
          filter(train_test==0)%>%
          ungroup()%>%
          dplyr::select(runsize_obs)%>%
          unlist()%>%
          ts()%>%
          auto.arima(lambda=0,seasonal = seasonal, xreg = xreg)
        
        pred<-c(m1$fitted,forecast::forecast(m1,lambda=0,h=1,
                                             xreg = xreg_pred
        )$mean)
        CI<-forecast::forecast(m1,lambda=0,h=1, level = c(50, 95),
                               xreg = xreg_pred
        )%>%
          as_tibble()%>%
          dplyr::select(!`Point Forecast`)%>%
          mutate(Year = last_train_yr+1)
      }else{
        m1<-tdat%>%
          filter(train_test==0)%>%
          ungroup()%>%
          dplyr::select(runsize_obs)%>%
          unlist()%>%
          ts()%>%
          auto.arima(lambda=0,seasonal = seasonal, xreg = NULL)
        
        pred<-c(m1$fitted,forecast::forecast(m1,lambda=0,h=1,
                                             xreg = NULL
        )$mean)
        CI<-forecast::forecast(m1,lambda=0,h=1, level = c(50, 95),
                               xreg = NULL
        )%>%
          as_tibble()%>%
          dplyr::select(!`Point Forecast`)%>%
          mutate(Year = last_train_yr+1)
      }
      
      # if(write_model_summaries ==T){
      #   sink("summary.txt",append=T)
      #   print(summary(m1))
      #   sink()
      # }
      # 
      tdat<-tdat%>%
        bind_cols(pred=pred)%>%
        left_join(CI, by = c("Year"))%>%
        dplyr::rename(predicted_runsize_obs = pred)%>%
        filter(train_test==1)
      
      if(i==1){forecasts = tdat
      }else{forecasts = forecasts %>% bind_rows(tdat)}
    }
    

      forecasts<-forecasts%>%
        mutate(obs_runsize_obs = 0)
    
      
    forecasts<-forecasts%>%
      mutate(error = predicted_runsize_obs-runsize_obs,
             pct_error=scales::percent(error/runsize_obs),
             model = as.character(c)
      )
    if(c==1){
      tdat2 <- forecasts
    }else{
      tdat2 <- tdat2%>%
        bind_rows(forecasts)
    }
  }
  forecasts<-tdat2
  
  #do model averaging and stacking and calculate performance metrics
  forcast_eval<-evaluate_forecasts_with_ensembles2(forecasts,dat)
  
  return(forcast_eval)
}

