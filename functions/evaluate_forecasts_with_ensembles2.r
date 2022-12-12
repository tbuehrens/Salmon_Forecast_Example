evaluate_forecasts_with_ensembles2<-function(forecasts,dat,y1=11){
  
  yrrange<-forecasts%>%
    summarise(minyr=min(Year),maxyr=max(Year))%>%
    unlist()
  
  ensembles<-NULL
  for(i in (thisYr-y1):2022){
    years<-c(yrrange[1]:i)
    tdat<-forecasts%>%
      filter(Year %in% years)%>%
      left_join(dat,by="Year")%>%
      dplyr::select(Year,model,predicted_runsize_obs,runsize_obs=runsize_obs.x)%>%
      mutate(error=runsize_obs-predicted_runsize_obs)%>%
      filter(!is.na(error))%>%
      group_by(model)%>%
      summarise(RMSE = sqrt(mean(error^2)),
                MAPE = mean(abs(error/runsize_obs))*100,
                MSA = 100*(exp(mean(abs(log(runsize_obs/predicted_runsize_obs))))-1)
      )%>%
      arrange(MSA)%>%
      mutate(MSA_weight=(1/MSA)^k/sum((1/MSA)^k), 
             RMSE_weight =(1/RMSE)^k/sum((1/RMSE)^k),
             MAPE_weight =(1/MAPE)^k/sum((1/MAPE)^k)
      )
    
    modelcnt<-length(unique(forecasts$model))
    stackyears<-years[years!=max(years)]
    stackdat<-forecasts%>%
      filter(Year %in% years)%>%
      pivot_wider(names_from = model, values_from = predicted_runsize_obs,id_cols = Year)%>%
      left_join(dat%>%dplyr::select(Year,runsize_obs))
    
    stack_weights<-find_stack_weights(tau=1,
                                      n=10000,
                                      metric=stack_metric,
                                      initial_weights=rep(1/modelcnt,modelcnt),
                                      preds=stackdat%>%
                                        filter(!is.na(runsize_obs))%>%
                                        dplyr::select(!runsize_obs & !Year)%>%
                                        as.matrix(),
                                      obs=stackdat%>%
                                        filter(!is.na(runsize_obs))%>%
                                        dplyr::select(runsize_obs & !Year)%>%
                                        as.matrix()
    )
    stacking_weights<-data.frame("Stacking_weight" = as.vector(round(unlist(stack_weights[[1]]),4)))
    stacking_weights$Model<-colnames(stackdat)[!colnames(stackdat)%in%c("Year","runsize_obs")]
    tdat<-tdat%>%
      left_join(stacking_weights %>% rename(model=Model))
    
    tdat2<-forecasts%>%
      filter(Year == max(years)+1)%>%
      pivot_longer(names_to = "Parameter",
                   cols=c("predicted_runsize_obs","Lo 95","Hi 95"),
                   values_to = "value")%>%
      left_join(tdat)%>%
      mutate(MSA_weighted = value * MSA_weight,
             RMSE_weighted = value * RMSE_weight,
             MAPE_weighted = value * MAPE_weight,
             Stack_weighted = value * Stacking_weight,
      )%>%
      group_by(Year,Parameter)%>%
      summarise(MSA_weighted = sum(MSA_weighted),
                RMSE_weighted = sum(RMSE_weighted),
                MAPE_weighted = sum(MAPE_weighted),
                Stack_weighted = sum(Stack_weighted),
      )%>%
      pivot_longer(names_to = "Model",
                   cols=c("MSA_weighted","RMSE_weighted","MAPE_weighted","Stack_weighted"),
                   values_to = "value")%>%
      pivot_wider(id_cols = c("Year","Model"),names_from=Parameter,values_from=value)
    
    ensembles<-bind_rows(ensembles,tdat2)
  }
  forecast_skill<-evaluate_forecasts2(forecasts = bind_rows(forecasts %>% rename(Model=model),ensembles %>% left_join(dat))%>%
                                       filter(Year>(thisYr-y1)),
                                     observations = dat
  )
  
  forecasts2<- bind_rows(forecasts %>% rename(Model=model),ensembles %>% left_join(dat))%>%
    filter(Year>(thisYr-y1))
  
  results<-list(
    final_model_weights = tdat,
    forecast_skill = forecast_skill,
    ensembles = ensembles,
    forecasts=forecasts2
  )
  return(results)
}
