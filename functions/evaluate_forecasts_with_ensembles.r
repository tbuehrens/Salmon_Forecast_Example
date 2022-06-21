evaluate_forecasts_with_ensembles<-function(forecasts,observations){
  yrrange<-forecasts%>%
    summarise(minyr=min(Year),maxyr=max(Year))%>%
    unlist()
  
  ensembles<-NULL
  for(i in 2: length(yrrange[1]:yrrange[2])){
    Years<-c(yrrange[1]:(yrrange[1]+i-1))
    tdat<-forecasts%>%
      filter(Year %in% Years)%>%
      left_join(observations,by="Year")%>%
      dplyr::select(Year,Model,Estimate,runsize_obs)%>%
      mutate(error=runsize_obs-Estimate)%>%
      filter(!is.na(error))%>%
      group_by(Model)%>%
      summarise(RMSE = sqrt(mean(error^2)),
                MAPE = mean(abs(error/runsize_obs)*100,na.rm = TRUE),
                MSA = 100*(exp(median(abs(log(Estimate/runsize_obs)),na.rm = T))-1))%>%
      arrange(MSA)%>%
      mutate(MSA_weight=(1/MSA)^k/sum((1/MSA)^k), 
             RMSE_weight =(1/RMSE)^k/sum((1/RMSE)^k),
             MAPE_weight =(1/MAPE)^k/sum((1/MAPE)^k)
      )
    
    modelcnt<-length(unique(forecasts$Model))
    stackYears<-Years[Years!=max(Years)]
    stackdat<-forecasts%>%
      filter(Year %in% Years)%>%
      pivot_wider(names_from = Model, values_from = Estimate,id_cols = Year)%>%
      left_join(observations%>%dplyr::select(Year,runsize_obs))
    
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
      left_join(stacking_weights)
    
    tdat2<-forecasts%>%
      filter(Year == max(Years))%>%
      pivot_longer(names_to = "Parameter",
                   cols=c("Estimate","L95","U95"),
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
  forecast_skill<-evaluate_forecasts(forecasts = bind_rows(forecasts,ensembles)%>%
                                       filter(Year>min(yrrange)),
                                     observations = observations
                                     )
  results<-list(
    final_model_weights = tdat,
    forecast_skill = forecast_skill,
    ensembles = ensembles
  )
  return(results)
}