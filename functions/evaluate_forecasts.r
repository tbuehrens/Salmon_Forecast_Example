#function to evaluate forecast skill ()
evaluate_forecasts<-function(forecasts,observations,stack_metric){
  forecast_skill<-forecasts%>%
    left_join(observations,by="Year")%>%
    dplyr::select(Year,Model,Estimate,runsize_obs)%>%
    mutate(error=runsize_obs-Estimate)%>%
    filter(!is.na(error))%>%
    group_by(Model)%>%
    summarise(RMSE = sqrt(mean(error^2)),
              MAPE = mean(abs(error/runsize_obs))*100,
              MSA = 100*(exp(median(abs(log(runsize_obs/Estimate))))-1)
    )%>%
    arrange(get(stack_metric))
  return(forecast_skill)
}
