#function to evaluate forecast skill ()
evaluate_forecasts<-function(forecasts, observations){
  forecast_skill<-forecasts%>%
    left_join(observations,by="Year")%>%
    dplyr::select(Year,Model,Estimate,runsize_obs)%>%
    mutate(error=runsize_obs-Estimate)%>%
    filter(!is.na(error))%>%
    group_by(Model)%>%
    summarise(RMSE = sqrt(mean(error^2)),
              MAPE = mean(abs(error/runsize_obs))*100,
              MSA = 100*(exp(mean(abs(log(runsize_obs/Estimate))))-1)
    )%>%
    arrange(MSA)
  return(forecast_skill)
}
