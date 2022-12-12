#function to evaluate forecast skill ()
evaluate_forecasts2<-function(forecasts,observations){
  forecast_skill<-forecasts%>%
    # left_join(observations,by=c("Year","runsize_obs"))%>%
    dplyr::select(Year,Model,runsize_obs,predicted_runsize_obs)%>%
    mutate(error=predicted_runsize_obs-runsize_obs)%>%
    filter(!is.na(error))%>%
    group_by(Model)%>%
    summarise(MAPE = mean(abs(error/runsize_obs))*100,
              RMSE = sqrt(mean(error^2)),
              
              MSA = 100*(exp(mean(abs(log(runsize_obs/predicted_runsize_obs))))-1)
    )%>%
    arrange(MAPE)
  return(forecast_skill)
}
