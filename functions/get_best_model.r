get_best_model<-function(forecasts,ensembles,stack_metric,forecast_skill){
   
  best_model<-forecasts%>%
     bind_rows(ensembles)%>%
     mutate(sd=abs(log(U95)-log(Estimate))/1.96)%>%
     left_join(forecast_skill)%>%
     filter(Model %in% unique(ensembles$Model))%>%
     filter(get(stack_metric)==min(get(stack_metric)))

   
}