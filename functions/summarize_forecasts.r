#creates summary of forecasts by year by combining model$results from tsCV2 or fc3 
summarize_forecasts<-function(models){
  f<-list(lapply(models,function(x) get(x)$results))
  names(f[[1]])<-models
  forecasts<-f[[1]]%>%
    map_dfr(~ .x %>% as_tibble(), .id = "Model")%>%
    filter(!is.na(Estimate))%>%
    mutate(Estimate=exp(Estimate),L95=exp(L95),U95=exp(U95))%>%
    dplyr::select(!SE)
  return(forecasts)
}
