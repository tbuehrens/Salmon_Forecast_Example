fit_pboost<-function(dat,TY,yr_ind){
  ## data frame with correct date format
  data<-dplyr::select(dat,Year,runsize_obs)%>%
    mutate(runsize_obs=log(as.numeric(runsize_obs)))%>%
    set_names(c("date","value"))%>% 
    mutate(date=as.Date(paste(date,1,1,sep="-")))
  ## index of years to forecast
  index<-(TY):(yr_ind-1)
  nY<-length(index)
  ## trim future years for splitting and forecasting
  dat_fit<-data[-yr_ind,]
  splits<-dat_fit%>%time_series_split(initial=TY,assess=nY-1)
  ## create a pre-processing recipe using recipe()
  recipe_spec<-recipe(value~date,training(splits)) %>%
    step_timeseries_signature(date) %>%
    step_dummy(all_nominal())
  recipe_spec %>% prep() %>% juice()
  ## model setup
  model_spec_prophet_boost<-prophet_boost(seasonality_yearly=TRUE)%>%
    set_engine("prophet_xgboost")
  ## model fit to training data
  workflow_fit_prophet_boost<-workflow() %>%
    add_model(model_spec_prophet_boost) %>%
    add_recipe(recipe_spec) %>%
    fit(training(splits))
  ## model calibration on test data
  calibration_tbl<-modeltime_table(workflow_fit_prophet_boost)%>%
    modeltime_calibrate(testing(splits))
  ## model forecasting of test data
  forecast_tbl<-calibration_tbl%>%
    modeltime_forecast(new_data=testing(splits))%>% 
    data.frame()
  ## re-fit to all data and predict future years
  prediction_tbl<-calibration_tbl%>%
    modeltime_refit(dat_fit)%>%
    modeltime_forecast(new_data=data[yr_ind,])%>%
    data.frame()
  ## all forecasts including predictions
  results_tbl<-bind_rows(list(forecast_tbl,prediction_tbl))%>%
    mutate(Year=as.numeric(substr(.index,1,4)))%>%
    dplyr::select("Year",".value",".conf_lo",".conf_hi")%>%
    mutate(across(everything(),round,5))
  ## re-name and supply all years to output
  colnames(results_tbl)<-c("Year","Estimate","L95","U95")
  results<-data.frame(Year=dat$Year)%>%left_join(results_tbl)
  results<-list(results=results,fit=model_fit_prophet)
  return(results)
}