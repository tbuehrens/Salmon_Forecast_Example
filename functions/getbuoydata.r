#get NOAA SST Buoy Data
getbuoydata<-function(buoylist,years){
  df<-data.frame(matrix(NA,ncol = 4))
  colnames(df)<-c("buoyid","year","month","meanSST")
  for(i in 1:length(bouylist)){
    for(j in 1:length(years)){
      try(
        df<-df%>%
          bind_rows(buoy(dataset = "stdmet",buoyid = buoylist[i],year=years[j])$data%>%
                      dplyr::select(sea_surface_temperature,time)%>%
                      mutate(buoyid=buoylist[i]
                             ,year = format(as.Date(time),"%Y")
                             ,month = format(as.Date(time),"%m")
                      )%>%
                      group_by(buoyid,year,month)%>%
                      summarise(meanSST = mean(sea_surface_temperature))
          )
        ,silent = T
      )
    }
  }
  df<-df%>%filter(!is.na(buoyid))
  return(df)
}
