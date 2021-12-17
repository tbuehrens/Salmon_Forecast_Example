get_flow_data<-function(flow_site,min_year,max_year){
  ## get URL for flow data from USGS
  flow_url <- paste0("https://waterdata.usgs.gov/nwis/dv",
                     "?cb_00060=on",
                     "&format=rdb",
                     "&site_no=",flow_site,
                     "&begin_date=",min(dat$Year),"-01-01",
                     "&end_date=",max(dat$Year),"-12-31")
  flow<-read_table(flow_url,skip=29)%>%
    dplyr::rename(Date=`20d`,CFS=`14n`)%>%
    dplyr::select(Date,CFS)
}