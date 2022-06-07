## runs Rmarkdown for several rivers and creates final forecast table
pkgs<-c("rmarkdown","here","dplyr","readr")
install.packages(setdiff(pkgs,rownames(installed.packages())), dependencies=TRUE)
invisible(lapply(pkgs,library,character.only=TRUE))
## names of all rivers to be included
rs<-c("Chehalis","Hoh","Humptulips","Queets","Quillayute","Quinault","Willapa")
## run Rmarkdown for each river
setwd(here("code"))
for(i in 1:length(rs)){
  render("Coastal_steelhead_forecasting.Rmd",
    params=list(system=rs[i]),
    output_file=paste0("../output/Forecast_",rs[i],"_steelhead.html")
    )
}
## combine all next year forecasts in one table
setwd(here("output"))
tab_name<-"Table_of_forecasts.csv"
if(file.exists(tab_name)) do.call(file.remove,list(tab_name))
files<-list.files(pattern="\\.csv$")
rivers<-gsub("Forecast-","",(gsub(".csv","",files)))
tab<-files %>% 
  lapply(read_csv) %>% 
  bind_rows %>%
  mutate(River=rivers) %>%
  mutate(Estimate=round(Estimate),L95=round(L95),U95=round(U95)) %>%
  dplyr::select(all_of(c("River","Year","Estimate","L95","U95")))
## remove river files and save table with forecasts for all rivers
do.call(file.remove,list(files))
write.csv(tab,"Table_of_forecasts.csv")
## end of code