pkgs<-c("tidyverse","rmarkdown","here")
install.packages(setdiff(pkgs,rownames(installed.packages())),dependencies=TRUE)
invisible(lapply(pkgs,library,character.only=TRUE))

rs<-c("Chehalis","Hoh","Humptulips","Queets","Quillayute","Quinault","Willapa")
nrs<-length(rs)

for(i in 1:nrs){
  render("code/Coastal_steelhead_forecasting.Rmd",
         params=list(system=rs[i]),
         output_file=paste0("../output/Forecast_",rs[i],"_steelhead.html"))
}