## runs Rmarkdown for several rivers and creates final forecast table
pkgs<-c("rmarkdown","here","dplyr","readr")
install.packages(setdiff(pkgs,rownames(installed.packages())), dependencies=TRUE)
invisible(lapply(pkgs,library,character.only=TRUE))
## names of all rivers to be included
rs<-c("Chehalis","Hoh","Humptulips","Queets","Quillayute","Quinault","Willapa")
nr<-length(rs)
## run Rmarkdown for each river
setwd(here("code"))
for(i in 1:nr){
  render("Coastal_steelhead_forecasting.Rmd",
    params=list(system=rs[i]),
    output_file=paste0("../output/Forecast_",rs[i],"_steelhead.html")
    )
}

## combine next year forecasts for all rivers in one table
setwd(here("output"))
tab_name<-"Table_of_forecasts.csv"
if(file.exists(tab_name)) do.call(file.remove,list(tab_name))
files<-list.files(pattern="\\.csv$")
files<-files[which(grepl("forecast",files))]
rivers<-gsub("_forecast","",(gsub(".csv","",files)))
tab<-files %>% 
  lapply(read_csv) %>% 
  bind_rows %>%
  mutate(River=rivers) %>%
  mutate(Estimate=round(Estimate),L95=round(L95),U95=round(U95)) %>%
  dplyr::select(all_of(c("River","Year","Estimate","L95","U95")))
## remove river files and save table with forecasts for all rivers
do.call(file.remove,list(files))
write.csv(tab,"Table_of_forecasts.csv")

## combine all one-year-ahead forecasts for all rivers in one figure
pdf("report-forecasts-figure.pdf",width=10,height=5)
par(mar=c(2.5,2.5,0.5,0.5),oma=c(1,1,1,1),mgp=c(1.5,0.25,0),tck=-0.02,cex.axis=0.8,cex.lab=1.2,xaxs="i",yaxs="i")
layout(matrix(1:8,ncol=4,nrow=2,byrow=T))
xlim<-c(1978,2023)
ylim<-c(0,23000)
col<-"firebrick"
for(i in 1:nr){
  system<-rs[i]
  forecast<-read.csv(paste0("Best-model-",system,".csv"))[,-1]
  dat<-read.csv(paste0("Data-observed-",system,".csv"))[,-1]
  xlim<-c(min(dat$Year)-1,max(dat$Year)+1)
  ylim<-c(0,1.2*max(c(dat$runsize_obs,forecast$U95),na.rm=TRUE))
  plot(NA,NA,xlim=xlim,ylim=ylim,xlab="Year",ylab="Run size")
  polyx<-c(forecast$Year,rev(forecast$Year))
  polyy<-c(forecast$L95,rev(forecast$U95))
  polygon(polyx,polyy,lwd=0.5,col=alpha(col,0.5),border=NA)
  lines(forecast$Year,forecast$Estimate,lwd=2,col=alpha(col,1))
  points(dat$Year,dat$runsize_obs,pch=16,cex=1,col="black")
  mtext(paste0(" ",system," "),side=3,line=-1.2,cex=0.75,adj=1,font=2)
}
## legend 
plot(NA,NA,axes=F,xaxt="n",yaxt="n",xlim=xlim,ylim=ylim,xlab="",ylab="")
l<-c("Historical run size","1-year-ahead forecasts\n(median and 95% CI)")
legend("topleft",legend=l,pch=NA,col=c(NA,alpha(col,0.5)),lwd=c(NA,8),cex=1,seg.len=1.5, bty="n",inset=c(0.05,0),text.col="white")
legend("topleft",legend=l,pch=c(16,NA),col=c("black",col),lwd=c(NA,2),cex=1,seg.len=1.5, bty="n",inset=c(0.05,0))
dev.off()
