

rm(list=ls())



library(zoo)
library(geotopbricks)

#write.geotop.table <- function(x,file,wpath=NULL,tz = "Etc/GMT-1",date_field="Date12.DDMMYYYYhhmm.",col_sep=",",
#		file_end = "",sep=",",format="%d/%m/%Y %H:%M", na = "-9999",...) {
#	
#	time <- as.POSIXct(index(x))
#	time <- as.POSIXlt(time,tz=tz)
#	x <- as.data.frame(x)
#	nn <- names(x)
#	x[,date_field] <- as.character(time,format=format)
#	x <- x[,c(date_field,nn)]
#	
#	if (!is.null(wpath)) file <- paste(wpath,file,sep="/")
#	file <- paste(file,file_end,sep="")
#	print(file)
#	print("DA INSERIRE IN GEOTOPOPTIM")
#	write.table(x=x,file=file,row.names=FALSE,sep=sep,quote=FALSE,na=na,...)
#	
#	## TO DO ON MONDAY!!!!
#	
#	
#}


wpath <- '/home/ecor/activity/2016/eurac2016/Incarico_EURAC/results/B2_BeG_017_DVM_001_test_1' 
load(file.path(wpath, "obs", "observation.RData"))

obs <- observation$hour

write.geotop.table(x=obs,file="obs.csv",wpath=wpath)
