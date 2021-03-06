NULL
#' Extract variable from an object returend by \code{\link{GEOtop_ReadValidationData}}
#' 
#' @param x objectect returned by \code{\link{GEOtop_ReadValidationData}} with \code{merge==TRUE}
#' @param InputVar discover variable
#' @param Add_InputVar additional variable (simulated) 
#' @param aggregate aggregate option. Default it is \code{c("hourly","daily","monthly","yearly")} and it is considered the first element. 
#' @param aggregate_fun aggregate function for \code{InputVar} and \code{Add_InputVar} respectively. Deafault is \code{c("mean","sum")}.
#' @param ... further arguments
#' 
#' @export 
#' @note It is assumed that \code{x} already contains hourly aggregated time series, so in case \code{aggregare=="hourly"} no aggragetion is calculated.
#' @examples 
#' 
#' wpath <- '/home/ecor/activity/2016/eurac2016/idra/B2_BeG_017_DVM_001_test_1' 
#' 
#' 
#' ex <- GEOtop_ReadValidationData(wpath = wpath)
#'  
#' eout <- extractGeotopVar(ex)
#' dout <- extractGeotopVar(ex,aggregate="daily")
#' mout <-  extractGeotopVar(ex,aggregate="monthly")
#' 
#' 
#' 


extractGeotopVar <- function(x,InputVar=NULL,Add_InputVar=NULL,aggregate=c("hourly","daily","monthly","yearly"),aggregate_fun=c("mean","sum"),...)  {
	
	obsnames <- attr(x,"observation_var")
	simnames <- attr(x,"simulation_var")
	
	nnames <- intersect(obsnames,simnames)
	
	aggregate <- aggregate[1]
	if (is.null(InputVar)) {
		
		InputVar <- nnames[1]
		
	}
	
	if (is.null(Add_InputVar)) {
		
		Add_InputVar <- InputVar
		
	}
	
	name <- paste(c("OBS","SIM"),InputVar,sep="_")
	
	add_name <- paste("SIM",Add_InputVar,sep='_')
	
	
	
	
	
	out <- x[,1:3]
	out[,] <- NA
	names(out) <- c( "additional.var", "observation", "simulation")
	
	nn <- c(add_name,name)[1:3]

	names(nn) <- names(out)
	onn <- nn
	nn <- nn[(nn %in% names(x))]
	out[,names(nn)] <- x[,nn]
	attr(out,"var_name") <- onn 
	
	if (aggregate!="hourly") {
		
		## 1  fun_aggregeta [2]
		## 2 and 3 fun_aggregate [3]
		
		
		tz <- sprintf("Etc/%s",format(index(out[1]),format="%Z"))
		
		if (aggregate=="daily") aggregate <- "%Y-%m-%d"
		if (aggregate=="monthly") aggregate <- "%Y-%m"
		if (aggregate=="yearly") aggregate <- "%Y"
		
		byc <- format(index(out),format=aggregate)
		
		by <- as.POSIXlt(byc,tz=tz,format=aggregate)
		
		out <- as.data.frame(out)
		lout <- list()
		cfun <- c(2,1,1)
		for (c in 1:ncol(out)) {
			
			lout[[c]] <- tapply(X=out[,c],INDEX=byc,FUN=get(aggregate_fun[cfun[c]]),na.rm=TRUE)
			
		}
		names(lout) <- names(out)
		
		lout <- as.data.frame(lout)
		
		lout <- as.zoo(lout)
		###str(lout)
		if (aggregate=="%Y-%m") rownames(lout) <- paste(rownames(lout),15,sep="-")
		if (aggregate=="%Y") rownames(lout) <- paste(rownames(lout),5,15,sep="-")
		index(lout) <- as.POSIXct(rownames(lout),tz=tz,format="%Y-%m-%d")
		
		out <- lout
		
		
		
	}
	
	return(out)
	
	
	
	
} 