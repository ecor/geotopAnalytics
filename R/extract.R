NULL
#' Extract variable from an object returend by \code{\link{GEOtop_ReadValidationData}}
#' 
#' @param x objectect returned by \code{\link{GEOtop_ReadValidationData}} with \code{merge==TRUE}
#' @param InputVar discover variable
#' @param Add_InputVar additional variable (simulated) 
#' 
#' @export 
#' @examples 
#' 
#' wpath <- '/home/ecor/activity/2016/eurac2016/idra/B2_BeG_017_DVM_001_test_1' 

#' 
#' ex <- GEOtop_ReadValidationData(wpath = wpath)
#'  
#' eout <- extractGeotopVar(ex)
#' 
#' 

extractGeotopVar <- function(x,InputVar=NULL,Add_InputVar=NULL)  {
	
	obsnames <- attr(x,"observation_var")
	simnames <- attr(x,"simulation_var")
	
	nnames <- intersect(obsnames,simnames)
	
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
	names(out) <- c( "additional var", "observation", "simulation")
	
	nn <- c(add_name,name)[1:3]

	names(nn) <- names(out)
	onn <- nn
	nn <- nn[(nn %in% names(x))]
	out[,names(nn)] <- x[,nn]
	attr(out,"var_name") <- onn 
	
	
	
	return(out)
	
	
	
	
} 